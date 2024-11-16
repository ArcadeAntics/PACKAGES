if (FALSE) {


.src_fields <- c(
    "Package", "Version", "Priority", "Depends",
    "Imports", "LinkingTo", "Suggests", "Enhances",
    "License", "License_is_FOSS", "License_restricts_use",
    "OS_type", "Archs", "MD5sum", "NeedsCompilation",
    "Path"
)


.bin_fields <- c(
    "Package", "Version", "Priority", "Depends",
    "Imports", "LinkingTo", "Suggests", "Enhances",
    "License", "License_is_FOSS", "License_restricts_use",
    "OS_type", "Archs"
)


.read_DESCRIPTION_from_dir <- function (dir, ...)
{
    dir <- path.expand(dir)


    desc <- read.dcf(file.path(dir, "DESCRIPTION"), ...)
    if (nrow(desc) != 1L)
        stop("contains a blank line", call. = FALSE)
    structure(c(desc), names = colnames(desc))
}


.read_DESCRIPTION_from_tarball <- function (tarpath, ...)
{
    tarpath <- path.expand(tarpath)


    files <- utils::untar(tarpath, list = TRUE)
    files <- grep(
        # sprintf("^%s/DESCRIPTION$", .standard_regexps()$valid_package_name),
        # "^[[:alpha:]][[:alnum:].]*[[:alnum:]]/DESCRIPTION$",
        "^[ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz][0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz.]*[0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz]/DESCRIPTION$",
        files,
        value = TRUE,
        useBytes = TRUE
    )
    if (length(files) != 1L)
        stop(gettextf("invalid '%s'", "tarpath"))
    exdir <- tempfile("exdir", tempdir(TRUE))
    tryCatch({
        utils::untar(tarpath, files, exdir = exdir)
        desc <- read.dcf(file = file.path(exdir, files), ...)
    }, finally = {
        unlink(exdir, recursive = TRUE, force = TRUE)
    })
    if (nrow(desc) != 1L)
        stop("contains a blank line", call. = FALSE)
    structure(c(desc), names = colnames(desc))
}


.read_DESCRIPTION_from_zip_archive <- function (zippath, ...)
{
    zippath <- path.expand(zippath)


    files <- utils::unzip(zippath, list = TRUE)$Name
    files <- grep(
        # sprintf("^%s/DESCRIPTION$", .standard_regexps()$valid_package_name),
        # "^[[:alpha:]][[:alnum:].]*[[:alnum:]]/DESCRIPTION$",
        "^[ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz][0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz.]*[0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz]/DESCRIPTION$",
        files,
        value = TRUE,
        useBytes = TRUE
    )
    if (length(files) != 1L)
        stop(gettextf("invalid '%s'", "zippath"))
    conn <- unz(zippath, files)
    tryCatch({
        desc <- read.dcf(file = conn, ...)
    }, finally = {
        close(conn)
    })
    if (nrow(desc) != 1L)
        stop("contains a blank line", call. = FALSE)
    structure(c(desc), names = colnames(desc))
}


.read_DESCRIPTION <- function (path)
{
    path <- path.expand(path)
    if (dir.exists(path))
        .read_DESCRIPTION_from_dir(path)
    else if (endsWith(path, ".tar.gz"))
        .read_DESCRIPTION_from_tarball(path)
    else if (endsWith(path, ".zip"))
        .read_DESCRIPTION_from_zip_archive(path)
    else if (endsWith(path, ".tgz"))
        .read_DESCRIPTION_from_tarball(path)
    else stop(gettextf("invalid '%s' value", "path", domain = "R"), domain = NA)
}


.write_PACKAGES <- function (desc, dir)
{
    if (!is.character(desc))
        stop("a character vector argument expected", domain = "R")
    if (!length(desc) || is.null(fields <- names(desc)))
        stop(gettextf("invalid '%s' argument", "desc", domain = "R"), domain = NA)


    dir <- path.expand(dir)


    PACKAGES_path <- file.path(dir, "PACKAGES")
    if (file.exists(PACKAGES_path)) {
        all_desc <- read.dcf(PACKAGES_path, fields)
    } else {
        all_desc <- matrix(
            character(0),
            nrow = 0L,
            ncol = length(fields),
            dimnames = list(NULL, fields)
        )
    }
    if (i <- match(desc[["Package"]], all_desc[, "Package"], 0L)) {
        all_desc[i, ] <- desc
    } else {
        all_desc <- rbind(all_desc, desc)
    }
    all_desc <- all_desc[order(all_desc[, "Package"]), , drop = FALSE]
    tmpfile <- tempfile("PACKAGES")
    on.exit(unlink(tmpfile), add = TRUE)
    write.dcf(all_desc, tmpfile, indent = 8L, width = 72L)
    if (!file.copy(tmpfile, PACKAGES_path, overwrite = TRUE, copy.date = TRUE))
        stop(sprintf("unable to copy file '%s' to '%s'", tmpfile, PACKAGES_path))


    PACKAGES_path
}


.find_R_CMD <- function (dir)
{
    if (.Platform$OS.type == "windows") {
        if (file.exists(command <- file.path(dir, "Rcmd.exe")))
            shQuote(command)
        else if (file.exists(command <- file.path(dir, "x64/Rcmd.exe")))
            shQuote(command)
        else if (file.exists(command <- file.path(dir, "i386/Rcmd.exe")))
            shQuote(command)
        else c(shQuote(file.path(dir, "R.exe")), "CMD")
    }
    else c(shQuote(file.path(dir, "R")), "CMD")
}


.system <- function (command, intern = FALSE, ..., dry.run = FALSE, mustWork = NA,
    quiet = intern)
{
    if (dry.run)
        return(command)
    if (!quiet)
        cat("$ ", command, "\n", sep = "")
    value <- system(command = command, intern = intern, ...)
    if (intern) {
        if (!is.null(status <- attr(value, "status")) && status) {
            if (isFALSE(mustWork)) {
                if (!quiet)
                    cat("\nProcess finished with exit code ", status, "\n", sep = "")
            }
            else if (isTRUE(mustWork)) {
                if (status == -1L)
                    stop(gettextf("'%s' could not be run", command, domain = "R-base"), domain = NA)
                else stop(gettextf("'%s' execution failed with error code %d", command, status, domain = "R-base"), domain = NA)
            }
            else if (status == -1L)
                warning(gettextf("'%s' could not be run", command, domain = "R-base"), domain = NA)
            else warning(gettextf("'%s' execution failed with error code %d", command, status, domain = "R-base"), domain = NA)
        }
        value
    }
    else {
        if (!value || isFALSE(mustWork)) {
            if (!quiet)
                cat("\nProcess finished with exit code ", value, "\n", sep = "")
        }
        else if (isTRUE(mustWork)) {
            if (value == -1L)
                stop(gettextf("'%s' could not be run", command, domain = "R-base"), domain = NA)
            else stop(gettextf("'%s' execution failed with error code %d", command, value, domain = "R-base"), domain = NA)
        }
        else if (value == -1L)
            warning(gettextf("'%s' could not be run", command, domain = "R-base"), domain = NA)
        else warning(gettextf("'%s' execution failed with error code %d", command, value, domain = "R-base"), domain = NA)
        invisible(value)
    }
}


make_R <- function (bin = NULL, version = NULL)
{
    R_version_pattern <- "^(([[:digit:]]+)\\.([[:digit:]]+))\\.[[:digit:]]+$"
    if (is.null(bin)) {
        bin <- R.home("bin")
        version <- getRversion()
        major_minor <- sub(R_version_pattern, "\\1", version)
    }
    else if (is.null(version)) {
        args <- c(
            shQuote(file.path(
                bin,
                if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"
            )),
            "--default-packages=NULL",
            "--vanilla",
            "-e", shQuote("writeLines(format(getRversion()))")
        )
        command <- paste(args, collapse = " ")
        rval <- .system(command, intern = TRUE, mustWork = TRUE)
        version <- if (is.character(rval) && length(rval) == 1L && !is.na(rval) &&
            grepl(R_version_pattern, rval))
        {
            rval
        }
        else NA_character_
        version <- R_system_version(version)
        major_minor <- sub(R_version_pattern, "\\1", version)
    }
    else {
        version <- package_version(version)
        major_minor <- sub("^(([[:digit:]]+)\\.([[:digit:]]+)).*$", "\\1", version)
    }
    structure(
        list(bin = bin, version = version, major_minor = major_minor),
        class = "R"
    )
}


build_tarball <- function (pkgpath, R = NULL)
{
    pkgpath <- path.expand(pkgpath)


    if (is.null(R))
        R <- make_R()
    if (!inherits(R, "R"))
        stop(gettextf("invalid '%s' value", "R", domain = "R"), domain = NA)
    r_bin <- R$bin
    r_version <- R$version


    desc <- .read_DESCRIPTION_from_dir(pkgpath, c("Package", "Version"))
    pkgname <- desc[["Package"]]
    version <- desc[["Version"]]
    if (!grepl(pkgname, pattern = paste0("^(", .standard_regexps()$valid_package_name, ")$")))
        stop("invalid package DESCRIPTION file")
    if (!grepl(version, pattern = paste0("^(", .standard_regexps()$valid_package_version, ")$")))
        stop("invalid package DESCRIPTION file")


    tarpath <- paste0(pkgname, "_", version, ".tar.gz")
    args <- c(.find_R_CMD(r_bin), "build", shQuote(pkgpath))
    command <- paste(args, collapse = " ")
    .system(command, mustWork = TRUE)
    tarpath
}


copy_tarball_to_repos <- function (tarpath, repos_dir, Path = NULL)
{
    tarpath <- path.expand(tarpath)
    repos_dir <- path.expand(repos_dir)
    if (is.null(Path)) {
        Path <- NA_character_
    } else if (is.character(Path) && length(Path) == 1L) {
        if (is.na(Path))
            Path <- NA_character_
        else if (nzchar(Path)) {
            Path <- gsub("\\", "/", Path, fixed = TRUE)
            Path <- gsub("^/+|/+$", "", Path)
            if (!nzchar(Path))
                Path <- NA_character_
        }
        else Path <- NA_character_
    } else if (is.logical(Path) && length(Path) == 1L && is.na(Path)) {
        Path <- NA_character_
    } else {
        stop(gettextf("invalid '%s' argument", "Path", domain = "R"))
    }


    desc <- .read_DESCRIPTION_from_tarball(tarpath, .src_fields)
    desc["MD5sum"] <- tools::md5sum(tarpath)
    desc["Path"] <- Path


    src_contrib_dir <- file.path(repos_dir, "src", "contrib")
    dir.create(src_contrib_dir, showWarnings = FALSE, recursive = TRUE)
    .write_PACKAGES(desc, src_contrib_dir)


    new_tarname <- paste0(desc[["Package"]], "_", desc[["Version"]], ".tar.gz")
    if (!is.na(desc[["Path"]])) {
        dir.create(
            file.path(src_contrib_dir, desc[["Path"]]),
            showWarnings = FALSE,
            recursive = TRUE
        )
        new_tarname <- file.path(desc[["Path"]], new_tarname)
    }
    new_tarpath <- file.path(src_contrib_dir, new_tarname)
    if (!file.copy(
        tarpath,
        new_tarpath,
        overwrite = TRUE,
        copy.date = TRUE
    )) {
        stop("failure to rename")
    }


    files <- list.files(
        src_contrib_dir,
        paste0(
            "^",
            gsub(".", "\\.", desc[["Package"]], fixed = TRUE),
            "_",
            .standard_regexps()$valid_package_version,
            "\\.tar\\.gz$"
        ),
        recursive = TRUE
    )
    files <- files[!startsWith(files, "Archive/")]
    files <- files[files != new_tarname]
    if (length(files)) {
        src_contrib_Archive_dir <- file.path(src_contrib_dir, "Archive", desc[["Package"]])
        dir.create(src_contrib_Archive_dir, showWarnings = FALSE, recursive = TRUE)
        if (!all(file.rename(
            file.path(src_contrib_dir, files),
            file.path(src_contrib_Archive_dir, files)
        ))) {
            stop("failure to rename")
        }
    }


    new_tarpath
}


build_tarball_in_repos <- function (pkgpath, repos_dir, Path = NULL, R = NULL)
{
    tarpath <- build_tarball(pkgpath, R)
    copy_tarball_to_repos(tarpath, repos_dir, Path)
}


find_tarball_in_repos <- function (pkgname, repos_dir)
{
    repos_dir <- path.expand(repos_dir)


    src_contrib_dir <- file.path(repos_dir, "src", "contrib")
    info <- read.dcf(file.path(src_contrib_dir, "PACKAGES"), .src_fields)
    i <- match(pkgname, info[, "Package"])
    if (is.na(i)) {
        warning(sprintf("package '%s' does not exist in 'src/contrib/PACKAGES'", pkg))
        return(FALSE)
    }
    pkgname <- info[[i, "Package"]]
    version <- info[[i, "Version"]]
    tarname <- paste0(pkgname, "_", version, ".tar.gz")
    if (!is.na(info[[i, "Path"]]))
        tarname <- file.path(info[[i, "Path"]], tarname)
    tarpath <- file.path(src_contrib_dir, tarname)
    if (!file.exists(tarpath)) {
        warning(sprintf("tarball 'src/contrib/%s' was not found", tarname))
        return(FALSE)
    }


    tarpath
}


build_binary <- function (tarpath, R = NULL)
{
    tarpath <- path.expand(tarpath)


    if (is.null(R))
        R <- make_R()
    if (!inherits(R, "R"))
        stop(gettextf("invalid '%s' value", "R", domain = "R"), domain = NA)
    r_bin <- R$bin
    r_version <- R$version
    r_major_minor <- R$major_minor


    if (.Platform$OS.type == "windows") {
        ext <- ".zip"
        platform <- "windows"
    } else if (grepl("^darwin", R.version$os)) {
        ext <- ".tgz"
        platform <- "macosx"
        if (startsWith(.Platform$pkgType, "mac.binary."))
            platform <- paste(platform, substring(.Platform$pkgType, 12L), sep = "/")
    } else stop("binary packages are not available")
    desc <- .read_DESCRIPTION_from_tarball(tarpath)
    binpath <- paste0(desc[["Package"]], "_", desc[["Version"]], ext)
    bin_dir <- file.path("bin", platform, "contrib", r_major_minor)


    args <- c(.find_R_CMD(r_bin), "INSTALL", "--build", shQuote(tarpath))
    command <- paste(args, collapse = " ")
    # unloadNamespace("essentials"); unloadNamespace("this.path"); stop("remove this later")
    .system(command, mustWork = TRUE)


    list(binpath = binpath, bin_dir = bin_dir)
}


build_binary_from_repos <- function (pkgname, repos_dir, R = NULL)
{
    tarpath <- find_tarball_in_repos(pkgname, repos_dir)
    build_binary(tarpath, R)
}


copy_binary_to_repos <- function (binpath, repos_dir, bin_dir)
{
    binpath <- path.expand(binpath)
    repos_dir <- path.expand(repos_dir)
    bin_dir <- file.path(repos_dir, bin_dir)


    if (endsWith(binpath, ".zip")) {
        ext <- ".zip"
        desc <- .read_DESCRIPTION_from_zip_archive(binpath, .bin_fields)
    } else if (endsWith(binpath, ".tgz")) {
        ext <- ".tgz"
        desc <- .read_DESCRIPTION_from_tarball(binpath, .bin_fields)
    } else {
        warning("invalid")
        return(FALSE)
    }


    dir.create(bin_dir, showWarnings = FALSE, recursive = TRUE)


    .write_PACKAGES(desc, bin_dir)


    binname <- paste0(
        desc[["Package"]],
        "_",
        desc[["Version"]],
        ext
    )
    to <- file.path(bin_dir, binname)
    if (!file.copy(
        binpath,
        to,
        overwrite = TRUE,
        copy.date = TRUE
    )) {
        stop(sprintf(
            "unable to rename file '%s' to '%s'",
            binpath,
            to
        ))
    }


    files <- list.files(
        bin_dir,
        paste0(
            "^",
            gsub(".", "\\.", desc[["Package"]], fixed = TRUE),
            "_",
            .standard_regexps()$valid_package_version,
            gsub(".", "\\.", ext, fixed = TRUE),
            "$"
        )
    )
    files <- files[files != basename(to)]
    if (length(files))
        file.remove(file.path(bin_dir, files))


    to
}


build_binary_in_repos <- function (pkgname, repos_dir, R = NULL)
{
    x <- build_binary_from_repos(pkgname, repos_dir, R)
    copy_binary_to_repos(x$binpath, repos_dir, x$bin_dir)
}


make_repos <- function (repos_dir)
{
    repos_dir <- path.expand(repos_dir)
    x <- list(
        repos_dir = repos_dir,
        copy_tarball = function (tarpath, Path = NULL)
copy_tarball_to_repos(tarpath, repos_dir, Path),
        build_tarball = function (pkgpath, Path = NULL, R = NULL)
build_tarball_in_repos(pkgpath, repos_dir, Path, R),
        find_tarball = function (pkgname)
find_tarball_in_repos(pkgname, repos_dir),
        copy_binary = function (binpath, bin_dir)
copy_binary_to_repos(binpath, repos_dir, bin_dir),
        build_binary = function (pkgname, R = NULL)
build_binary_in_repos(pkgname, repos_dir, R = NULL)
    )
    class(x) <- "repos"
    x
}


repos <- make_repos("~/test")
unlink(repos$repos_dir, recursive = TRUE, force = TRUE)
dir.create(repos$repos_dir)


repos$copy_tarball("~/this.path/this.path_2.5.0.77.tar.gz", "4.5.0/Recommended")


repos$find_tarball("this.path")


dir.create(file.path(repos$repos_dir, "bin", "windows", "contrib", "4.4"), showWarnings = FALSE, recursive = TRUE)
file.create(file.path(repos$repos_dir, "bin", "windows", "contrib", "4.4", "this.path_2.5.0.76.zip"))


unloadNamespace("essentials"); unloadNamespace("this.path")
repos$build_binary("this.path")


repos$copy_binary(
    "~/PACKAGES/bin/macosx/big-sur-arm64/contrib/4.3/this.path_2.4.0.1.tgz",
    "bin/macosx/big-sur-arm64/contrib/4.3"
)


dir(repos$repos_dir, all.files = TRUE, recursive = TRUE, include.dirs = TRUE)
write.dcf(read.dcf(file.path(repos$repos_dir, "src/contrib/PACKAGES"                         ), .src_fields))
write.dcf(read.dcf(file.path(repos$repos_dir, "bin/windows/contrib/4.4/PACKAGES"             ), .bin_fields))
write.dcf(read.dcf(file.path(repos$repos_dir, "bin/macosx/big-sur-arm64/contrib/4.3/PACKAGES"), .bin_fields))


}


main <- function (args = this.path::progArgs())
{
    # args <- "this.path"; stop("remove this later")
    if (length(args) <= 0L) {
        if (interactive())
            args <- strsplit(readline("Packages to build binaries: "), "[[:blank:]]+")[[1L]]
        else stop("must provide arguments or be in interactive mode")
        if (length(args) <= 0L)
            stop("expected at least 1 argument")
    }


    ## load all necessary namespaces
    loadNamespace("utils")
    loadNamespace("this.path")


    R <- data.frame(bin = local({
        x <- Sys.getenv(c("r_release", "r_oldrel"), NA)
        if (any(i <- is.na(x))) {
            warning(sprintf(
                ngettext(sum(i), "environment variable %s is not defined",
                                 "environment variables %s are not defined"),
                paste(encodeString(names(x)[i], quote = "\""), collapse = ", ")))
            x <- x[!i]
        }
        if (any(i <- !dir.exists(x))) {
            warning(sprintf(
                ngettext(sum(i), "environment variable %s is not an existing directory",
                                 "environment variables %s are not existing directories"),
                paste(encodeString(names(x)[i], quote = "\""), collapse = ", ")))
            x <- x[!i]
        }
        y <- Sys.getenv()
        m <- regexec("(?i)^r_([[:digit:]]+)_([[:digit:]]+)$", names(y))
        keep <- lengths(m) == 3L
        y <- y[keep]
        m <- m[keep]
        keep <- !(y %in% x)
        y <- y[keep]
        m <- m[keep]
        keep <- dir.exists(y)
        y <- y[keep]
        m <- m[keep]
        z <- regmatches(names(y), m)
        z <- vapply(z, function(zz) as.integer(zz[-1L]), integer(2), USE.NAMES = FALSE)
        y <- y[order(z[1L, ], z[2L, ], decreasing = TRUE)]
        x <- c(x, y)
        if (length(x)) x else R.home("bin")
    }))


    R_version_pattern <- "^(([[:digit:]]+)\\.([[:digit:]]+))\\.[[:digit:]]+$"
    R$version <- vapply(R$bin, function(xx) {
        apt <- if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"
        apt <- this.path::path.join(xx, apt)
        args <- c(apt, "--default-packages=NULL", "--vanilla", "-e", "writeLines(format(getRversion()))")
        command <- paste(shQuote(args), collapse = " ")
        rval <- suppressWarnings(system(command, intern = TRUE))
        if (!is.null(status <- attr(rval, "status")) && status) {
            if (status == -1L)
                warning(gettextf("'%s' could not be run",
                    command, domain = "R-base"), domain = NA)
            else
                warning(gettextf("'%s' execution failed with error code %d",
                    command, status, domain = "R-base"), domain = NA)
            return(NA_character_)
        }
        if (is.character(rval) && length(rval) == 1L && !is.na(rval) &&
            grepl(R_version_pattern, rval))
        {
            rval
        }
        else NA_character_
    }, "")
    R$major_minor <- sub(R_version_pattern, "\\1", R$version)
    R <- do.call("rbind", lapply(
        split(R, factor(R$major_minor, unique(R$major_minor))),
        function(r) {
            n <- nrow(r)
            if (n == 1L)
               r
            else
                r[order(r$version)[n], , drop = FALSE]
        }
    ))


    owd <- getwd()
    if (is.null(owd))
        warning("cannot 'chdir' as current directory is unknown")


    main_dir <- this.path::here(.. = 1)
    tmp_dir <- this.path::path.join(main_dir, "tmp")
    unlink(tmp_dir, recursive = TRUE, force = TRUE, expand = FALSE)
    dir.create(tmp_dir, showWarnings = FALSE)
    on.exit({
        ## must change back to original directory before attempting to unlink
        ## temporary directory
        setwd(owd)
        unlink(tmp_dir, recursive = TRUE, force = TRUE, expand = FALSE)
    }, add = TRUE, after = FALSE)
    setwd(tmp_dir)
    main_dir <- ".."


    build_binary <- function(pkg, r) {
        # dir.create(tmp_dir <- this.path::here(.. = 1, "tmp"), showWarnings = FALSE); setwd(tmp_dir); main_dir <- ".."; pkg <- "this.path"; r <- data.frame(bin = Sys.getenv("r_oldrel"), version = "4.2.3", major_minor = "4.2"); stop("remove this later")
        contrib_dir <- file.path("src", "contrib")
        info <- read.dcf(
            file.path(main_dir, contrib_dir, "PACKAGES"),
            fields = c("Package", "Version")
        )
        i <- match(pkg, info[, "Package"])
        if (is.na(i)) {
            warning(sprintf("package '%s' does not exist in '/%s/PACKAGES'", pkg, contrib_dir))
            return(FALSE)
        }
        pkgname <- info[[i, "Package"]]
        version <- info[[i, "Version"]]
        tar_file <- paste0(pkgname, "_", version, ".tar.gz")
        tar_path <- file.path(main_dir, contrib_dir, tar_file)
        if (!file.exists(tar_path)) {
            warning(sprintf("tarball '/%s/%s' was not found", contrib_dir, tar_file))
            return(FALSE)
        }


        if (.Platform$OS.type == "windows") {
            ext <- ".zip"
            platform <- "windows"
        } else if (grepl("^darwin", R.version$os)) {
            ext <- ".tgz"
            platform <- "macosx"
            if (startsWith(.Platform$pkgType, "mac.binary."))
                platform <- paste(platform, substring(.Platform$pkgType, 12L), sep = "/")
        } else {
            warning("binary packages are not available")
            return(FALSE)
        }
        bin_file <- paste0(pkgname, "_", version, ext)
        bin_dir <- file.path("bin", platform, "contrib", r$major_minor)
        bin_path <- file.path(main_dir, bin_dir)
        dir.create(bin_path, showWarnings = FALSE, recursive = TRUE)


        exdir <- tempfile("dir")
        utils::untar(tar_path, DESCRIPTION_file <- file.path(pkgname, "DESCRIPTION"), exdir = exdir)
        desc <- read.dcf(file.path(exdir, DESCRIPTION_file))
        unlink(exdir, recursive = TRUE, force = TRUE)
        if (nrow(desc) != 1L) {
            warning("bruh wtf are you doing???")
            return(FALSE)
        }
        desc <- structure(c(desc), names = colnames(desc))


        fields <- c("Package", "Version", "Depends", "Suggests",
            "License", "Imports", "LinkingTo", "Enhances", "OS_type")
        desc <- structure(desc[fields], names = fields)
        desc <- t(desc)


        failure <- TRUE


        files <- list.files(file.path(main_dir, bin_dir), full.names = TRUE)
        files <- files[startsWith(basename(files), paste0(pkgname, "_"))]
        files <- files[endsWith(basename(files), ext)]
        files <- files[basename(files) != bin_file]
        if (length(files))
            on.exit(if (!failure) file.remove(files), add = TRUE, after = FALSE)


        command <- if (.Platform$OS.type == "windows") {
            shQuote(file.path(r$bin, "Rcmd.exe"))
        } else {
            paste(shQuote(file.path(r$bin, "R")), "CMD")
        }
        command <- paste(command, "INSTALL", "--build", shQuote(tar_path))
        cat("\n", command, "\n", sep = "")
        # unloadNamespace("essentials"); unloadNamespace("this.path"); stop("remove this later")
        res <- system(command)
        cat("\n")
        if (res) {
            if (res == -1L)
                warning(gettextf("'%s' could not be run",
                    command, domain = "R-base"), domain = NA)
            else
                warning(gettextf("'%s' execution failed with error code %d",
                    command, res, domain = "R-base"), domain = NA)
            return(FALSE)
        }
        PACKAGES_path <- file.path(bin_path, "PACKAGES")
        if (file.exists(PACKAGES_path)) {
            text <- readLines(PACKAGES_path)
            conn <- file("./PACKAGES", "w")
            tryCatch({
                matchThis <- paste0("Package: ", pkgname)
                if (i <- match(matchThis, text, 0L)) {
                    writeLines(text[seq_len(i - 1L)], conn)
                } else if (i <- match(TRUE, startsWith(text, "Package: ") & text > matchThis, 0L)) {
                    writeLines(text[seq_len(i - 1L)], conn)
                    i <- i - 2L
                } else {
                    i <- length(text)
                    writeLines(c(text, ""), conn)
                }
                write.dcf(desc, conn, indent = 8L, width = 72L)
                j <- which(text == "")
                j <- j[j > i]
                if (length(j) > 0) {
                    j <- j[[1L]]
                    writeLines(text[j:length(text)], conn)
                }
            }, finally = close(conn))
        } else {
            write.dcf(desc, "./PACKAGES", indent = 8L, width = 72L)
        }


        rename_these <- c("PACKAGES", bin_file)
        failure <- !all(file.rename(
            file.path(".", rename_these),
            file.path(bin_path, rename_these)
        ))
        return(!failure)
    }


    build_binaries <- function(pkgs, r) {
        vapply(pkgs, build_binary, r, FUN.VALUE = NA)
    }


    all_pkgs <- read.dcf(
        file.path(main_dir, "src", "contrib", "PACKAGES"),
        fields = "Package"
    )


    args_sep <- "/"
    argslist <- vector("list", sum(args == args_sep) + 1L)
    indx <- 0L
    while (i <- match(args_sep, args, 0L)) {
        argslist[[indx <- indx + 1L]] <- args[seq_len(i - 1L)]
        args <- args[-seq_len(i)]
    }
    argslist[[indx + 1L]] <- args
    argslist <- argslist[lengths(argslist) >= 1L]
    argslist <- lapply(argslist, function(args) {
        m <- regexec("^--version=(.*)$", args)
        keep <- (lengths(m) > 1L)
        pkgs <- args[!keep]
        if ("--all" %in% pkgs)
            pkgs <- setdiff(all_pkgs, pkgs)
        args <- args[keep]
        m <- m[keep]
        args <- regmatches(args, m)
        args <- vapply(args, `[`, 2L, FUN.VALUE = "")
        args <- strsplit(args, "[[:blank:]]+|[[:blank:]]*[,;][[:blank:]]*")
        args <- unlist(args)
        args <- if (!length(args) || "all" %in% args)
            seq_len(nrow(R))
        else which(R$major_minor %in% args)
        list(pkgs = pkgs, R_indx = args)
    })


    unloadNamespace("this.path")
    e <- Sys.getenv(c("R_LIBS", "R_LIBS_USER", "R_LIBS_SITE"), NA)
    if (any(unset <- is.na(e)))
        on.exit(Sys.unsetenv(names(which(unset))), add = TRUE, after = FALSE)
    if (any(set <- !unset))
        on.exit(do.call(Sys.setenv, as.list(e[set])), add = TRUE, after = FALSE)
    Sys.unsetenv(names(e))


    for (args in argslist) {
        for (i in args$R_indx) {
            r <- R[i, , drop = TRUE]
            cat("\n", "Building binaries for R ", r$version, "\n", sep = "")
            print(tryCatch(build_binaries(args$pkgs, r), error = identity))
            cat("\n")
        }
    }
}


if (this.path::is.main()) main()
