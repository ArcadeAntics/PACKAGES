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


    svn_revision <- tryCatch({
        x <- bin
        while (dirname(x) != x && basename(x) != "bin") {
            x <- dirname(x)
        }
        if (dirname(x) == x || basename(x) != "bin")
            stop()
        x <- dirname(x)
        conn <- file(file.path(x, "include", "Rversion.h"), "r", encoding = "native.enc")
        on.exit(close(conn))
        x <- readLines(conn, warn = FALSE, encoding = "bytes")
        pattern <- "^[[:blank:]]*#[[:blank:]]*define[[:blank:]]+R_SVN_REVISION[[:blank:]]+(?:([[:digit:]]+)|\"([[:digit:]]+)\")[[:blank:]]*$"
        m <- regexec(pattern, x)
        keep <- which(lengths(m) == 3L)
        x <- regmatches(x[keep], m[keep])[[1L]]
        as.integer(if (nzchar(x[[2L]])) x[[2L]] else x[[3L]])
    }, error = function(e) {
        NA_integer_
    })


    structure(
        list(bin = bin, version = version, major_minor = major_minor, svn_revision = svn_revision),
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
