if (getRversion() < "2.15.0")
    stop("only valid for R >= 2.15")


if (sys.nframe())
    stop("must be run in a top level context")


args <- commandArgs(trailingOnly = TRUE)
main.dir <- args[[1L]]; args <- args[-1L]


if (!exists("startsWith", mode = "function")) {
    startsWith <- function(x, prefix) {
        pattern <- gsub("([.\\\\|()[{^$*+?])", "\\\\\\1", prefix)
        pattern <- paste0("^", pattern)
        grepl(pattern, x)
    }
    environment(startsWith) <- .BaseNamespaceEnv
    attributes(startsWith) <- NULL
}


build.binary <- function (pkg)
{
    src.dir <- file.path("src", "contrib")
    info <- read.dcf(
        file.path(main.dir, src.dir, "PACKAGES"),
        fields = c("Package", "Version")
    )
    i <- match(pkg, info[, "Package"])
    if (is.na(i)) {
        warning(sprintf("package '%s' does not exist in '/%s/PACKAGES'", pkg, src.dir))
        return(FALSE)
    }
    pkgname <- info[[i, "Package"]]
    version <- info[[i, "Version"]]
    tar.file <- paste0(pkgname, "_", version, ".tar.gz")
    tar.path <- file.path(main.dir, src.dir, tar.file)
    if (!file.exists(tar.path)) {
        warning(sprintf("tarball '/%s/%s' was not found", src.dir, tar.file))
        return(invisible())
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
    bin.file <- paste0(pkgname, "_", version, ext)
    bin.dir <- file.path("bin", platform, "contrib", R.version = sub("^([[:digit:]]+\\.[[:digit:]]+)\\.[[:digit:]]+$", "\\1", getRversion()))
    bin.path <- file.path(main.dir, bin.dir)
    dir.create(bin.path, showWarnings = FALSE, recursive = TRUE)


    exdir <- tempfile("dir")
    utils::untar(tar.path, DESCRIPTION.file <- file.path(pkgname, "DESCRIPTION"), exdir = exdir)
    packageInfo <- read.dcf(file.path(exdir, DESCRIPTION.file))
    unlink(exdir, recursive = TRUE, force = TRUE)
    if (nrow(packageInfo) != 1L) {
        warning("bruh wtf are you doing???")
        return(FALSE)
    }
    packageInfo <- structure(c(packageInfo), names = colnames(packageInfo))


    fields <- c("Package", "Version", "Depends", "Suggests",
                "License", "Imports", "LinkingTo", "Enhances", "OS_type")
    PACKAGES.info <- structure(packageInfo[fields], names = fields)
    PACKAGES.info <- t(PACKAGES.info)


    owd <- getwd()
    if (is.null(owd)) {
        warning("cannot 'chdir' as current directory is unknown")
        return(FALSE)
    }
    on.exit(setwd(owd), add = TRUE)
    setwd(bin.path)


    tar.path <- c(rep("..", length(gregexpr("/", bin.dir)[[1L]]) + 1L), src.dir, tar.file)
    tar.path <- paste(tar.path, collapse = "/")


    failure <- TRUE


    files <- list.files(getwd(), full.names = TRUE)
    files <- files[startsWith(basename(files), paste0(pkgname, "_"))]
    if (length(files) <= 0L) {
    } else if (length(files) == 1L) {
        backup <- tempfile("back_up_", tmpdir = dirname(files), fileext = ext)
        if (!file.rename(files, backup)) {
            warning("unable to rename old tarball as a backup")
            return(FALSE)
        }
        on.exit(if (failure) file.rename(backup, files) else file.remove(backup), add = TRUE)
    } else {
        warning("invalid 'files'")
        return(FALSE)
    }


    command <- if (.Platform$OS.type == "windows") {
        shQuote(file.path(R.home("bin"), "Rcmd.exe"))
    } else {
        c(shQuote(file.path(R.home("bin"), "R")), "CMD")
    }
    command <- paste(command, "INSTALL", "--build", shQuote(tar.path))
    cat("\n", command, "\n", sep = "")
    res <- system(command)
    cat("\n")
    if (res) {
        if (res == -1L)
            warning(sprintf("'%s' could not be run", command))
        else warning(sprintf("'%s' execution failed with error code %d", command, res))
        return(FALSE)
    }
    PACKAGES.file <- file.path(bin.path, "PACKAGES")
    if (file.exists(PACKAGES.file)) {
        text <- readLines(PACKAGES.file)
        on.exit(if (failure) writeLines(text, PACKAGES.file), add = TRUE)
        con <- file(PACKAGES.file, "w")
        tryCatch({
            if (i <- match(paste0("Package: ", pkgname), text, 0L)) {
                writeLines(text[seq_len(i - 1L)], con)
            } else if (i <- match(TRUE, startsWith(text, "Package: ") & substr(text, 10L, 1000000L) > pkgname, 0L)) {
                writeLines(text[seq_len(i - 1L)], con)
            } else {
                i <- length(text)
                writeLines(c(text, ""), con)
            }
            write.dcf(PACKAGES.info, con)
            j <- which(text == "")
            j <- j[j > i]
            if (length(j) > 0) {
                j <- j[[1L]]
                writeLines(text[j:length(text)], con)
            }
            failure <- FALSE
        }, error = function(c) {
            close(con)
            con <- NULL
        }, finally = if (!is.null(con)) close(con))
    } else {
        on.exit(if (failure) file.remove(PACKAGES.file), add = TRUE)
        write.dcf(PACKAGES.info, PACKAGES.file)
        failure <- FALSE
    }
    return(!failure)
}


build.binaries <- function (pkgs)
{
    vapply(pkgs, build.binary, NA)
}


main <- function ()
{
    pkgs <- args
    if (length(pkgs) <= 0L) {
        if (interactive())
            pkgs <- strsplit(readline("Packages to build binaries: "), "[[:blank:]]+|[[:blank:]]*[,;][[:blank:]]*")[[1L]]
        else stop("must provide arguments or be in interactive mode")
    }
    if ("--all" %in% pkgs)
        pkgs <- setdiff(
            read.dcf(
                file.path(main.dir, "src", "contrib", "PACKAGES"),
                fields = "Package"
            ),
            pkgs
        )
    build.binaries(pkgs)
}


main()
