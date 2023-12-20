main <- function (args = this.path::progArgs())
{
    # args <- "this.path"; stop("remove out later")
    if (length(args) <= 0L) {
        if (interactive())
            args <- strsplit(readline("Packages to build binaries: "), "[[:blank:]]+|[[:blank:]]*[,;][[:blank:]]*")[[1L]]
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
        if (length(x)) {
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
        } else {
            x <- R.home("bin")
        }
    }))


    R_version_pattern <- "^(([[:digit:]]+)\\.([[:digit:]]+))\\.[[:digit:]]+$"
    R$versions <- vapply(R$bin, function(xx) {
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
    R$major_minor <- sub(R_version_pattern, "\\1", R$versions)
    R <- do.call("rbind", lapply(
        split(R, factor(R$major_minor, unique(R$major_minor))),
        function(r) {
            n <- nrow(r)
            if (n == 1L)
               r
            else
                r[order(r$versions)[n], , drop = FALSE]
        }
    ))


    main.dir <- this.path::here(.. = 1)


    build_binary <- function(pkg, r) {
        # main.dir <- this.path::here(.. = 1); pkg <- "this.path"; r <- data.frame(bin = Sys.getenv("r_oldrel"), version = "4.2.3", major_minor = "4.2"); stop("remove this later")
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
        bin.file <- paste0(pkgname, "_", version, ext)
        bin.dir <- file.path("bin", platform, "contrib", r$major_minor)
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


        ## use getwd() instead of "." to get absolute file paths
        files <- list.files(getwd(), full.names = TRUE)
        files <- files[startsWith(basename(files), paste0(pkgname, "_"))]
        if (length(files) <= 0L) {
        } else if (length(files) == 1L) {
            backup <- tempfile("backup_", tmpdir = dirname(files), fileext = ext)
            if (!file.rename(files, backup)) {
                warning("unable to rename old tarball as a backup")
                return(FALSE)
            }
            on.exit(if (failure) file.rename(backup, files) else file.remove(backup), add = TRUE)
        } else {
            warning("invalid 'files'")
            return(FALSE)
        }


        command <- shQuote(file.path(r$bin, if (.Platform$OS.type == "windows") "R.exe" else "R"))
        command <- paste(command, "CMD", "INSTALL", "--build", shQuote(tar.path))
        cat("\n", command, "\n", sep = "")
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


    build_binaries <- function(r) {
        vapply(pkgs, build_binary, r, FUN.VALUE = NA)
    }


    pkgs <- args
    if ("--all" %in% pkgs)
        pkgs <- setdiff(
            read.dcf(
                file.path(main.dir, "src", "contrib", "PACKAGES"),
                fields = "Package"
            ),
            pkgs
        )


    unloadNamespace("this.path")
    e <- Sys.getenv(c("R_LIBS", "R_LIBS_USER", "R_LIBS_SITE"), NA)
    if (any(unset <- is.na(e)))
        on.exit(Sys.unsetenv(names(which(unset))), add = TRUE)
    if (any(set <- !unset))
        on.exit(do.call(Sys.setenv, as.list(e[set])), add = TRUE)
    Sys.unsetenv(names(e))


    for (i in seq_len(nrow(R))) {
        cat("\n", "Building binaries for R ", R$versions[i], "\n", sep = "")
        print(tryCatch(build_binaries(R[i, , drop = TRUE]), error = identity))
        cat("\n")
    }
}


if (this.path::is.main()) main()
