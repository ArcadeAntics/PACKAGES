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


        command <- shQuote(file.path(r$bin, if (.Platform$OS.type == "windows") "R.exe" else "R"))
        command <- paste(command, "CMD", "INSTALL", "--build", shQuote(tar_path))
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
