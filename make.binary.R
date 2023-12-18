main <- function (args = this.path::progArgs())
{
    # args <- this.path::progArgs(); stop("comment this out later")
    x <- Sys.getenv(c("r_release", "r_oldrel"), NA)
    if (any(i <- is.na(x))) {
        warning(sprintf(
            ngettext(sum(i), "environment variable %s is not defined",
                             "environment variables %s are not defined"),
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
        keep <- file.exists(y)
        y <- y[keep]
        m <- m[keep]
        z <- regmatches(names(y), m)
        z <- vapply(z, function(zz) as.integer(zz[-1L]), integer(2), USE.NAMES = FALSE)
        y <- y[order(z[1L, ], z[2L, ], decreasing = TRUE)]
        x <- c(x, y)
    } else {
        x <- R.home("bin")
    }


    if (!this.path::from.shell())
        stop("can only run from a shell")


    FILE <- this.path::here(.. = 1, "bin", "make.binary.R")
    FILE <- this.path::relpath(FILE)


    if (length(args) <= 0L) {
        if (interactive())
            args <- strsplit(readline("Packages to build binaries: "), "[[:blank:]]+|[[:blank:]]*[,;][[:blank:]]*")[[1L]]
        else stop("must provide arguments or be in interactive mode")
        if (length(args) <= 0L)
            stop("expected at least 1 argument")
    }
    ## add main.dir to args
    args <- c(this.path::here(.. = 1), args)


    if (startsWith(FILE, "-")) {
        apt <- if (.Platform$OS.type == "windows") "Rterm.exe" else "R"
        R_DEFAULT_PACKAGES <- Sys.getenv("R_DEFAULT_PACKAGES", NA)
        if (is.na(R_DEFAULT_PACKAGES)) {
            on.exit(Sys.unsetenv("R_DEFAULT_PACKAGES"), add = TRUE)
        } else {
            on.exit(Sys.setenv(R_DEFAULT_PACKAGES = R_DEFAULT_PACKAGES), add = TRUE)
        }
        Sys.setenv(R_DEFAULT_PACKAGES = "NULL")
        args <- c(
            "-s", "--no-restore", if (.Platform$OS.type == "unix") "--no-readline", "--vanilla",
            paste0("--file=", shQuote(FILE)),
            "--args", shQuote(args)
        )
    } else {
        apt <- if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"
        args <- c(
            "--default-packages=NULL", "--vanilla",
            shQuote(FILE),
            shQuote(args)
        )
    }


    apts <- shQuote(this.path::path.join(x, apt))
    unloadNamespace("this.path")
    e <- Sys.getenv(c("R_LIBS", "R_LIBS_USER", "R_LIBS_SITE"), NA)
    if (any(unset <- is.na(e)))
        on.exit(Sys.unsetenv(names(which(unset))), add = TRUE)
    if (any(set <- !unset))
        on.exit(do.call(Sys.setenv, as.list(e[set])), add = TRUE)
    Sys.unsetenv(names(e))
    for (apt in apts) {
        command <- paste(c(apt, args), collapse = " ")
        cat("\n", command, "\n", sep = "")
        system(command)
        cat("\n")
    }
}


if (this.path::is.main()) main()
