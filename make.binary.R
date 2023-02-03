main <- function (args = this.path::progArgs())
{
    x <- Sys.getenv(c("r-release", "r-oldrel"), NA)
    if (any(i <- is.na(x))) {
        warning(sprintf(
            ngettext(sum(i), "environment variable %s is not defined",
                             "environment variables %s are not defined"),
            paste(encodeString(names(x)[i], quote = "\""), collapse = ", ")))
        x <- x[!i]
        if (length(x) <= 0L)
            x <- R.home("bin")
    }


    if (!this.path::from.shell())
        stop("can only run from a shell")


    FILE <- this.path::here(.. = 1, "bin", "make.binary.R")
    FILE <- this.path::as.rel.path(FILE, relative.to = ".")


    if (length(args) <= 0L) {
        if (interactive())
            args <- strsplit(readline("Packages to build binaries: "), "[[:blank:]]+|[[:blank:]]*[,;][[:blank:]]*")[[1L]]
        else stop("must provide arguments or be in interactive mode")
        if (length(args) <= 0L)
            stop("expected at least 1 argument")
    }


    if (startsWith(FILE, "-")) {
        apt <- if (.Platform$OS.type == "windows") "Rterm.exe" else "R"
        R_DEFAULT_PACKAGES <- Sys.getenv("R_DEFAULT_PACKAGES", NA)
        if (is.na(R_DEFAULT_PACKAGES)) {
            on.exit(Sys.unsetenv("R_DEFAULT_PACKAGES"))
        } else {
            on.exit(Sys.setenv(R_DEFAULT_PACKAGES = R_DEFAULT_PACKAGES))
        }
        Sys.setenv(R_DEFAULT_PACKAGES = "NULL")
        args <- c(
            "--no-echo", "--no-restore", "--vanilla",
            paste0("--file=", shQuote(FILE)),
            if (length(args)) c("--args", shQuote(args))
        )
    } else {
        apt <- if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"
        args <- c(
            "--default-packages=NULL", "--vanilla",
            shQuote(FILE),
            if (length(args)) shQuote(args)
        )
    }


    apts <- shQuote(this.path::path.join(x, apt))
    unloadNamespace("this.path")
    for (apt in apts) {
        command <- paste(c(apt, args), collapse = " ")
        cat("\n", command, "\n", sep = "")
        system(command)
        cat("\n")
    }
}


if (this.path::is.main()) main()
