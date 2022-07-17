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


if (this.path::from.shell()) {
    FILE <- dirname(this.path::shFILE())
    FILE <- if (FILE == ".")
        file.path("..", "bin", "make.binary.R")
    else file.path(FILE, "..", "bin", "make.binary.R")
} else FILE <- this.path::here(.. = 1, "bin", "make.binary.R")
args <- this.path::fileArgs()
if (length(args) <= 0L) {
    if (interactive())
        args <- strsplit(readline("Packages to build binaries: "), "[, ]+")[[1L]]
    else stop("must provide arguments or be in interactive mode")
    if (length(args) <= 0L)
        stop("expected at least 1 argument")
}
FILE <- paste0("--file=", shQuote(FILE))
args <- if (length(args)) c("--args", shQuote(args))


for (name in x) {
    command <- paste(c(
        shQuote(file.path(name, "R")),
        "--no-echo", "--no-restore",
        FILE, args
    ), collapse = " ")
    cat("\n", command, "\n", sep = "")
    system(command)
    cat("\n")
}
