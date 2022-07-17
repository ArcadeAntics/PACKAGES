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


if (isTRUE(attr(this.path::this.path(verbose = FALSE), "this.path.from.shell"))) {
    FILE <- file.path(dirname(this.path::shFILE()), "..", "bin", "make.binary.R")
    args <- commandArgs(trailingOnly = TRUE)
} else {
    FILE <- this.path::here(.. = 1, "bin", "make.binary.R")
    args <- strsplit(readline("Packages to build binaries: "), "[, ]+")[[1L]]
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
