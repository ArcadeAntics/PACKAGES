if (isTRUE(attr(this.path::this.path(verbose = FALSE), "this.path.from.shell"))) {
    command <- paste(c(
        shQuote(file.path(R.home("bin"), "R")),
        "--no-echo", "--no-restore",
        paste0("--file=", shQuote(file.path(this.path::shFILE(), "..", "..", "bin", "make.binary.R"))),
        if (length(args <- commandArgs(trailingOnly = TRUE)))
            c("--args", shQuote(args))
    ), collapse = " ")
    cat("\n", command, "\n", sep = "")
    system(command)
    cat("\n")
} else source(this.path::here(.. = 1, "bin", "make.binary.R"), environment(), echo = FALSE)
