local({
    owd <- getwd()
    if (is.null(owd))
        stop("cannot 'chdir' as current directory is unknown")
    on.exit(setwd(owd))


    fun <- function(command) {
        cat("\n", command, "\n", sep = "")
        res <- system(command)
        cat("\n")
        if (res) {
            if (res == -1L)
                stop(gettextf("'%s' could not be run", command), domain = NA)
            else stop(gettextf("'%s' execution failed with error code %d", command, res), domain = NA)
        }
    }


    git <- if (.Platform$OS.type == "windows") "cmd /c git" else "git"
    for (branch in c("src", "bin")) {
        setwd(this.path::here(.. = 1, branch))
        fun(sprintf("%s add *"         , git))
        fun(sprintf("%s commit -m %s"  , git, essentials::shEncode(paste("commit", structure(Sys.time(), tzone = "UTC")))))
        fun(sprintf("%s push origin %s", git, branch))
    }
})
