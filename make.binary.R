if (FALSE) {


sourcelike <- function (file)
{
    filename <- normalizePath(file, "/", TRUE)
    envir <- new.env(hash = TRUE, parent = .BaseNamespaceEnv)
    envir$.packageName <- filename
    exprs <- parse(filename, n = -1L, keep.source = TRUE)
    eval(exprs, envir)
    envir
}


repos_R <- sourcelike("~/PACKAGES/src/repos.R")
repos <- repos_R$make_repos("~/test")
unlink(repos$repos_dir, recursive = TRUE, force = TRUE)
dir.create(repos$repos_dir)


repos$copy_tarball("~/this.path/this.path_2.5.0.77.tar.gz", "4.5.0/Recommended")


repos$find_tarball("this.path")


 dir.create(file.path(repos$repos_dir, "bin/windows/contrib/4.4"), showWarnings = FALSE, recursive = TRUE)
file.create(file.path(repos$repos_dir, "bin/windows/contrib/4.4/this.path_2.5.0.76.zip"))


unloadNamespace("essentials"); unloadNamespace("this.path")
repos$build_binary("this.path")


repos$copy_binary(
    "~/PACKAGES/bin/macosx/big-sur-arm64/contrib/4.3/this.path_2.4.0.1.tgz",
    "bin/macosx/big-sur-arm64/contrib/4.3"
)


dir(repos$repos_dir, all.files = TRUE, recursive = TRUE, include.dirs = TRUE)
write.dcf(read.dcf(file.path(repos$repos_dir, "src/contrib/PACKAGES"                         ), repos_R$.src_fields))
write.dcf(read.dcf(file.path(repos$repos_dir, "bin/windows/contrib/4.4/PACKAGES"             ), repos_R$.bin_fields))
write.dcf(read.dcf(file.path(repos$repos_dir, "bin/macosx/big-sur-arm64/contrib/4.3/PACKAGES"), repos_R$.bin_fields))


}


main <- function (args = this.path::progArgs())
{
    # args <- "this.path"; stop("remove this later")
    # args <- c("this.path (R >= 2.14.0) (R >= r56550)", "essentials (R >= 4.0)", "iris "); stop("remove this later")
    if (length(args) <= 0L) {
        if (interactive())
            args <- strsplit(readline("Packages to build binaries: "), ",")[[1L]]
        else stop("must provide arguments or be in interactive mode")
        if (length(args) <= 0L)
            stop("expected at least 1 argument")
    }
    pkgname_pattern <- "([[:alpha:]][[:alnum:].]*[[:alnum:]])"
    ops_pattern <- "(<|>|<=|>=|==|!=)"
    version_patterns <- c(
        "((?:[[:digit:]]+[.-]){1,}[[:digit:]]+)",
        "r([[:digit:]]+)"
    )
    space_pattern <- "[[:space:]]*"
    pattern <- paste0(
        "^",
        space_pattern,
        pkgname_pattern,
            "(",
            "(?:",
            space_pattern,
            "\\(",
            space_pattern,
            "R",
            space_pattern,
            ops_pattern,
            space_pattern,
                "(?:",
                paste(version_patterns, collapse = "|"),
                ")",
            "\\)",
            ")*",
            ")",
        space_pattern,
        "$"
    )
    m <- regexec(pattern, args)
    if (any(lengths(m) == 1L))
        stop("invalid arguments, must be package names each optionally followed\n by a comment in parentheses specifying an R version requirement\n that is \"<pkgname> (R <op> <version>)\" i.e. \"this.path (R >= 4.0)\"")
    args <- regmatches(args, m)
    args <- lapply(args, function(args) {
        v <- strsplit(args[[3L]], ")", fixed = TRUE)[[1L]]
        pattern <- paste0(
            "^",
            space_pattern,
            "\\(",
            space_pattern,
            "R",
            space_pattern,
            ops_pattern,
            space_pattern,
                "(?:",
                paste(version_patterns, collapse = "|"),
                ")",
            "$"
        )
        m <- regexec(pattern, v)
        if (any(lengths(m) == 1L))
            stop("invalid 'pattern'; should never happen, please report!")
        list(
            pkgname = args[[2L]],
            R = lapply(regmatches(v, m), function(r) {
                list(
                    op = r[2L],
                    version = if (nzchar(r[3L]))
                        package_version(r[3L])
                    else as.integer(r[4L])
                )
            })
        )
    })
    cmp <- function(e1, op, e2) {
        op <- switch(op, `<` = `<`, `>` = `>`, `<=` = `<=`, `>=` = `>=`, `==` = `==`, NULL)
        if (is.null(op)) {
            if (!length(e1) || !length(e2))
                return(logical())
            rep(TRUE, max(length(e1), length(e2)))
        }
        else op(e1, e2)
    }


    ## load all necessary namespaces
    loadNamespace("utils")
    loadNamespace("this.path")


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


    sourcelike <- function(file) {
        filename <- normalizePath(file, "/", TRUE)
        envir <- new.env(hash = TRUE, parent = .BaseNamespaceEnv)
        envir$.packageName <- filename
        exprs <- parse(filename, n = -1L, keep.source = TRUE)
        eval(exprs, envir)
        envir
    }


    repos_R <- sourcelike(file.path(main_dir, "src/repos.R"))
    repos <- repos_R$make_repos(main_dir)


    R <- local({
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
        if (length(x)) x else list(NULL)
    })
    R <- lapply(R, repos_R$make_R)
    R <- local({
        f <- vapply(R, `[[`, "", "major_minor")
        f <- factor(f, unique(f))
        lapply(split(R, f), function(r) {
            n <- length(r)
            if (n == 1L)
                r[[1L]]
            else r[[order(do.call("c", lapply(R, `[[`, "version")))[n]]]
        })
    })


    all_pkgs <- read.dcf(
        file.path(main_dir, "src/contrib/PACKAGES"),
        fields = "Package"
    )


    unloadNamespace("this.path")
    e <- Sys.getenv(c("R_LIBS", "R_LIBS_USER", "R_LIBS_SITE"), NA)
    if (any(unset <- is.na(e)))
        on.exit(Sys.unsetenv(names(which(unset))), add = TRUE, after = FALSE)
    if (any(set <- !unset))
        on.exit(do.call(Sys.setenv, as.list(e[set])), add = TRUE, after = FALSE)
    Sys.unsetenv(names(e))


    for (argsi in args) {
        i <- local({
            i <- rep(TRUE, length(R))
            names(i) <- names(R)
            for (argsir in argsi$R) {
                if (is.numeric_version(argsir$version))
                    i <- i & cmp(
                        do.call("c", lapply(R, `[[`, "version")),
                        argsir$op,
                        argsir$version
                    )
                else
                    i <- i & cmp(
                        vapply(R, `[[`, 0L, "svn_rev"),
                        argsir$op,
                        argsir$version
                    )
            }
            tarpath <- repos$find_tarball(argsi$pkgname)
            depends <- repos_R$.read_DESCRIPTION_from_tarball(tarpath, "Depends")
            depends <- strsplit(depends, ",", fixed = TRUE)[[1L]]
            pattern <- paste0(
                "^",
                space_pattern,
                "R",
                space_pattern,
                "\\(",
                space_pattern,
                ops_pattern,
                space_pattern,
                "(?:",
                paste(version_patterns, collapse = "|"),
                ")",
                space_pattern,
                "\\)",
                space_pattern,
                "$"
            )
            m <- regexec(pattern, depends)
            if (any(keep <- lengths(m) == 4L)) {
                depends <- regmatches(depends[keep], m[keep])
                for (depends in depends) {
                    if (nzchar(depends[[3L]]))
                        i <- i & cmp(
                            do.call("c", lapply(R, `[[`, "version")),
                            depends[[2L]],
                            depends[[3L]]
                        )
                    else
                        i <- i & cmp(
                            vapply(R, `[[`, 0L, "svn_rev"),
                            depends[[2L]],
                            as.integer(depends[[4L]])
                        )
                }
            }
            i
        })
        for (r in R[which(i)]) {
            cat(sprintf(
                "\nBuilding package:%s binary for R %s\n",
                argsi$pkgname,
                format(r$version)
            ))
            x <- withVisible(
                tryCatch({
                    invisible(repos$build_binary(argsi$pkgname, r))
                }, error = identity)
            )
            if (x$visible)
                print(x$value)
            cat("\n")
        }
    }
}


if (this.path::is.main()) main()
