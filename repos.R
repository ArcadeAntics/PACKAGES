#' DCF Fields in PACKAGES Files
#'
#' @aliases .src_fields .bin_fields
#'
#' @description
#' \code{.src_fields} is a character vector, the names of the fields of a source
#' type \file{PACKAGES} file.
#'
#' \code{.bin_fields} is a character vector, the names of the fields of a binary
#' type \file{PACKAGES} file.
#'
#' @usage .src_fields
#' @usage .bin_fields
#'
#' @format NULL
#'
#' @details
#' This is used in combination with \code{\link{read.dcf}()}.
#'
#' @section Value:
#' \verb{> .src_fields
#'  [1] Package               Version               Priority
#'  [4] Depends               Imports               LinkingTo
#'  [7] Suggests              Enhances              License
#' [10] License_is_FOSS       License_restricts_use OS_type
#' [13] Archs                 MD5sum                NeedsCompilation
#' [16] Path}
#'
#' \verb{> .bin_fields
#'  [1] Package               Version               Priority
#'  [4] Depends               Imports               LinkingTo
#'  [7] Suggests              Enhances              License
#' [10] License_is_FOSS       License_restricts_use OS_type
#' [13] Archs}
#'
#' @export
.src_fields <- c(
    "Package", "Version", "Priority", "Depends",
    "Imports", "LinkingTo", "Suggests", "Enhances",
    "License", "License_is_FOSS", "License_restricts_use",
    "OS_type", "Archs", "MD5sum", "NeedsCompilation",
    "Path"
)


#' @export
.bin_fields <- c(
    "Package", "Version", "Priority", "Depends",
    "Imports", "LinkingTo", "Suggests", "Enhances",
    "License", "License_is_FOSS", "License_restricts_use",
    "OS_type", "Archs"
)


#' Read DESCRIPTION of Package
#'
#' @name .read_DESCRIPTION
#'
#' @aliases .read_DESCRIPTION_from_dir .read_DESCRIPTION_from_tarball .read_DESCRIPTION_from_zip_archive .read_DESCRIPTION
#'
#' @description
#' Reads the contents of a \file{DESCRIPTION} file.
#'
#' @usage .read_DESCRIPTION_from_dir(dir, \dots)
#' @usage .read_DESCRIPTION_from_tarball(tarpath, \dots)
#' @usage .read_DESCRIPTION_from_zip_archive(zippath, \dots)
#' @usage .read_DESCRIPTION(path, \dots)
#'
#' @param dir character string; path of the package directory.
#' @param tarpath character string; path of the package tarball.
#' @param zippath character string; path of the package zip archive.
#' @param path character string; path of the package in one of the previous formats.
#' @param \dots further arguments passed to \code{\link{read.dcf}()}.
#'
#' @details
#' \code{.read_DESCRIPTION()} will guess the format of the package in the
#' following manner:
#'
#' \enumerate{
#'   \item{If \code{path} is an existing directory, invokes
#'     \code{.read_DESCRIPTION_from_dir()}.}
#'
#'   \item{If \code{path} ends with \code{".tar.gz"}, invokes
#'     \code{.read_DESCRIPTION_from_tarball()}.}
#'
#'   \item{If \code{path} ends with \code{".zip"}, invokes
#'     \code{.read_DESCRIPTION_from_zip_archive()}.}
#'
#'   \item{If \code{path} ends with \code{".tgz"}, invokes
#'     \code{.read_DESCRIPTION_from_tarball()}.}
#'
#'   \item{Otherwise, throws an error.}
#' }
#'
#' @section Value:
#' named character vector.
#'
#' @export
.read_DESCRIPTION_from_dir <- function (dir, ...)
{
    dir <- path.expand(dir)


    desc <- read.dcf(file.path(dir, "DESCRIPTION"), ...)
    if (nrow(desc) != 1L)
        stop("contains a blank line", call. = FALSE)
    structure(c(desc), names = colnames(desc))
}


#' @export
.read_DESCRIPTION_from_tarball <- function (tarpath, ...)
{
    tarpath <- path.expand(tarpath)


    files <- utils::untar(tarpath, list = TRUE)
    files <- grep(
        # sprintf("^%s/DESCRIPTION$", .standard_regexps()$valid_package_name),
        # "^[[:alpha:]][[:alnum:].]*[[:alnum:]]/DESCRIPTION$",
        "^[ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz][0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz.]*[0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz]/DESCRIPTION$",
        files,
        value = TRUE,
        useBytes = TRUE
    )
    if (length(files) != 1L)
        stop(gettextf("invalid '%s'", "tarpath"))
    exdir <- tempfile("exdir", tempdir(TRUE))
    tryCatch({
        utils::untar(tarpath, files, exdir = exdir)
        desc <- read.dcf(file = file.path(exdir, files), ...)
    }, finally = {
        unlink(exdir, recursive = TRUE, force = TRUE)
    })
    if (nrow(desc) != 1L)
        stop("contains a blank line", call. = FALSE)
    structure(c(desc), names = colnames(desc))
}


#' @export
.read_DESCRIPTION_from_zip_archive <- function (zippath, ...)
{
    zippath <- path.expand(zippath)


    files <- utils::unzip(zippath, list = TRUE)$Name
    files <- grep(
        # sprintf("^%s/DESCRIPTION$", .standard_regexps()$valid_package_name),
        # "^[[:alpha:]][[:alnum:].]*[[:alnum:]]/DESCRIPTION$",
        "^[ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz][0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz.]*[0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz]/DESCRIPTION$",
        files,
        value = TRUE,
        useBytes = TRUE
    )
    if (length(files) != 1L)
        stop(gettextf("invalid '%s'", "zippath"))
    conn <- unz(zippath, files)
    tryCatch({
        desc <- read.dcf(file = conn, ...)
    }, finally = {
        close(conn)
    })
    if (nrow(desc) != 1L)
        stop("contains a blank line", call. = FALSE)
    structure(c(desc), names = colnames(desc))
}


#' @export
.read_DESCRIPTION <- function (path, ...)
{
    path <- path.expand(path)
    if (dir.exists(path))
        .read_DESCRIPTION_from_dir(dir = path, ...)
    else if (endsWith(path, ".tar.gz"))
        .read_DESCRIPTION_from_tarball(tarpath = path, ...)
    else if (endsWith(path, ".zip"))
        .read_DESCRIPTION_from_zip_archive(zippath = path, ...)
    else if (endsWith(path, ".tgz"))
        .read_DESCRIPTION_from_tarball(tarpath = path, ...)
    else stop(gettextf("invalid '%s' value", "path", domain = "R"), domain = NA)
}


#' Write PACKAGES File
#'
#' @description
#' Writes a package's \file{DESCRIPTION} fields to a \file{PACKAGES} file.
#'
#' @usage .write_PACKAGES(desc, dir)
#'
#' @param desc named character vector; contents of the package's
#'   \file{DESCRIPTION} file.
#' @param dir character string; directory in which to write the \file{PACKAGES}
#'   file.
#'
#' @details
#' If \file{PACKAGES} already exists in \code{dir}, \code{.write_PACKAGES()} will
#' append or overwrite the record in the database.
#'
#' @section Value:
#' character string; path of the \file{PACKAGES} file.
#'
#' @export
.write_PACKAGES <- function (desc, dir)
{
    if (!is.character(desc))
        stop("a character vector argument expected", domain = "R")
    if (!length(desc) || is.null(fields <- names(desc)))
        stop(gettextf("invalid '%s' argument", "desc", domain = "R"), domain = NA)


    dir <- path.expand(dir)


    PACKAGES_path <- file.path(dir, "PACKAGES")
    if (file.exists(PACKAGES_path)) {
        all_desc <- read.dcf(PACKAGES_path, fields)
    } else {
        all_desc <- matrix(
            character(0),
            nrow = 0L,
            ncol = length(fields),
            dimnames = list(NULL, fields)
        )
    }
    if (i <- match(desc[["Package"]], all_desc[, "Package"], 0L)) {
        all_desc[i, ] <- desc
    } else {
        all_desc <- rbind(all_desc, desc)
    }
    all_desc <- all_desc[order(all_desc[, "Package"]), , drop = FALSE]
    tmpfile <- tempfile("PACKAGES")
    on.exit(unlink(tmpfile), add = TRUE)
    write.dcf(all_desc, tmpfile, indent = 8L, width = 72L)
    if (!file.copy(tmpfile, PACKAGES_path, overwrite = TRUE, copy.date = TRUE))
        stop(sprintf("unable to copy file '%s' to '%s'", tmpfile, PACKAGES_path))


    PACKAGES_path
}


#' Find R CMD Executable
#'
#' @description
#' Finds the relevant executable and quotes the strings to be passed to an
#' operating system shell.
#'
#' @usage .find_R_CMD(dir)
#'
#' @param dir character string; \code{"bin"} component of the \R home directory.
#'
#' @details
#' This is mostly desirable on Windows where invoking \command{R CMD} does not
#' support a UNC path as the working directory.
#'
#' @section Value:
#' character vector.
#'
#' @examples
#' .find_R_CMD(R.home("bin"))
#'
#' @export
.find_R_CMD <- function (dir)
{
    if (.Platform$OS.type == "windows") {
        if (file.exists(command <- file.path(dir, "Rcmd.exe")))
            shQuote(command)
        else if (file.exists(command <- file.path(dir, "x64/Rcmd.exe")))
            shQuote(command)
        else if (file.exists(command <- file.path(dir, "i386/Rcmd.exe")))
            shQuote(command)
        else c(shQuote(file.path(dir, "R.exe")), "CMD")
    }
    else c(shQuote(file.path(dir, "R")), "CMD")
}


#' @export
.switch2 <- function (EXPR, TRUE_expr = invisible(), FALSE_expr = invisible(),
    alt_expr = invisible())
{
    if (is.character(EXPR)) {
        switch(EXPR,
        T = ,
        `TRUE` = ,
        True = ,
        true = TRUE_expr,
        F = ,
        `FALSE` = ,
        False = ,
        false = FALSE_expr,
        if (!is.na(EXPR)) alt_expr else if (EXPR) NULL)
    }
    else if (EXPR)
        TRUE_expr
    else FALSE_expr
}


#' @export
.make_R_CMD_build_opts <- function (
    ...,
    force = FALSE,
    keep_empty_dirs = FALSE,
    no_build_vignettes = FALSE,
    no_manual = FALSE,
    resave_data = FALSE, no_resave_data = FALSE,
    compact_vignettes = FALSE,
    compression = NULL,
    md5 = FALSE,
    log = FALSE,
    user = NULL)
{
    dots <- list(...)
    n <- names(dots)
    if (!is.null(n) && length(i <- which(nzchar(n)))) {
        f <- setdiff(names(formals()), "...")
        f <- utils::capture.output(print(f, quote = FALSE, max = 99))
        warning(
            "named argument(s) ",
            paste(dQuote(n[i]), collapse = ", "),
            "\ndo not match names of formals:\n",
            paste(f, collapse = "\n")
        )
        names(dots) <- NULL
    }
    if (!all(vapply(dots, is.character, NA))) {
        stop("non-character argument(s)")
    }
    c(
        if (force) "--force",
        if (keep_empty_dirs) "--keep-empty-dirs",
        if (no_build_vignettes) "--no-build-vignettes",
        if (no_manual) "--no-manual",
        .switch2(resave_data,
        TRUE_expr = "--resave-data",
        FALSE_expr = if (no_resave_data) "--no-resave-data",
        alt_expr = paste0("--resave-data=", match.arg(resave_data, c("no", "best", "gzip")))
        ),
        .switch2(compact_vignettes,
        TRUE_expr = "--compact-vignettes",
        alt_expr = paste0("--compact-vignettes=", match.arg(compact_vignettes, c("no", "qpdf", "gs", "gs+qpdf", "both")))
        ),
        if (!is.null(compression))
            paste0("--compression=", match.arg(compression, c("gzip", "none", "bzip2", "xz"))),
        if (md5) "--md5",
        if (log) "--log",
        if (!is.null(user)) {
            if (is.character(user) && length(user) == 1L)
                paste0("--user=", user)
            else stop(gettextf("invalid '%s' value", "user", domain = "R"), domain = NA)
        },
        dots,
        recursive = TRUE,
        use.names = FALSE
    )
}


#' @export
.make_R_CMD_INSTALL_opts <- function(
    ...,
    clean = FALSE,
    preclean = FALSE,
    debug = FALSE,
    library = NULL,
    no_configure = FALSE,
    no_docs = FALSE,
    html = FALSE, no_html = FALSE,
    latex = FALSE,
    example = FALSE,
    fake = FALSE,
    no_lock = FALSE,
    lock = FALSE,
    pkglock = FALSE,
    build = FALSE,
    install_tests = FALSE,
    no_R = FALSE, no_libs = FALSE, no_data = FALSE, no_help = FALSE, no_demo = FALSE, no_exec = FALSE, no_inst = FALSE,
    no_multiarch = FALSE,
    libs_only = FALSE,
    data_compress = NULL,
    resave_data = FALSE,
    compact_docs = FALSE,
    with_keep.source = FALSE, without_keep.source = FALSE,
    with_keep.parse.data = FALSE, without_keep.parse.data = FALSE,
    byte_compile = FALSE, no_byte_compile = FALSE,
    staged_install = FALSE, no_staged_install = FALSE,
    no_test_load = FALSE,
    no_clean_on_error = FALSE,
    merge_multiarch = FALSE,
    use_vanilla = FALSE,
    use_LTO = FALSE, no_use_LTO = FALSE)
{
    dots <- list(...)
    n <- names(dots)
    if (!is.null(n) && length(i <- which(nzchar(n)))) {
        f <- setdiff(names(formals()), "...")
        f <- utils::capture.output(print(f, quote = FALSE, max = 99))
        warning(
            "named argument(s) ",
            paste(dQuote(n[i]), collapse = ", "),
            "\ndo not match names of formals:\n",
            paste(f, collapse = "\n")
        )
        names(dots) <- NULL
    }
    if (!all(vapply(dots, is.character, NA))) {
        stop("non-character argument(s)")
    }
    c(
        if (clean) "--clean",
        if (preclean) "--preclean",
        if (debug) "--debug",
        if (!is.null(library)) {
            if (is.character(library) && length(library) == 1L)
                paste0("--library=", library)
            else stop(gettextf("invalid '%s' value", "library", domain = "R"), domain = NA)
        },
        if (no_configure) "--no-configure",
        if (no_docs) "--no-docs",
        if (html) "--html" else if (no_html) "--no-html",
        if (latex) "--latex",
        if (example) "--example",
        if (fake) "--fake",
        if (no_lock) "--no-lock" else if (lock) "--lock" else if (pkglock) "--pkglock",
        if (build) "--build",
        if (install_tests) "--install-tests",
        if (no_R) "--no-R",
        if (no_libs) "--no-libs",
        if (no_data) "--no-data",
        if (no_help) "--no-help",
        if (no_demo) "--no-demo",
        if (no_exec) "--no-exec",
        if (no_inst) "--no-inst",
        if (no_multiarch) "--no-multiarch",
        if (libs_only) "--libs-only",
        if (!is.null(data_compress))
            paste0("--data-compress=", match.arg(data_compress, c("gzip", "none", "bzip2", "xz"))),
        if (resave_data) "--resave-data",
        if (compact_docs) "--compact-docs",
        if (with_keep.source) "--with-keep.source" else if (without_keep.source) "--without-keep.source",
        if (with_keep.parse.data) "--with-keep.parse.data" else if (without_keep.parse.data) "--without-keep.parse.data",
        if (byte_compile) "--byte-compile" else if (no_byte_compile) "--no-byte-compile",
        if (staged_install) "--staged-install" else if (no_staged_install) "--no-staged-install",
        if (no_test_load) "--no-test-load",
        if (no_clean_on_error) "--no-clean-on-error",
        if (merge_multiarch) "--merge-multiarch",
        if (use_vanilla) "--use-vanilla",
        if (use_LTO) "--use-LTO" else if (no_use_LTO) "--no-use-LTO",
        dots,
        recursive = TRUE,
        use.names = FALSE
    )
}


#' Invoke a System Command
#'
#' @description
#' Invoke the OS command specified by \code{command}.
#'
#' @usage .system(command, intern = FALSE, \dots,
#'     dry.run = FALSE, mustWork = NA, quiet = intern)
#'
#' @param command,intern,\dots arguments passed to \code{\link{system}()}.
#' @param dry.run \code{TRUE} or \code{FALSE}; return the command without invoking?
#' @param mustWork a logical; if \code{TRUE} failure to run the command will give
#'   an \R error, if \code{NA} a warning, and if \code{FALSE}, no \R message.
#' @param quiet \code{TRUE} or \code{FALSE}; print a message before and after
#'   invoking \code{command}?
#'
#' @section Value:
#' if \code{dry.run} is \code{TRUE}, \code{command}.
#'
#' if \code{intern} is \code{FALSE}, the result of \code{system()} invisibly.
#'
#' if \code{intern} is \code{TRUE}, the result of \code{system()} visibly.
#'
#' @seealso
#' \code{\link{system}}
#'
#' @export
.system <- function (command, intern = FALSE, ..., dry.run = FALSE, mustWork = NA,
    quiet = intern)
{
    if (dry.run)
        return(command)
    if (!quiet)
        cat("$ ", command, "\n", sep = "")
    value <- system(command = command, intern = intern, ...)
    if (intern) {
        if (!is.null(status <- attr(value, "status")) && status) {
            if (isFALSE(mustWork)) {
                if (!quiet)
                    cat("\nProcess finished with exit code ", status, "\n", sep = "")
            }
            else if (isTRUE(mustWork)) {
                if (status == -1L)
                    stop(gettextf("'%s' could not be run", command, domain = "R-base"), domain = NA)
                else stop(gettextf("'%s' execution failed with error code %d", command, status, domain = "R-base"), domain = NA)
            }
            else if (status == -1L)
                warning(gettextf("'%s' could not be run", command, domain = "R-base"), domain = NA)
            else warning(gettextf("'%s' execution failed with error code %d", command, status, domain = "R-base"), domain = NA)
        }
        value
    }
    else {
        if (!value || isFALSE(mustWork)) {
            if (!quiet)
                cat("\nProcess finished with exit code ", value, "\n", sep = "")
        }
        else if (isTRUE(mustWork)) {
            if (value == -1L)
                stop(gettextf("'%s' could not be run", command, domain = "R-base"), domain = NA)
            else stop(gettextf("'%s' execution failed with error code %d", command, value, domain = "R-base"), domain = NA)
        }
        else if (value == -1L)
            warning(gettextf("'%s' could not be run", command, domain = "R-base"), domain = NA)
        else warning(gettextf("'%s' execution failed with error code %d", command, value, domain = "R-base"), domain = NA)
        invisible(value)
    }
}


#' Class R Objects
#'
#' @aliases make_R as_R
#'
#' @description
#' Create or coerce objects of class \code{"R"} containing information about the
#' specified installation of \R.
#'
#' @usage make_R(bin = NULL)
#' @usage as_R(x, \dots)
#'
#' @param bin \code{NULL} or character string; \code{"bin"} component of the \R
#'   home directory.
#' @param x object to be coerced.
#' @param \dots further arguments passed to or from other methods.
#'
#' @section Value:
#' An object of class \code{"R"} with at least the following members:
#'
#' \describe{
#'   \item{bin}{
#'
#'     \code{"bin"} component of the \R home directory.}
#'
#'   \item{version}{
#'
#'     corresponding version of \R as an \code{\link{R_system_version}} object.}
#'
#'   \item{major_minor}{
#'
#'     corresponding version of \R as a character string, excluding the patch
#'     level.}
#'
#'   \item{svn_rev}{
#'
#'     corresponding Subversion revision number, see \code{?\link{R.Version}}.}
#' }
#'
#' @examples
#' make_R()
#'
#' @export
make_R <- function (bin = NULL)
{
    R_version_pattern <- "^(([[:digit:]]+)\\.([[:digit:]]+))\\.[[:digit:]]+$"
    if (is.null(bin)) {
        bin <- R.home("bin")
        version <- getRversion()
        major_minor <- sub(R_version_pattern, "\\1", version)
        svn_rev <- R.version$`svn rev`
    }
    else {
        args <- c(
            shQuote(file.path(
                bin,
                if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"
            )),
            "--default-packages=NULL",
            "--vanilla",
            "-e", shQuote("writeLines(format(getRversion()))"),
            "-e", shQuote("writeLines(R.version$`svn rev`)")
        )
        command <- paste(args, collapse = " ")
        rval <- .system(command, intern = TRUE, mustWork = TRUE)
        version <- NA_character_
        svn_rev <- NA_integer_
        if (is.character(rval) && length(rval) == 2L && !anyNA(rval)) {
            if (grepl(R_version_pattern, rval[[1L]]))
                version <- rval[[1L]]
            svn_rev <- rval[[2L]]
        }
        version <- R_system_version(version)
        major_minor <- sub(R_version_pattern, "\\1", version)
    }
    svn_rev <- if (grepl("^[[:digit:]]+$", svn_rev)) as.integer(svn_rev) else NA_integer_


    structure(
        list(bin = bin, version = version, major_minor = major_minor, svn_rev = svn_rev),
        class = "R"
    )
}


#' @export
as_R <- function (x, ...)
{
    if (missing(x) || is.null(x))
        return(make_R())
    UseMethod("as_R")
}


#' Coerce to an Object of Class R
#'
#' @aliases as_R.R as_R.default
#'
#' @description
#' Functions to coerce an object to class \code{"R"} if possible.
#'
#' @usage as_R.R(x, \dots)
#' @usage as_R.default(x, \dots)
#'
#' @param x any \R object.
#' @param \dots additional arguments to be passed to or from methods.
#'
#' @details
#' The default method handles \code{NULL} and character strings.
#'
#' @section Value:
#' An object of class \code{"R"}.
#'
#' @export
as_R.R <- function (x, ...)
x
registerS3method("as_R", "R", "as_R.R")


#' @export
as_R.default <- function (x, ...)
{
    if (!is.null(x) && !is.character(x))
        stop(gettextf("invalid '%s' argument", "x", domain = "R"), domain = NA)
    make_R(x)
}
registerS3method("as_R", "default", "as_R.default")


#' Build an R Package
#'
#' @description
#' Build an \R package from a package source in the directory specified by
#' \code{pkgpath}.
#'
#' @usage build_tarball(pkgpath, R = NULL, opts = NULL)
#'
#' @param pkgpath character string; directory of the package source.
#' @param R an object of class \code{"R"}, or coercible.
#' @param opts further options to \command{R CMD build}.
#'
#' @details
#' Invokes \command{R CMD build} to build the \R package.
#'
#' @section Value:
#' character string; filename of the resultant tarball.
#'
#' @export
build_tarball <- function (pkgpath, R = NULL, opts = NULL)
{
    pkgpath <- path.expand(pkgpath)


    R <- as_R(R)
    r_bin <- R$bin


    desc <- .read_DESCRIPTION_from_dir(pkgpath, c("Package", "Version"))
    pkgname <- desc[["Package"]]
    version <- desc[["Version"]]
    if (!grepl(pkgname, pattern = paste0("^(", .standard_regexps()$valid_package_name, ")$")))
        stop("invalid package DESCRIPTION file")
    if (!grepl(version, pattern = paste0("^(", .standard_regexps()$valid_package_version, ")$")))
        stop("invalid package DESCRIPTION file")


    ## the return value
    tarpath <- paste0(pkgname, "_", version, ".tar.gz")


    opts <- if (is.null(opts))
        .make_R_CMD_build_opts()
    else if (is.character(opts) && is.null(names(opts)))
        opts
    else do.call(".make_R_CMD_build_opts", opts, quote = TRUE)
    args <- c(.find_R_CMD(r_bin), "build", opts, shQuote(pkgpath))
    command <- paste(args, collapse = " ")
    .system(command, mustWork = TRUE)


    tarpath
}


#' Copy a Tarball Into a Local Repository
#'
#' @description
#' Copy a package \sQuote{tar} archive into a local repository.
#'
#' @usage copy_tarball_to_repos(tarpath, repos_dir, Path = NULL)
#'
#' @param tarpath character string; path of the package \sQuote{tar} archive.
#' @param repos_dir character string; directory of the local repository.
#' @param Path \code{NULL} or a character string; sub path in which to copy the
#'   package \sQuote{tar} archive.
#'
#' @details
#' \code{Path} is mostly used by \code{CRAN} to keep the recommended packages in a
#' separate directory. For example, \code{Path="4.5.0/Recommended"}.
#'
#' @section Value:
#' character string; path of the copied package \sQuote{tar} archive.
#'
#' \code{<repos_dir>/src/contrib[/<Path>]/<pkgname>_<version>.tar.gz}
#'
#' @export
copy_tarball_to_repos <- function (tarpath, repos_dir, Path = NULL)
{
    tarpath <- path.expand(tarpath)
    repos_dir <- path.expand(repos_dir)
    if (is.null(Path)) {
        Path <- NA_character_
    } else if (is.character(Path) && length(Path) == 1L) {
        if (is.na(Path))
            Path <- NA_character_
        else if (nzchar(Path)) {
            Path <- gsub("\\", "/", Path, fixed = TRUE)
            Path <- gsub("^/+|/+$", "", Path)
            if (!nzchar(Path))
                Path <- NA_character_
        }
        else Path <- NA_character_
    } else if (is.logical(Path) && length(Path) == 1L && is.na(Path)) {
        Path <- NA_character_
    } else {
        stop(gettextf("invalid '%s' argument", "Path", domain = "R"))
    }


    desc <- .read_DESCRIPTION_from_tarball(tarpath, .src_fields)
    desc["MD5sum"] <- tools::md5sum(tarpath)
    desc["Path"] <- Path


    src_contrib_dir <- file.path(repos_dir, "src", "contrib")
    dir.create(src_contrib_dir, showWarnings = FALSE, recursive = TRUE)
    .write_PACKAGES(desc, src_contrib_dir)


    new_tarname <- paste0(desc[["Package"]], "_", desc[["Version"]], ".tar.gz")
    if (!is.na(desc[["Path"]])) {
        dir.create(
            file.path(src_contrib_dir, desc[["Path"]]),
            showWarnings = FALSE,
            recursive = TRUE
        )
        new_tarname <- file.path(desc[["Path"]], new_tarname)
    }
    new_tarpath <- file.path(src_contrib_dir, new_tarname)
    if (!file.copy(
        tarpath,
        new_tarpath,
        overwrite = TRUE,
        copy.date = TRUE
    )) {
        stop("failure to copy")
    }


    files <- list.files(
        src_contrib_dir,
        paste0(
            "^",
            gsub(".", "\\.", desc[["Package"]], fixed = TRUE),
            "_",
            .standard_regexps()$valid_package_version,
            "\\.tar\\.gz$"
        ),
        recursive = TRUE
    )
    files <- files[!startsWith(files, "Archive/")]
    files <- files[files != new_tarname]
    if (length(files)) {
        src_contrib_Archive_dir <- file.path(src_contrib_dir, "Archive", desc[["Package"]])
        dir.create(src_contrib_Archive_dir, showWarnings = FALSE, recursive = TRUE)
        if (!all(file.rename(
            file.path(src_contrib_dir, files),
            file.path(src_contrib_Archive_dir, files)
        ))) {
            stop("failure to rename")
        }
    }


    new_tarpath
}


#' Build an R Package in a Local Repository
#'
#' @description
#' Build an \R package from a package source in the directory specified by
#' \code{pkgpath} into a local repository.
#'
#' @usage build_tarball_in_repos(pkgpath, repos_dir, Path = NULL, R = NULL, opts = NULL)
#'
#' @param pkgpath,R see \code{build_tarball}.
#' @param repos_dir,Path see \code{copy_tarball_to_repos}.
#' @param opts further options to \command{R CMD build}.
#'
#' @section Value:
#' character string; path of the package \sQuote{tar} archive.
#'
#' \code{<repos_dir>/src/contrib[/<Path>]/<pkgname>_<version>.tar.gz}
#'
#' @export
build_tarball_in_repos <- function (pkgpath, repos_dir, Path = NULL, R = NULL, opts = NULL)
{
    tarpath <- build_tarball(pkgpath, R, opts)
    copy_tarball_to_repos(tarpath, repos_dir, Path)
}


#' Find Tarball in Local Repository
#'
#' @description
#' Get the path of a package \sQuote{tar} archive in a local repository.
#'
#' @usage find_tarball_in_repos(pkgname, repos_dir)
#'
#' @param pkgname character string; name of the \R package to find.
#' @param repos_dir character string; directory of the local repository.
#'
#' @section Value:
#' character string; path of the package \sQuote{tar} archive.
#'
#' \code{<repos_dir>/src/contrib[/<Path>]/<pkgname>_<version>.tar.gz}
#'
#' @export
find_tarball_in_repos <- function (pkgname, repos_dir)
{
    repos_dir <- path.expand(repos_dir)


    src_contrib_dir <- file.path(repos_dir, "src", "contrib")
    info <- read.dcf(file.path(src_contrib_dir, "PACKAGES"), .src_fields)
    i <- match(pkgname, info[, "Package"])
    if (is.na(i)) {
        warning(sprintf("package '%s' does not exist in 'src/contrib/PACKAGES'", pkg))
        return(FALSE)
    }
    pkgname <- info[[i, "Package"]]
    version <- info[[i, "Version"]]
    tarname <- paste0(pkgname, "_", version, ".tar.gz")
    if (!is.na(info[[i, "Path"]]))
        tarname <- file.path(info[[i, "Path"]], tarname)
    tarpath <- file.path(src_contrib_dir, tarname)
    if (!file.exists(tarpath)) {
        warning(sprintf("tarball 'src/contrib/%s' was not found", tarname))
        return(FALSE)
    }


    tarpath
}


#' Build a Binary
#'
#' @description
#' Build a binary of a package \sQuote{tar} archive.
#'
#' @usage build_binary(tarpath, R = NULL, opts = NULL)
#'
#' @param tarpath character string; path of the package \sQuote{tar} archive.
#' @param R an object of class \code{"R"}, or coercible.
#' @param opts further options to \command{R CMD INSTALL --build}.
#'
#' @details
#' Invokes \command{R CMD INSTALL --build} to build the \R package binary.
#'
#' @section Value:
#' A list with at least the following components:
#'
#' \describe{
#'   \item{binpath}{
#'
#'     character string; filename of the resultant binary.}
#'
#'   \item{bin_dir}{
#'
#'     character string; sub path for which the binary should be placed in a
#'     repository.}
#' }
#'
#' @export
build_binary <- function (tarpath, R = NULL, opts = NULL)
{
    tarpath <- path.expand(tarpath)


    R <- as_R(R)
    r_bin <- R$bin
    r_major_minor <- R$major_minor


    if (.Platform$OS.type == "windows") {
        ext <- ".zip"
        platform <- "windows"
    } else if (grepl("^darwin", R.version$os)) {
        ext <- ".tgz"
        platform <- "macosx"
        if (startsWith(.Platform$pkgType, "mac.binary."))
            platform <- paste(platform, substring(.Platform$pkgType, 12L), sep = "/")
    } else stop("binary packages are not available")
    desc <- .read_DESCRIPTION_from_tarball(tarpath)
    binpath <- paste0(desc[["Package"]], "_", desc[["Version"]], ext)
    bin_dir <- file.path("bin", platform, "contrib", r_major_minor)


    opts <- if (is.null(opts))
        .make_R_CMD_INSTALL_opts()
    else if (is.character(opts) && is.null(names(opts)))
        opts
    else do.call(".make_R_CMD_INSTALL_opts", opts, quote = TRUE)
    args <- c(.find_R_CMD(r_bin), "INSTALL", "--build", opts, shQuote(tarpath))
    command <- paste(args, collapse = " ")
    # unloadNamespace("essentials"); unloadNamespace("this.path"); stop("remove this later")
    .system(command, mustWork = TRUE)


    list(binpath = binpath, bin_dir = bin_dir)
}


#' Build a Binary From a Local Repository
#'
#' @description
#' Build a binary of a package \sQuote{tar} archive from a local repository.
#'
#' @usage build_binary_from_repos(pkgname, repos_dir, R = NULL, opts = NULL)
#'
#' @param pkgname character string; name of the \R package.
#' @param repos_dir character string; directory of the local repository.
#' @param R an object of class \code{"R"}, or coercible.
#' @param opts further options to \command{R CMD INSTALL --build}.
#'
#' @section Value:
#' A list with at least the following components:
#'
#' \describe{
#'   \item{binpath}{
#'
#'     character string; filename of the resultant binary.}
#'
#'   \item{bin_dir}{
#'
#'     character string; sub path for which the binary should be placed in a
#'     repository.}
#' }
#'
#' @export
build_binary_from_repos <- function (pkgname, repos_dir, R = NULL, opts = NULL)
{
    tarpath <- find_tarball_in_repos(pkgname, repos_dir)
    build_binary(tarpath, R, opts)
}


#' Copy a Binary Into a Local Repository
#'
#' @description
#' Copy a package binary into a local repository.
#'
#' @usage copy_binary_to_repos(binpath, repos_dir, bin_dir)
#'
#' @param binpath character string; path of the package binary.
#' @param repos_dir character string; directory of the local repository.
#' @param bin_dir character string; sub path for which the binary should be placed.
#'
#' @details
#' \code{bin_dir} would typically be something like:
#'
#' \code{"bin/windows/contrib/4.4"}
#'
#' \code{"bin/macosx/big-sur-arm64/contrib/4.4"}
#'
#' @section Value:
#' character string; path of the copied package binary.
#'
#' \code{<repos_dir>/<bin_dir>/<pkgname>_<version>.[zip|tgz]}
#'
#' @export
copy_binary_to_repos <- function (binpath, repos_dir, bin_dir)
{
    binpath <- path.expand(binpath)
    repos_dir <- path.expand(repos_dir)
    bin_dir <- file.path(repos_dir, bin_dir)


    if (endsWith(binpath, ".zip")) {
        ext <- ".zip"
        desc <- .read_DESCRIPTION_from_zip_archive(binpath, .bin_fields)
    } else if (endsWith(binpath, ".tgz")) {
        ext <- ".tgz"
        desc <- .read_DESCRIPTION_from_tarball(binpath, .bin_fields)
    } else {
        warning("invalid")
        return(FALSE)
    }


    dir.create(bin_dir, showWarnings = FALSE, recursive = TRUE)


    .write_PACKAGES(desc, bin_dir)


    binname <- paste0(
        desc[["Package"]],
        "_",
        desc[["Version"]],
        ext
    )
    to <- file.path(bin_dir, binname)
    if (!file.copy(
        binpath,
        to,
        overwrite = TRUE,
        copy.date = TRUE
    )) {
        stop(sprintf(
            "unable to rename file '%s' to '%s'",
            binpath,
            to
        ))
    }


    files <- list.files(
        bin_dir,
        paste0(
            "^",
            gsub(".", "\\.", desc[["Package"]], fixed = TRUE),
            "_",
            .standard_regexps()$valid_package_version,
            gsub(".", "\\.", ext, fixed = TRUE),
            "$"
        )
    )
    files <- files[files != basename(to)]
    if (length(files))
        file.remove(file.path(bin_dir, files))


    to
}


#' Build a Binary in a Local Repository
#'
#' @description
#' Build a binary of a package \sQuote{tar} archive in a local repository.
#'
#' @usage build_binary_in_repos(pkgname, repos_dir, R = NULL, opts = NULL)
#'
#' @param pkgname character string; name of the \R package.
#' @param repos_dir character string; directory of the local repository.
#' @param R an object of class \code{"R"}, or coercible.
#' @param opts further options to \command{R CMD INSTALL --build}.
#'
#' @section Value:
#' character string; path of the package binary.
#'
#' @export
build_binary_in_repos <- function (pkgname, repos_dir, R = NULL, opts = NULL)
{
    x <- build_binary_from_repos(pkgname, repos_dir, R, opts)
    copy_binary_to_repos(x$binpath, repos_dir, x$bin_dir)
}


#' Class repos Objects
#'
#' @description
#' Create objects of class \code{"repos"}.
#'
#' @usage make_repos(repos_dir)
#'
#' @param repos_dir character string; directory of the local repository.
#'
#' @section Value:
#' An object of class \code{"repos"} with at least the following members:
#'
#' \describe{
#'   \item{repos_dir}{
#'
#'     character string; directory of the local repository.}
#'
#'   \item{copy_tarball}{
#'
#'     function with formals \code{(tarpath, Path = NULL)} that copies a package
#'     \sQuote{tar} archive into the local repository.}
#'
#'   \item{build_tarball}{
#'
#'     function with formals \code{(pkgpath, Path = NULL, R = NULL, opts = NULL)}
#'     that builds an \R package from a package source in the directory specified
#'     by \code{pkgpath} into the local repository.}
#'
#'   \item{find_tarball}{
#'
#'     function with formals \code{(pkgname)} that gets the path of a package
#'     \sQuote{tar} archive in the local repository.}
#'
#'   \item{copy_binary}{
#'
#'     function with formals \code{(binpath, bin_dir)} that copies a package
#'     binary into the local repository.}
#'
#'   \item{build_binary}{
#'
#'     function with formals \code{(pkgname, R = NULL, opts = NULL)} that builds a
#'     binary of a package \sQuote{tar} archive in the local repository.}
#' }
#'
#' @export
make_repos <- function (repos_dir)
{
    repos_dir <- path.expand(repos_dir)
    x <- list(
        repos_dir = repos_dir,
        copy_tarball = function (tarpath, Path = NULL)
copy_tarball_to_repos(tarpath, repos_dir, Path),
        build_tarball = function (pkgpath, Path = NULL, R = NULL, opts = NULL)
build_tarball_in_repos(pkgpath, repos_dir, Path, R, opts),
        find_tarball = function (pkgname)
find_tarball_in_repos(pkgname, repos_dir),
        copy_binary = function (binpath, bin_dir)
copy_binary_to_repos(binpath, repos_dir, bin_dir),
        build_binary = function (pkgname, R = NULL, opts = NULL)
build_binary_in_repos(pkgname, repos_dir, R, opts)
    )
    class(x) <- "repos"
    x
}
