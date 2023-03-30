pkgname <- "this.path"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
base::assign(".ExTimings", "this.path-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('this.path')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("Args")
### * Args

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: Args
### Title: Providing Arguments to a Script
### Aliases: Args asArgs fileArgs progArgs withArgs

### ** Examples

this.path::asArgs(NULL, c(TRUE, FALSE, NA), 1:5, pi, exp(6i),
    letters[1:5], as.raw(0:4), Sys.Date(), Sys.time(),
    list(list(list("lists are recursed"))))


FILE <- tempfile(fileext = ".R")
this.path:::write.code({
    this.path:::withAutoprint({
        this.path::this.path()
        this.path::fileArgs()
        this.path::progArgs()
    }, spaced = TRUE, verbose = FALSE, width.cutoff = 60L)
}, FILE)


# wrap your source call with a call to withArgs()
this.path::withArgs(
    source(FILE, local = TRUE, verbose = FALSE),
    letters[6:10], pi, exp(1)
)
this.path::withArgs(
    sys.source(FILE, environment()),
    letters[11:15], pi + 1i * exp(1)
)
this.path:::.Rscript(c("--default-packages=NULL", "--vanilla", FILE,
    this.path::asArgs(letters[16:20], pi, Sys.time())))
# fileArgs() will be character(0) because there is no executing script
this.path:::.Rscript(c("--default-packages=NULL", "--vanilla",
    rbind("-e", readLines(FILE)[-2L]),
    this.path::asArgs(letters[16:20], pi, Sys.time())))


# with R >= 4.1.0, use the forward pipe operator '|>' to
# make calls to withArgs() more intuitive:
# source(FILE, local = TRUE, verbose = FALSE) |> this.path::withArgs(
#     letters[6:10], pi, exp(1))
# sys.source(FILE, environment()) |> this.path::withArgs(
#     letters[11:15], pi + 1i * exp(1))


# withArgs() also works with inside.source() and wrap.source()
sourcelike <- function (file, envir = parent.frame())
{
    file <- inside.source(file)
    envir <- as.environment(envir)
    exprs <- parse(n = -1, file = file)
    for (i in seq_along(exprs)) eval(exprs[i], envir)
}
this.path::withArgs(sourcelike(FILE), letters[21:26])


sourcelike2 <- function (file, envir = parent.frame())
{
    envir <- as.environment(envir)
    exprs <- parse(n = -1, file = file)
    for (i in seq_along(exprs)) eval(exprs[i], envir)
}
sourcelike3 <- function (file, envir = parent.frame())
{
    envir <- as.environment(envir)
    wrap.source(sourcelike2(file = file, envir = envir))
}
this.path::withArgs(sourcelike3(FILE), LETTERS[1:5])
this.path::withArgs(wrap.source(sourcelike2(FILE)), LETTERS[6:10])
## Don't show: 
unlink(FILE)
## End(Don't show)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("Args", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("LINENO")
### * LINENO

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: LINENO
### Title: Line Number of Executing Script
### Aliases: LINENO

### ** Examples

FILE <- tempfile(fileext = ".R")
writeLines(c(
    "LINENO()",
    "LINENO()",
    "# LINENO() respects #line directives",
    "#line 1218",
    "LINENO()"
), FILE)


# previously used:
#
# ```
# source(FILE, echo = TRUE, verbose = FALSE,
#     max.deparse.length = Inf, keep.source = TRUE)
# ```
#
# but it echoes incorrectly with #line directives.
# `source2()` echoes correctly!
this.path:::source2(FILE, echo = TRUE, verbose = FALSE,
    max.deparse.length = Inf, keep.source = TRUE)
## Don't show: 
unlink(FILE)
## End(Don't show)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("LINENO", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("Sys.putenv")
### * Sys.putenv

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: Sys.putenv
### Title: Set Environment Variables
### Aliases: Sys.putenv

### ** Examples

Sys.putenv(c("R_TEST=testit", "A+C=123"))
Sys.getenv("R_TEST")
Sys.unsetenv("R_TEST") # on Unix-alike may warn and not succeed
Sys.getenv("R_TEST", unset = NA)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("Sys.putenv", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("basename2")
### * basename2

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: basename2
### Title: Manipulate File Paths
### Aliases: basename2 dirname2

### ** Examples

path <- c("/usr/lib", "/usr/", "usr", "/", ".", "..")
x <- cbind(path, dirname = dirname2(path), basename = basename2(path))
print(x, quote = FALSE, print.gap = 3)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("basename2", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("check.path")
### * check.path

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: check.path
### Title: Check 'this.path()' is Functioning Correctly
### Aliases: check.path check.dir check.proj

### ** Examples

# I have a project called 'EOAdjusted'
#
# Within this project, I have a folder called 'code'
# where I place all of my scripts.
#
# One of these scripts is called 'provrun.R'
#
# So, at the top of that R script, I could write:


# this.path::check.path("EOAdjusted", "code", "provrun.R")
#
# or
#
# this.path::check.path("EOAdjusted/code/provrun.R")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("check.path", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ext")
### * ext

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ext
### Title: File Extensions
### Aliases: splitext removeext ext ext<-

### ** Examples

splitext(character(0))
splitext("")

splitext("file.ext")

path <- c("file.tar.gz", "file.tar.bz2", "file.tar.xz")
splitext(path, compression = FALSE)
splitext(path, compression = TRUE)

path <- "this.path_1.0.0.tar.gz"
ext(path) <- ".png"
path

path <- "this.path_1.0.0.tar.gz"
ext(path, compression = TRUE) <- ".png"
path



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ext", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("from.shell")
### * from.shell

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: from.shell
### Title: Top-Level Code Environment
### Aliases: from.shell is.main

### ** Examples

FILES <- tempfile(c("file1_", "file2_"), fileext = ".R")
this.path:::write.code({
    from.shell()
    is.main()
}, FILES[2])
this.path:::write.code((
    bquote(this.path:::withAutoprint({
        from.shell()
        is.main()
        source(.(FILES[2]), echo = TRUE, verbose = FALSE,
            prompt.echo = "file2> ", continue.echo = "file2+ ")
    }, spaced = TRUE, verbose = FALSE, width.cutoff = 60L,
       prompt.echo = "file1> ", continue.echo = "file1+ "))
), FILES[1])


this.path:::.Rscript(c("--default-packages=this.path",
                       "--vanilla", FILES[1]))


this.path:::.Rscript(c("--default-packages=this.path", "--vanilla",
    "-e", "cat(\"\n> from.shell()\\n\")",
    "-e", "from.shell()",
    "-e", "cat(\"\n> is.main()\\n\")",
    "-e", "is.main()",
    "-e", "cat(\"\n> source(commandArgs(TRUE)[[1L]])\\n\")",
    "-e", "source(commandArgs(TRUE)[[1L]])",
    FILES[1]))
## Don't show: 
unlink(FILES)
## End(Don't show)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("from.shell", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("getinitwd")
### * getinitwd

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: getinitwd
### Title: Get Initial Working Directory
### Aliases: getinitwd initwd

### ** Examples

cat("\ninitial working directory:\n"); getinitwd()
cat("\ncurrent working directory:\n"); getwd()



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("getinitwd", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("here")
### * here

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: here
### Title: Construct Path to File, Beginning with 'this.dir()'
### Aliases: here ici

### ** Examples

FILE <- tempfile(fileext = ".R")
this.path:::write.code({


    this.path::here()
    this.path::here(.. = 1)
    this.path::here(.. = 2)


    # use 'here' to read input from a file located nearby
    this.path::here(.. = 1, "input", "file1.csv")


    # or maybe to run another script
    this.path::here("script2.R")


}, FILE)


source(FILE, echo = TRUE, verbose = FALSE)
## Don't show: 
unlink(FILE)
## End(Don't show)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("here", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("path.join")
### * path.join

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: path.join
### Title: Construct Path to File
### Aliases: path.join

### ** Examples

path.join("C:", "test1")

path.join("C:/", "test1")

path.join("C:/path/to/file1", "/path/to/file2")

path.join("//host-name/share-name/path/to/file1", "/path/to/file2")

path.join("C:testing", "C:/testing", "~", "~/testing", "//host",
    "//host/share", "//host/share/path/to/file", "not-an-abs-path")

path.join("c:/test1", "c:test2", "C:test3")

path.join("test1", "c:/test2", "test3", "//host/share/test4", "test5",
    "c:/test6", "test7", "c:test8", "test9")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("path.join", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("path.split")
### * path.split

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: path.split
### Title: Split File Path Into Individual Components
### Aliases: path.split path.split.1 path.unsplit

### ** Examples

path <- c(
    NA,
    "",
    paste0("https://raw.githubusercontent.com/ArcadeAntics/PACKAGES/",
           "src/contrib/Archive/this.path/this.path_1.0.0.tar.gz"),
    "\\\\host\\share\\path\\to\\file",
    "\\\\host\\share\\",
    "\\\\host\\share",
    "C:\\path\\to\\file",
    "C:path\\to\\file",
    "path\\to\\file",
    "\\path\\to\\file",
    "~\\path\\to\\file",
    # paths with character encodings
    `Encoding<-`("path/to/fil\xe9", "latin1"),
    "C:/Users/iris/Documents/\u03b4.R"
)
print(x <- path.split(path))
print(path.unsplit(x))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("path.split", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("relpath")
### * relpath

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: relpath
### Title: Make a Path Relative to Another Path
### Aliases: relpath rel2here

### ** Examples

## Not run: 
##D relpath(
##D     c(
##D         # paths which are equivalent will return "."
##D         "C:/Users/effective_user/Documents/this.path/man",
##D 
##D 
##D         # paths which have no base in common return as themselves
##D         paste0("https://raw.githubusercontent.com/ArcadeAntics/",
##D                "this.path/main/tests/this.path_w_URLs.R"),
##D         "D:/",
##D         "//host-name/share-name/path/to/file",
##D 
##D 
##D         "C:/Users/effective_user/Documents/testing",
##D         "C:\\Users\\effective_user",
##D         "C:/Users/effective_user/Documents/R/this.path.R"
##D     ),
##D     relative.to = "C:/Users/effective_user/Documents/this.path/man"
##D )
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("relpath", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("set.this.path.jupyter")
### * set.this.path.jupyter

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: set.this.path.jupyter
### Title: Declare Executing 'Jupyter' Notebook's Filename
### Aliases: set.this.path.jupyter

### ** Examples

# if you opened the file "~/file50b816a24ec1.ipynb", the initial
# working directory should be "~". You can write:
#
# set.this.path.jupyter("file50b816a24ec1.ipynb")
#
# and then this.path() will return "~/file50b816a24ec1.ipynb"



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("set.this.path.jupyter", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("shFILE")
### * shFILE

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: shFILE
### Title: Get Argument 'FILE' Provided to R by a Shell
### Aliases: shFILE

### ** Examples

FILE <- tempfile(fileext = ".R")
this.path:::write.code({
    this.path:::withAutoprint({
        shFILE(original = TRUE)
        shFILE()
        shFILE(default = {
            stop("since 'FILE' will be found, argument 'default'\n",
                " will not be evaluated, so this error will not be\n",
                " thrown! you can use this to your advantage in a\n",
                " similar manner, doing arbitrary things only if\n",
                " 'FILE' is not found")
        })
    }, spaced = TRUE, verbose = FALSE, width.cutoff = 60L)
}, FILE)
this.path:::.Rscript(c("--default-packages=this.path",
                       "--vanilla", FILE))


for (expr in c("shFILE(original = TRUE)",
               "shFILE(original = TRUE, default = NULL)",
               "shFILE()",
               "shFILE(default = NULL)"))
{
    cat("\n\n")
    this.path:::.Rscript(c(
        "--default-packages=this.path", "--vanilla", "-e", expr))
}
## Don't show: 
unlink(FILE)
## End(Don't show)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("shFILE", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("this.path")
### * this.path

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: this.path
### Title: Determine Executing Script's Filename
### Aliases: this.path this.dir Sys.path Sys.dir

### ** Examples

FILE <- tempfile(fileext = ".R")
this.path:::write.code({
    this.path:::withAutoprint({
        cat(sQuote(this.path::this.path(verbose = TRUE, default = {
            stop("since the executing script's path will be found,\n",
                " argument 'default' will not be evaluated, so this\n",
                " error will not be thrown! you can use this to\n",
                " your advantage in a similar manner, doing\n",
                " arbitrary things only if the executing script\n",
                " does not exist")
        })), "\n\n")
    }, spaced = TRUE, verbose = FALSE, width.cutoff = 60L,
        prompt.echo = "FILE> ", continue.echo = "FILE+ ")
}, FILE)


source(FILE, verbose = FALSE)
sys.source(FILE, envir = environment())
if (.Platform$GUI == "RStudio")
    get("debugSource", "tools:rstudio", inherits = FALSE)(FILE)
if (requireNamespace("testthat"))
    testthat::source_file(FILE, chdir = FALSE, wrap = FALSE)
if (requireNamespace("knitr")) {
    FILE2 <- tempfile(fileext = ".Rmd")
    FILE3 <- tempfile(fileext = ".md")
    writeLines(c(
        "```{r}",
        # same expression as above
        deparse(parse(FILE)[[c(1L, 2L, 2L)]]),
        "```"
    ), FILE2)
    knitr::knit(input = FILE2, output = FILE3, quiet = TRUE)
    this.path:::cat.file(FILE2, number.nonblank = TRUE,
        squeeze.blank = TRUE, show.tabs = TRUE,
        print.command = TRUE)
    this.path:::cat.file(FILE3, number.nonblank = TRUE,
        squeeze.blank = TRUE, show.tabs = TRUE,
        print.command = TRUE)
    unlink(c(FILE3, FILE2))
}
if (requireNamespace("box")) {
    FILE4 <- tempfile(fileext = ".R")
    this.path:::write.code(bquote({
        this.path:::withAutoprint({
            # we have to use box::set_script_path() because {box}
            # does not allow us to import a module by its path
            script_path <- box::script_path()
            on.exit(box::set_script_path(script_path))
            box::set_script_path(.(normalizePath(FILE, "/")))
            box::use(./.(as.symbol(this.path::removeext(
                this.path::basename2(FILE)
            ))))
            box::unload(.(as.symbol(this.path::removeext(
                this.path::basename2(FILE)
            ))))
        }, spaced = TRUE, verbose = FALSE, width.cutoff = 60L,
            prompt.echo = "FILE4> ", continue.echo = "FILE4+ ")
    }), FILE4)
    source(FILE4, verbose = FALSE)
    unlink(FILE4)
}


this.path:::.Rscript(c("--default-packages=NULL", "--vanilla", FILE))


# this.path also works when source-ing a URL
# (included tryCatch in case an internet connection is not available)
tryCatch({
    source(paste0("https://raw.githubusercontent.com/ArcadeAntics/",
                  "this.path/main/tests/this.path_w_URLs.R"))
}, condition = this.path:::cat.condition)


for (expr in c("this.path()",
               "this.path(default = NULL)",
               "this.dir()",
               "this.dir(default = NULL)",
               "this.dir(default = getwd())"))
{
    cat("\n\n")
    suppressWarnings(this.path:::.Rscript(
        c("--default-packages=this.path", "--vanilla", "-e", expr)
    ))
}


# an example from R package 'logr'
this.path::this.path(verbose = FALSE, default = "script.log",
    else. = function(path) {
        # replace extension (probably .R) with .log
        this.path::ext(path) <- ".log"
        path
        # or you could use paste0(this.path::removeext(path), ".log")
    })
## Don't show: 
unlink(FILE)
## End(Don't show)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("this.path", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("tryCatch2")
### * tryCatch2

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: tryCatch2
### Title: Condition Handling and Recovery
### Aliases: tryCatch2

### ** Examples

FILES <- tempfile(c("existent-file_", "non-existent-file_"))
writeLines("line1\nline2", FILES[[1L]])
for (FILE in FILES) {
    con <- file(FILE)
    tryCatch2({
        open(con, "r")
    }, condition = function(cond) {
        cat("cannot open", FILE, "\n")
    }, else. = {
        cat(FILE, "has", length(readLines(con)), "lines\n")
    }, finally = {
        close(con)
    })
}
unlink(FILES)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("tryCatch2", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("wrap.source")
### * wrap.source

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: wrap.source
### Title: Implement 'this.path()' For Arbitrary 'source()'-Like Functions
### Aliases: wrap.source inside.source set.this.path unset.this.path

### ** Examples

FILE <- tempfile(fileext = ".R")
this.path:::write.code({
    this.path::this.path(verbose = TRUE)
}, FILE)


# here we have a source-like function, suppose this
# function is in a package for which you have write permission
sourcelike <- function (file, envir = parent.frame())
{
    file <- inside.source(file)
    envir <- as.environment(envir)
    exprs <- parse(n = -1, file = file)
    # this prints nicely
    this.path:::withAutoprint(exprs = exprs, evaluated = TRUE,
        local = envir, spaced = TRUE, verbose = FALSE,
        width.cutoff = 60L)
    # you could alternatively do:
    # 'for (i in seq_along(exprs)) eval(exprs[i], envir)'
    # which does no pretty printing
}


sourcelike(FILE)
sourcelike(con <- file(FILE)); close(con)


# here we have another source-like function, suppose this function
# is in a foreign package for which you do not have write permission
sourcelike2 <- function (pathname, envir = globalenv())
{
    if (!(is.character(pathname) && file.exists(pathname)))
        stop(gettextf("'%s' is not an existing file",
             pathname, domain = "R-base"))
    envir <- as.environment(envir)
    exprs <- parse(n = -1, file = pathname)
    this.path:::withAutoprint(exprs = exprs, evaluated = TRUE,
        local = envir, spaced = TRUE, verbose = FALSE,
        width.cutoff = 60L)
}


# the above function is similar to sys.source(), and it
# expects a character string referring to an existing file
#
# with the following, you should be able
# to use 'this.path()' within 'FILE':
wrap.source(sourcelike2(FILE), path.only = TRUE)


# with R >= 4.1.0, use the forward pipe operator '|>' to
# make calls to 'wrap.source' more intuitive:
# sourcelike2(FILE) |> wrap.source(path.only = TRUE)


# 'wrap.source' can recognize arguments by name, so they
# do not need to appear in the same order as the formals
wrap.source(sourcelike2(envir = new.env(), pathname = FILE),
    path.only = TRUE)


# it it much easier to define a new function to do this
sourcelike3 <- function (...)
wrap.source(sourcelike2(...), path.only = TRUE)


# the same as before
sourcelike3(FILE)


# however, this is preferable:
sourcelike4 <- function (pathname, ...)
{
    # pathname is now normalized
    pathname <- inside.source(pathname, path.only = TRUE)
    sourcelike2(pathname = pathname, ...)
}
sourcelike4(FILE)


# perhaps you wish to run several scripts in the same function
fun <- function (paths, ...)
{
    for (pathname in paths) {
        pathname <- set.this.path(pathname, path.only = TRUE)
        sourcelike2(pathname = pathname, ...)
        unset.this.path(pathname)
    }
}
## Don't show: 
unlink(FILE)
## End(Don't show)


base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("wrap.source", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
