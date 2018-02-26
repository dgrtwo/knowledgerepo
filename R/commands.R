# This file wraps commands from the Knowledge Repo CLI

#' Create a new post from a default template
#'
#' Creates a new post from the built in template, using either
#' Rmd (default), IPython notebook, or Markdown
#'
#' @param filename Where this file should be created.
#' @param format The format of the knowledge post to be created:
#' either Rmd, ipynb, or md. By default, retrieved from the extension
#' of the filename.
#' @param template Optionally, a template to create the knowledge post from.
#'
#' @template extra-args
#'
#' @examples
#'
#' # set up a repository and post
#' repo <- tempfile()
#' kr_init(repo)
#' kr_create("test.Rmd", repo = repo)
#'
#' @export
kr_create <- function(filename, format = NULL, template = NULL, ...) {
  if (is.null(format)) {
    format <- tools::file_ext(filename)
    # to be case insensitive
    format <- switch(tolower(format),
      rmd = "Rmd",
      ipynb = "ipynb",
      md = "md",
      stop("Extension '", format, "' not recognized"))
  }

  kr_command(...,
             "create",
             template = template,
             format,
             filename)
}


#' Add a post to a knowledge repository
#'
#' Add a local file to a knowledge repository. Unless submit = TRUE, this doesn't
#' (yet) submit the post.
#'
#' @param filename Filename to add
#' @param path The path of the destination post to be added in the
#' knowledge repo. Required if the knowledge post does
#' not specify "path" in its headers.
#' @param update Whether this should update an existing post of the
#' same name.
#' @param branch The branch to use for this addition, if not the
#' default (which is the path of the knowledge post).
#' @param squash Automatically suppress all previous commits, and
#' replace it with this version.
#' @param submit Submit newly added post.
#' @param message Commit message. By default, will use 'Adding post: [title]'
#' @param src Specify additional source files to add to
#' \code{<knowledge_post>/orig_src.}
#' @param browse_pr If \code{submit}, whether to browse to a GitHub pull request for
#' submitting this post.
#' @param repo Repository of the knowledge post to add
#'
#' @template extra-args
#'
#' @examples
#'
#' # set up a repository and post
#' repo <- tempfile()
#' kr_init(repo)
#' kr_create("test.Rmd", repo = repo)
#'
#' # add to knowledge repo
#' kr_add("test.Rmd", repo = repo, path = "tests/test")
#'
#' # custom commit message
#' kr_add("test.Rmd", repo = repo, path = "tests/test2", message = "Committing a new post")
#'
#' @export
kr_add <- function(filename,
                   path = NULL,
                   update = FALSE,
                   branch = NULL,
                   squash = FALSE,
                   submit = FALSE,
                   message = NULL,
                   src = NULL,
                   browse_pr = FALSE,
                   ...,
                   repo = Sys.getenv("KNOWLEDGE_REPO")) {
  if (is.null(message)) {
    title <- rmarkdown::yaml_front_matter(filename)$title
    if (is.null(title)) {
      warning("Can't find title in YAML header of ", title)
    }
    message <- paste0("Adding post: ", title)
  }

  if (repo == "") {
    stop("No repository specified and no KNOWLEDGE_REPO environment variable")
  }

  kr_command(repo = repo,
             ...,
             "add",
             path = path,
             update = update,
             branch = branch,
             squash = squash,
             submit = submit,
             message = message,
             src = src,
             filename)

  # may need to add reminder
  if (submit) {
    if (is.null(path)) {
      path <- rmarkdown::yaml_front_matter(filename)$path
    }

    print(repo)
    print(path)

    after_submit(repo, path, browse_pr = browse_pr)
  }
}


#' Initialize a new knowledge repository
#'
#' @param repo Folder of the repository to create
#' @param tooling_embed Embed a reference version knowledge_repo tooling in
#' the repository.
#' @param tooling_repo The repository to use (if not the default).
#' @param tooling_branch The branch to use when embedding the tools as a
#' submodule (default is "master").
#' @template extra-args
#'
#' @examples
#'
#' repo <- tempfile()
#' kr_init(repo)
#'
#' @export
kr_init <- function(repo,
                    tooling_embed = FALSE,
                    tooling_repo = NULL,
                    tooling_branch = NULL,
                    ...) {
  kr_command(repo = repo,
             ...,
             "init",
             tooling_embed = tooling_embed,
             tooling_repo = tooling_repo,
             tooling_branch = tooling_branch)
}


#' Submit a post to the knowledge base for review
#'
#' Submit a post to the knowledge base for review. This would be done
#' after adding it with \code{\link{kr_add}}
#' (assuming submit was FALSE in that command).
#'
#' @param path The path of the knowledge post to submit for review.
#' @param repo Repository of the knowledge post to add
#' @param browse_pr Whether to browse to a GitHub pull request for
#' submitting this post
#' @param master Whether to submit it on master rather than on a
#' separate branch (allowing for review). If TRUE, rather than using
#' the knowlege_base CLI it locally merges to master than pushes.
#' Works only if you have commit access to master.
#'
#' @details The "direct" option is not supported by the \code{knowledge_repo}
#' command line interface, and is a shortcut provided by this package.
#'
#' @template extra-args
#'
#' @examples
#'
#' # set up a repository and post
#' repo <- tempfile()
#' kr_init(repo)
#' kr_create("test.Rmd", repo = repo)
#'
#' # add to knowledge repo
#' kr_add("test.Rmd", repo = repo, path = "tests/test")
#'
#' \dontrun{
#' # submit to remote repository
#' kr_submit("tests/test")
#' }
#'
#' @export
kr_submit <- function(path,
                      repo = Sys.getenv("KNOWLEDGE_REPO"),
                      browse_pr = FALSE,
                      master = FALSE,
                      ...) {
  if (repo == "") {
    stop("No repository specified and no KNOWLEDGE_REPO environment variable")
  }

  if (master) {
    # Push directly to master branch rather than a PR
    # Note that this isn't generally supported by knowledge_repo:
    # we'll have to see how it works in practice
    message("Merging ", path, " to master and pushing")
    r <- git2r::init(repo)
    git2r::checkout(r, "master")
    git2r::merge(r, paste0(path, ".kp"))
    git2r::push(r)
    return()
  }

  kr_command(..., repo = repo, "submit", path)

  after_submit(repo, path, browse_pr = browse_pr)
}


#' Show status of knowledge repo
#'
#' @param ... Global arguments to \code{knowledge_repo} commands, such as
#' \code{repo} or \code{noupdate}
#'
#' @export
kr_status <- function(...) {
  kr_command(..., "status")
}


#' Show status of knowledge repo
#'
#' @template extra-args
#'
#' @export
kr_runserver <- function(...) {
  kr_command(..., "status")
}


#' Preview a post locally
#'
#' @param path The path of the knowledge post to preview.
#' @param port Specify the port on which to run the web server.
#' @param dburi The SQLAlchemy database uri.
#' @param config Configuration file
#'
#' @template extra-args
#'
#' @export
kr_preview <- function(path, port = NULL, dburi = NULL, config = NULL, ...) {
  kr_command(...,
             "preview",
             path,
             port = port,
             dburi = dburi,
             config = config)
}


#' Deploy a local server
#'
#' @param port Specify the port on which to run the web server.
#' @param dburi The SQLAlchemy database uri.
#' @param workers Number of gunicorn worker threads to spin up.
#' @param timeout Specify the timeout (seconds) for the gunicorn web
#' server.
#' @param config The config file from which to read server configuration.
#' @param engine Which server engine to use when deploying; choose
#' from: "flask", "gunicorn" (default) or "uwsgi".
#'
#' @template extra-args
#'
#' @export
kr_deploy <- function(port = NULL,
                      dburi = NULL,
                      workers = NULL,
                      timeout = NULL,
                      config = NULL,
                      engine = NULL,
                      ...) {
  kr_command(...,
             "deploy",
             port = port,
             dburi = dburi,
             workers = workers,
             timeout = timeout,
             config = config,
             engine = engine)
}


#' Run a keybase command on the command line
#'
#' Run a keybase command with the given arguments. Positional
#' arguments are provided in order, while named arguments are prefixed
#' with \code{--name}. Arguments passed as TRUE are empty (e.g.
#' \code{help = TRUE} becomes \code{--help}), while FALSE and NULL are
#' dropped.
#'
#' @param ... Arguments to construct a command, either unnamed (positional)
#' or named
#' @param .verbose Whether to display the output as a message
#'
#' @examples
#'
#' kr_command("create", "Rmd", "test.Rmd")
#'
#' @export
kr_command <- function(..., .verbose = TRUE) {
  # remove NULLs and FALSEs, turn TRUE to ""
  args <- list(...)
  args <- purrr::keep(purrr::compact(args), ~ !identical(., FALSE))
  args <- purrr::modify_if(args, ~ identical(., TRUE), ~ "")
  args <- purrr::map(args, shQuote)

  n <- names(args)
  n <- gsub("_", "-", n)
  n[n != ""] <- paste0("--", n, " ")[n != ""]

  with_names <- as.list(c("knowledge_repo", paste0(n, unlist(args))))

  cmd <- do.call(paste, with_names)

  if (.verbose) {
    message("Running '", cmd, "'")
  }
  system(cmd)
}
