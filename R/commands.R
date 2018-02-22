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
#' kr_create("example.Rmd")
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
#' Add a post to a knowledge repository
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
#' @param submit Submit newly added post
#' @param message Commit message
#' @param src Specify additional source files to add to
#' \code{<knowledge_post>/orig_src.}
#'
#' @template extra-args
#'
#' @examples
#'
#' kr_add("test.Rmd")
#' kr_add("test.Rmd", message = "Committing a new post")
#' kr_add("test.Rmd", submit = TRUE)
#'
#' @export
kr_add <- function(filename,
                   path = NULL,
                   update = FALSE,
                   branch = NULL,
                   squash = FALSE,
                   submit = FALSE,
                   message = paste0("Adding ", filename),
                   src = NULL,
                   ...) {
  message <- paste0("'", gsub("'", "\\'", message), "'")

  kr_command(...,
             "add",
             path = path,
             update = update,
             branch = branch,
             squash = squash,
             submit = submit,
             message = message,
             src = src,
             filename)
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
#' kr_init("example_repository")
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
#' after adding it (assuming submit was FALSE in that command).
#'
#' @param path The path of the knowledge post to submit for review.
#'
#' @template extra-args
#'
#' @examples
#'
#' kr_create("test.Rmd")
#' kr_add("test.Rmd", path = "examples/test")
#' kr_submit("examples/test")
#'
#' @export
kr_submit <- function(path, ...) {
  kr_command(..., "submit", path)
}


#' Show status of knowledge repo
#'
#' @param ... Global arguments to \code{knowledge_repo} commands, such as
#' \code{repo} or \code{noupdate}
#'
#' kr_status()
#'
#' @export
kr_status <- function(...) {
  kr_command(..., "status")
}


#' Show status of knowledge repo
#'
#' @template extra-args
#'
#' @examples
#'
#' kr_runserver()
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
#' @examples
#'
#' kr_deploy()
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
#' with \code{--name}. To make passing arguments easier, commit messages
#'
#' @param ... Arguments to construct a command, either unnamed (positional)
#' or named
#' @param .verbose Whether to display the output as a message
#'
#' @template extra-args
#'
#' @examples
#'
#' kr_command("create", "Rmd", "test.Rmd")
#' kr_command("add", help = TRUE)
#'
#' @export
kr_command <- function(..., .verbose = TRUE) {
  # remove NULLs and FALSEs, turn TRUE to ""
  args <- list(...)
  args <- purrr::keep(purrr::compact(args), ~ !identical(., FALSE))
  args <- purrr::modify_if(args, ~ identical(., TRUE), ~ "")

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

