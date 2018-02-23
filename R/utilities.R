#' Retrieve the remote link of a GitHub repository
#'
#' This is useful for linking to a remote pull request
#'
#' @param path Path to a local git repository
get_remote_link <- function(path) {
  if (!requireNamespace("git2r")) {
    stop("Working with remote GitHub repos requires the git2r package")
  }

  remote_link <- git2r::remote_url(git2r::init(path))[1]

  if (length(remote_link) == 0) {
    return(NULL)
  }

  if (!stringr::str_detect(remote_link, "github.com.*git")) {
    stop("Does not appear to be a github remote: ", remote_link)
  }

  repo_name <- stringr::str_match(remote_link, ":(.*).git")[2]
  paste0("https://github.com/", repo_name)
}


#' Provide a message with context about a path variable, after submitting
#'
#' @param repo Local path to repo
#' @param path Path of knowledge post
#' @param browse_pr Whether to browse to a pull request
after_submit <- function(repo, path, browse_pr = FALSE) {
  remote_link <- get_remote_link(repo)

  if (is.null(remote_link)) {
    stop("Appears to have no remote repository; cannot submit")
  }

  pr_url <- paste0(remote_link, "/compare/", path, ".kp?expand=1")

  message("You've pushed the post to the ", path, " branch, ",
          "you can now submit a PR for review at ",
          pr_url)

  if (browse_pr) {
    utils::browseURL(pr_url)
  }
}
