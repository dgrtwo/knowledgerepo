#' @param ... Global arguments to \code{knowledge_repo} commands, such as
#' \code{repo} or \code{noupdate} (see Details)
#'
#' @details Global arguments that can be passed to all knowledge_repo
#' commands include:
#'
#' \itemize{
#'   \item{repo}{Path to Knowledge Repository to add to. Can also set
#'   \code{KNOWLEDGE_REPO} variable in \code{.Renviron}.}
#'   \item{dev}{Whether to skip passing control to version of code
#'   checked out in knowledge repository.}
#'   \item{debug}{Whether to enable debug mode.}
#'   \item{noupdate}{Whether script should update the repository before
#'   performing actions.}
#'   \item{version}{Show version and exit.}
#'   \item{help}{Show help and exit}
#' }
