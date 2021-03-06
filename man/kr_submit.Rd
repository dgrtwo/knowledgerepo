% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/commands.R
\name{kr_submit}
\alias{kr_submit}
\title{Submit a post to the knowledge base for review}
\usage{
kr_submit(path, repo = Sys.getenv("KNOWLEDGE_REPO"), browse_pr = FALSE,
  master = FALSE, ...)
}
\arguments{
\item{path}{The path of the knowledge post to submit for review.}

\item{repo}{Repository of the knowledge post to add}

\item{browse_pr}{Whether to browse to a GitHub pull request for
submitting this post}

\item{master}{Whether to submit it on master rather than on a
separate branch (allowing for review). If TRUE, rather than using
the knowlege_base CLI it locally merges to master than pushes.
Works only if you have commit access to master.}

\item{...}{Global arguments to \code{knowledge_repo} commands, such as
\code{repo} or \code{noupdate} (see Details)}
}
\description{
Submit a post to the knowledge base for review. This would be done
after adding it with \code{\link{kr_add}}
(assuming submit was FALSE in that command).
}
\details{
The "direct" option is not supported by the \code{knowledge_repo}
command line interface, and is a shortcut provided by this package.

Global arguments that can be passed to all knowledge_repo
commands include:

\itemize{
  \item{repo}{Path to Knowledge Repository to add to. Can also set
  \code{KNOWLEDGE_REPO} variable in \code{.Renviron}.}
  \item{dev}{Whether to skip passing control to version of code
  checked out in knowledge repository.}
  \item{debug}{Whether to enable debug mode.}
  \item{noupdate}{Whether script should update the repository before
  performing actions.}
  \item{version}{Show version and exit.}
  \item{help}{Show help and exit}
}
}
\examples{

# set up a repository and post
repo <- tempfile()
kr_init(repo)
kr_create("test.Rmd", repo = repo)

# add to knowledge repo
kr_add("test.Rmd", repo = repo, path = "tests/test")

\dontrun{
# submit to remote repository
kr_submit("tests/test")
}

}
