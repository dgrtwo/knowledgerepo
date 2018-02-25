

[![Travis build status](https://travis-ci.org/dgrtwo/knowledgerepo.svg?branch=master)](https://travis-ci.org/dgrtwo/knowledgerepo)

# knowledgerepo

The knowledgerepo package is a wrapper around [AirBnB's Knowledge Repository project](https://github.com/airbnb/knowledge-repo), particularly the command line tools included within the project. Its goal is to make it easy to create and submit knowledge posts from within an R session.

## Installation

Use the [remotes](https://cran.r-project.org/web/packages/remotes/index.html) package to install the development version from GitHub:


```r
remotes::install_github("dgrtwo/knowledgerepo")
```

You'll also need to install the [knowledge_repo](https://github.com/airbnb/knowledge-repo) Python package from your terminal.

```
[sudo] pip install --upgrade knowledge-repo
```

## Usage

The package offers functions, each prefixed with `kr_`, that wrap the `knowledge_base` command line interface. For example, `knowledge_repo init` is replaced with `kr_init`, and `knowledge_repo add` replaced with `kr_add`.


```r
library(knowledgerepo)

# Initialize a repository
repo_dir <- tempfile()
kr_init(repo_dir)

# Create a test post
test_file <- tempfile(fileext = ".Rmd")
kr_create(test_file)

# Create a test post
kr_add(test_file, path = "examples/test_post", repo = repo_dir)
```

Once you've added a remote GitHub repository, you can also submit the post for review.


```r
# Submit it for review
kr_submit("examples/test_post", repo = repo)
```

## Code of Conduct

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md).
By participating in this project you agree to abide by its terms.
