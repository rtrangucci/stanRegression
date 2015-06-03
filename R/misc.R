# utility function to convert alist() construction with <- tagged linear model to regular ~ variety
flist_untag <- function(flist) {
  for (i in 1:length(flist)) {
    if (class(flist[[i]]) == "<-") {
      # tagged formula, so convert to ~ formula expression
      flist[[i]][[1]] <- as.name("~")
    }
    # eval required to convert from class language to class formula
    flist[[i]] <- eval(flist[[i]])
  }
  as.list(flist)
}


# check for .stan file extension ------------------------------------------
check_file_name <- function(x) {
  nc <- nchar(x)
  ok <- substr(x, nc-4, nc) == ".stan"
  if (!ok) stop("Generated stan code should go into a file with .stan suffix")
}

# run rstan  --------------------------------------------------------------
run_rstan <- function(file, ...) {
  # @param file .stan or .txt file to use
  # @param ... other arguments to pass to rstan::stan (e.g. iter, chains, etc)
  # @return stanfit object

  rstan_ok <- requireNamespace("rstan", quietly = TRUE)
  if (!rstan_ok) stop("Please install the RStan package to use this option.")
  if (missing(file)) stop("'file' must be specified")
  require("rstan")
  rstan::stan(file = file, ...)
}


# is.stanfit --------------------------------------------------------------
is.stanfit <- function(x) inherits(x, "stanfit")

