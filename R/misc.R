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

# check for rstan package -------------------------------------------------
check_rstan <- function() {
  ok <- requireNamespace("rstan", quietly = TRUE)
  msg <- "Please install the RStan package to use this option."
  if (!ok) stop(msg)
}

# run rstan  --------------------------------------------------------------
run_rstan <- function(file, ...) {
  # @param file .stan or .txt file to use
  # @param ... other arguments to pass to rstan::stan (e.g. iter, chains, etc)
  # @return stanfit object

  if (missing(file)) stop("'file' must be specified")
  check_rstan()
  rstan::stan(file = file, ...)
}
