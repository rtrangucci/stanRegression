.onAttach <- function(...) {
  ver <- utils::packageVersion("stanRegression")
  msg <- paste("stanRegression version", ver)
  packageStartupMessage(msg)
}
