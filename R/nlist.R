#' Named lists
#'
#' Creates named lists by using specified names or, if names are omitted, using
#' the names of the objects in the list.
#' The code \code{list(a = a, b = b)}
#' becomes \code{nlist(a,b)} and \code{list(a = a, b = 2)}
#' becomes \code{nlist(a, b = 2)}, etc.
#'
#' @param ... Objects to include in list.
#' @return A named list with names corresponding the names of the objects in \code{...}
#'
#' # @export
#'
#' @seealso \code{\link[base]{list}}
#' @examples
#'
#' # All variables already defined
#' a <- rnorm(100)
#' b <- mat.or.vec(10, 3)
#' nlist(a,b)
#'
#' # Define some variables in the call and take the rest from the environment
#' nlist(a, b, veggies = c("lettuce", "spinach"), fruits = c("banana", "papaya"))
#'

nlist <- function(...) {
  m <- match.call()
  out <- list(...)
  no_names <- is.null(names(out))
  has_name <- if (no_names) FALSE else nzchar(names(out))
  if (all(has_name)) return(out)
  nms <- as.character(m)[-1]
  if (no_names) {
    names(out) <- nms
  } else {
    names(out)[!has_name] <- nms[!has_name]
  }
  out
}

