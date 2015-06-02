#' stan_lmer
#'
#' @param formula Formula (using lmer syntax).
#' @param data A \code{data.frame} containing the variables in the model.
#' @param prefix
#' @param default_prior Default prior for 'fixed effects'.
#' @param loc_scale_transform Logical. Use non-centered parameterizations?
#' @param file_name Full path for .stan file to be created.
#' @param run_stan Logical. Fit the model using \pkg{rstan} in addition
#' to creating the .stan file?
#' @param ... If \code{run_stan = TRUE}, optional arguments to pass to
#' \code{stan} (e.g. \code{iter}, \code{chains}, etc.).
#' See \code{\link[rstan]{stan}} (\pkg{rstan}).
#'
#' @export
#'

stan_lmer <- function(formula, data, prefix = c("b_","v_"),
                      default_prior = "dnorm(0,10)", loc_scale_transform = TRUE,
                      file_name = NULL, run_stan = FALSE, ...) {

  stan_glmer(formula = formula,
             data = data,
             family = "gaussian",
             prefix = prefix,
             default_prior = default_prior,
             loc_scale_transform = loc_scale_transform,
             file = file_name,
             run_stan = run_stan,
             ...)
}
