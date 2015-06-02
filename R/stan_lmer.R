#' stan_lmer
#'
#' @param formula Formula (using lmer syntax).
#' @param data A \code{data.frame} containing the variables in the model.
#' @param family
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

stan_lmer <- function(formula, data, family = "gaussian", prefix = c("b_","v_"),
                      default_prior = "dnorm(0,10)", loc_scale_transform = TRUE,
                      file_name = NULL, run_stan = FALSE, ...) {
  if(is.null(file_name)){
    warning("Generated stan code should go into a file with .stan suffix")
  }
  glim_result <- glimmer(formula , data, family, prefix=c("b_","v_") , default_prior=default_prior , loc_scale_transform)
  stan_code <- map2stan(glim_result$f, glim_result$d, sample = FALSE)
  if(!is.null(file_name)){
    filewrite <- file(file_name)
    writeLines(stan_code$model, filewrite)
    close(filewrite)
  }

  if (run_stan) {
    stanfit <- run_rstan(file = file_name, data = stan_code$data, ...)
    return(list(fit = stanfit, data = glim_result$d))
  }

  invisible(list(stan_code = stan_code$model, data=stan_code$data))
}
