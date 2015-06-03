#' @rdname stan_glmer
#' @export
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
