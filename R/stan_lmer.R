#' stan_lmer
#' 
#' @export
#' 

stan_lmer <- function(fl , data , family=gaussian , prefix=c("b_","v_") , default_prior="dnorm(0,10)" , matt_trick = TRUE, file_name=NULL, run_stan = FALSE, ...){
  if(is.null(file_name)){
    warning("Generated stan code should go into a file with .stan suffix")
  }
  glim_result <- glimmer(fl , data , family, prefix=c("b_","v_") , default_prior="dnorm(0,10)" , matt_trick)
  stan_code <- map2stan(glim_result$f, glim_result$d, sample=FALSE)
  if(!is.null(file_name)){
    filewrite <- file(file_name)
    writeLines(stan_code$model, filewrite)
    close(filewrite)
  }
  
  if (run_stan) {
    stanfit <- run_rstan(file = file_name, data = stan_code$data, ...)
    return(stanfit)
  }
  
  invisible(list(stan_code = stan_code$model, data=stan_code$data))
}
