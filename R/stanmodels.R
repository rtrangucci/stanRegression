require(rstan)
MODELS_HOME <- file.path(dirname(system.file(package = "stanRegression")),
                         "stanRegression", "exec")
stanfit_gaussian <- stan(file.path(MODELS_HOME, "gaussian.stan"),
                         model_name = "Gaussian GLM", chains = 0)
stanfit_discrete <- stan(file.path(MODELS_HOME, "discrete.stan"),
                         model_name = "Discrete GLM", chains = 0)
