#' @rdname stanglm
#' @export

stanglm.fit <- function(x, y, weights = rep(1, NROW(x)), start = NULL, etastart = NULL,
           mustart = NULL, offset = rep(0, NROW(x)), family = gaussian(),
           control = list(), intercept = TRUE,
           # above arguments from glm(), below arguments from arm:::bayesglm()
           prior.mean = 0, prior.scale = NULL, prior.df = 1,
           prior.mean.for.intercept = 0, prior.scale.for.intercept = NULL,
           prior.df.for.intercept = 1, min.prior.scale = 1e-12, scaled = TRUE,
           prior.scale.for.dispersion = 5, ...) { # further arguments to stan()

    if(!is(family, "family")) stop("'family' must be a family")
    x <- as.matrix(x)

    # these are from help(family)
    supported_families <- c("binomial", "gaussian", "Gamma",
                            "inverse.gaussian","poisson")
    fam <- which(supported_families == family$family)
    if(length(fam) == 0) {
      stop("'family' must be one of ", supported_families)
    }

    # these are also from help(family)
    supported_links <- switch(supported_families[fam],
                              binomial = c("logit", "probit", "cauchit", "log", "cloglog"),
                              gaussian = c("identity", "log", "inverse"),
                              Gamma = c("inverse", "identity", "log"),
                              inverse.gaussian = c("1/mu^2", "inverse", "identity", "log"),
                              poisson = c("log", "identity", "sqrt"),
                              stop("unsupported family")
    )
    link <- which(supported_links == family$link)
    if (length(link) == 0) {
      stop("'link' must be one of ", supported_links)
    }

    # process hyperprior values like arm:::bayesglm() does
    if (is.null(prior.scale)) {
      prior.scale <- 2.5
      if (family$link == "probit") prior.scale <- prior.scale * dnorm(0) / dlogis(0)
    }
    if (is.null(prior.scale.for.intercept)) {
      prior.scale.for.intercept <- 10
      if (family$link == "probit") {
        prior.scale.for.intercept <- prior.scale.for.intercept * dnorm(0) / dlogis(0)
      }
    }
    nvars <- ncol(x) - 1
    if (length(prior.df) == 1)    prior.df <- rep(prior.df, nvars)
    prior.df <- as.array(pmin(.Machine$double.xmax, prior.df))
    prior.df.for.intercept <- min(.Machine$double.xmax, prior.df.for.intercept)
    if (length(prior.mean)  == 1) prior.mean  <- rep(prior.mean,  nvars)
    prior.mean <- as.array(prior.mean)
    if (length(prior.scale) == 1) prior.scale <- rep(prior.scale, nvars)
    if (scaled) {
      if (family$family == "gaussian") {
        prior.scale <- prior.scale * 2 * sd(y)
        prior.scale.for.intercept <- prior.scale.for.intercept * 2 * sd(y)
      }
      prior.scale <- pmax(min.prior.scale, prior.scale /
                            apply(x[,-1,drop=FALSE], 2, FUN = function(x) {
                              num.categories <- length(unique(x))
                              x.scale <- 1
                              if(num.categories == 2)     x.scale <- diff(range(x))
                              else if(num.categories > 2) x.scale <- 2 * sd(x)
                            }))
    }
    prior.scale <- as.array(pmin(.Machine$double.xmax, prior.scale))
    priors.scale.for.intercept <- min(.Machine$double.xmax, prior.scale.for.intercept)

    # create entries in the data {} block of the .stan file
    standata <- list(N = nrow(x),
                     K = ncol(x),
                     X = x,
                     y = y,
                     family = fam,
                     link = link,
                     has_weights = as.integer(!all(weights == 1)),
                     weights = weights,
                     has_offset = as.integer(!all(offset == 0)),
                     offset = offset,
                     prior_scale = prior.scale,
                     prior_scale_for_intercept = prior.scale.for.intercept,
                     prior_mean = prior.mean,
                     prior_mean_for_intercept = prior.mean.for.intercept,
                     prior_df = prior.df,
                     prior_df_for_intercept = prior.df.for.intercept)

    if (family$family == "gaussian") {
      standata$prior_scale_for_dispersion <- prior.scale.for.dispersion
    }

    # call stan() to draw from posterior distribution
    if (supported_families[fam] == "gaussian") stanfit <- stanfit_gaussian
    else if (supported_families[fam] %in% c("binomial", "poisson")) stanfit <- stanfit_discrete
    else stop("model not supported yet") # FIXME

    if (is.null(start)) start <- "random"
    else start <- as.list(start)
    stanfit <- rstan:::stan(fit = stanfit, data = standata, init = start, ...)
    betas <- grepl("beta[", dimnames(stanfit)$parameters, fixed = TRUE)
    stanfit@sim$fnames_oi[betas] <- colnames(x)
    return(stanfit)
}


