#' Fitting Bayesian generalized linear models via Stan
#'
#' Full Bayesian inference for generalized linear modeling with
#' student t prior distributions for the coefficients.
#'
#'
#' @param formula, family, data, weights, subset, na.action, offset, contrasts
#' Same as in \code{\link[stats]{glm}}.
#'
#' @param start Same as in \code{\link[stats]{glm}}, but if not \code{NULL}
#' also used as starting values for the MCMC. If \code{NULL} (the default),
#' then \code{\link[rstan]{stan}} is initialized with \code{init = 'random'}.
#'
#' @param etastart, mustart, control, model, method Same as in
#' \code{\link[stats]{glm}} but ignored by \code{\link[rstan]{stan}}.
#'
#' @param intercept Same as in \code{\link[stats]{glm.fit}}.
#'
#' @param x For \code{stanglm} and \code{stanlm}, this argument is included for
#' compatibility with \code{\link[stats]{glm}} but ignored by
#' \code{\link[rstan]{stan}}. For \code{stanglm.fit}, a \code{\link{model.matrix}}
#' like that for \code{\link{glm.fit}}.
#'
#' @param y For \code{stanglm} and \code{stanlm}, this argument is included for
#' compatibility with \code{\link[stats]{glm}} but ignored by
#' \code{\link[rstan]{stan}}. For \code{stanglm.fit}, a \code{\link{model.response}}
#' like that for \code{\link{glm.fit}}.
#'
#' @param qr,singular.ok Included for compatibility with \code{\link[stats]{lm}}
#' but ignored by \code{\link[rstan]{stan}}.
#'
#' @param prior.mean Prior mean (technically location) vector for the
#' coefficient(s) in the model, not counting the intercept, which defaults to 0
#' and is replicated to the appropriate length.
#'
#' @param prior.scale Prior scale vector for the coefficient(s) in the model,
#' not counting the intercept, whose default depend on the nature of the model.
#' See the Details section.
#'
#' @param prior.df Prior degrees of freedom vector for the coefficient(s) in the
#' model, not countingthe intercept, which defaults to 1 (implying a Cauchy prior)
#' and is replicated to the appropriate length.
#'
#' @param prior.mean.for.intercept Prior mean (technically location) for the
#' intercept, which defaults to 0.
#'
#' @param prior.scale.for.intercept Prior scale for the intercept, which
#' defaults to 10.
#'
#' @param prior.df.for.intercept Prior degrees of freedom for the intercept,
#' which defaults to 1 and implies a Cauchy prior.
#'
#' @param min.prior.scale Minimum prior scale for the intercept and coefficients.
#' See the Details section.
#'
#' @param scaled Logical scalar, defaulting to \code{TRUE},
#' and if \code{TRUE} further scales the prior.scale by the range of the
#' predictor if the predictor has exactly two unique values and scales
#' prior.scale by twice the standard deviation of the predictor if it has
#' more than two unique values.
#'
#' @param prior.scale.for.dispersion Prior scale for the standard error of the
#' regression in Gaussian models, which is given a half-Cauchy prior truncated
#' at zero.
#'
#' @param ... Further arguments passed to \code{\link[rstan]{stan}}
#' (e.g. \code{iter}, \code{chains}, etc.).
#'
#' @details The \code{stanglm} function is similar in syntax to
#' \code{\link[stats]{glm}} but rather than performing maximum likelihood
#' estimation of generalized linear models, full Bayesian estimation is
#' performed via Markov Chain Monte Carlo. Thus, the Bayesian model adds
#' independent student t priors on the coefficients of the generalized linear
#' model. The \code{stanlm} function calls \code{stanglm} with
#' \code{family = gaussian}.
#'
#' @return An object of \code{\link[rstan]{stanfit-class}}.
#'
#' @seealso \code{\link[stats]{glm}}, \code{\link[rstan]{stan}}
#'
#' @export
#'

stanglm <- function(formula, family = gaussian(), data, weights, subset,
           na.action, start = NULL, etastart, mustart, offset, control = list(),
           model = TRUE, method = "glm.fit", x = FALSE, y = TRUE, contrasts = NULL,
           # above arguments from glm(), below arguments from arm:::bayesglm()
           prior.mean = 0, prior.scale = NULL, prior.df = 1,
           prior.mean.for.intercept = 0, prior.scale.for.intercept = NULL,
           prior.df.for.intercept = 1, min.prior.scale = 1e-12, scaled = TRUE,
           prior.scale.for.dispersion = 5, ...) { # further arguments to stan()

    # Parse like glm()
    mf <- match.call(expand.dots = FALSE)
    arg_nms <- c("formula", "data", "subset", "weights", "na.action",
                 "etastart", "mustart", "offset")
    m <- match(arg_nms, names(mf), nomatch = 0L)
    mf <- mf[c(1L, m)]
    mf$drop.unused.levels <- TRUE
    mf[[1L]] <- as.name("model.frame")
    mf <- eval(mf, parent.frame())
    mt <- attr(mf, "terms")
    Y <- model.response(mf, "any")
    if (length(dim(Y)) == 1L) {
      nm <- rownames(Y)
      dim(Y) <- NULL
      if (!is.null(nm))
        names(Y) <- nm
    }
    if (!is.empty.model(mt)) X <- model.matrix(mt, mf, contrasts)
    else X <- matrix(, NROW(Y), 0L)
    weights <- as.vector(model.weights(mf))
    if (!is.null(weights) && !is.numeric(weights)) stop("'weights' must be a numeric vector")
    if (!is.null(weights) && any(weights < 0))     stop("negative weights not allowed")
    if ( is.null(weights)) weights <- rep(1.0, NROW(Y))
    offset <- as.vector(model.offset(mf))
    if (!is.null(offset)) {
      if (length(offset) != NROW(Y))
        stop(gettextf("number of offsets is %d should equal %d (number of observations)",
                      length(offset), NROW(Y)), domain = NA)
    }
    else offset <- rep(0, nrow(X))

    stanglm.fit(X, Y, weights, start, etastart, mustart, offset, family, list(), TRUE,
                prior.mean, prior.scale, prior.df,
                prior.mean.for.intercept, prior.scale.for.intercept,
                prior.df.for.intercept, min.prior.scale,
                scaled, prior.scale.for.dispersion, ...)
  }

