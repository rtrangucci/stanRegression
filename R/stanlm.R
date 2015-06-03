#' @rdname stanglm
#' @export
stanlm <- function (formula, data, subset, weights, na.action, method = "qr",
            model = TRUE, x = FALSE, y = FALSE, qr = TRUE, singular.ok = TRUE,
            contrasts = NULL, offset = NULL,
            # above arguments from glm(), below arguments from arm:::bayesglm()
            prior.mean = 0, prior.scale = NULL, prior.df = 1,
            prior.mean.for.intercept = 0, prior.scale.for.intercept = NULL,
            prior.df.for.intercept = 1, min.prior.scale = 1e-12, scaled = TRUE,
            prior.scale.for.dispersion = 5, ...) { # further arguments to stan()

    mf <- match.call(expand.dots = TRUE)
    mf[[1L]] <- as.name("stanglm")
    mf$qr <- mf$singular.ok <- NULL
    return(eval(mf, parent.frame()))
}
