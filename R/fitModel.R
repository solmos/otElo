#' Fit point-spread model
#'
#' @param y Numeric vector of point spreads.
#' @param x Numeric vector of Elo rating differences.
#' @param init Named vector with starting values for the parameters
#'   to be optimized.
#'
#' @return
#' @export
#'
#' @examples
fitModel <- function(y, x, init) {

  if (missing(init)) {
    init <- c(alpha = 1, beta = 0.5, sigma = 5)
  }

  # Define log posterior that needs to be optimized (maximized)
  log_posterior <- function(p, y, x) {
    # Log riors
    alpha <- stats::dnorm(p["alpha"], 0, 1, log = TRUE)
    beta <- stats::dlnorm(p["beta"], -4, 0.8, log = TRUE)
    sigma <- stats::dunif(p["sigma"], 0, 50, log = TRUE)
    # Assuming independence
    joint_log_prior <- alpha + beta + sigma

    # Log likelihood
    mu <- p["alpha"] + p["beta"] * x
    log_lik <- sum(stats::dnorm(y, mu, p["sigma"], log = TRUE))

    # Unnormalized joint log posterior distribution
    log_posterior <- log_lik + joint_log_prior

    unname(log_posterior)
  }

  quad_approx <- stats::optim(init, log_posterior,
                       control = list(fnscale = -1),
                       hessian = TRUE, y = y, x = x)

  # Parameter values that maximize the joint posterior log likelihood
  par_peak <- quad_approx$par
  # Variance-covariance matrix of the approximated joint posterior distribution
  cov_matrix <- solve(-quad_approx$hessian)
  # Generate posterior samples for all parameters
  posterior_samples <- mvtnorm::rmvnorm(1000, par_peak, sigma = cov_matrix) %>%
    tibble::as_tibble()

  list(par = par_peak, covariance = cov_matrix, samples = posterior_samples)
}

#' Compute mean, sd, and credible interval of model parameters
#'
#' @param fit List with model fit
#'
#' @return
#' @export
#'
#' @examples
getParTable <- function(fit) {
  par_means <- colMeans(fit$samples)
  par_sd <- apply(fit$samples, 2, sd)
  par_uci <- apply(x$samples, 2, quantile, 0.95)
  par_lci <- apply(x$samples, 2, quantile, 0.05)
  tibble::tibble(
    mean = par_means,
    sd = par_sd,
    lower = par_lci,
    upper = par_uci
  )
}

#' Predict a single mu observation for a single Elo difference
#'
#' @param fit List with model fit
#' @param x Scalar of Elo difference in a game
#'
#' @return
#' @export
#'
#' @examples
predictPointDiff <- function(fit, x) {
  pos_sample <- mvtnorm::rmvnorm(1, fit$par, sigma = fit$covariance) %>%
    tibble::as_tibble()
  mu <- pos_sample$alpha + pos_sample$beta * x
  pt_diff <- stats::rnorm(1, mean = mu, sd = pos_sample$sigma)

  pt_diff
}
