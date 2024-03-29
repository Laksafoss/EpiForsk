% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kija_fct_confint.R
\name{fct_confint}
\alias{fct_confint}
\alias{fct_confint.lm}
\alias{fct_confint.glm}
\alias{fct_confint.lms}
\alias{fct_confint.default}
\title{Confidence set for functions of model parameters}
\usage{
fct_confint(
  object,
  f,
  which_parm = rep(TRUE, length(coef(object))),
  level = 0.95,
  ...
)

\method{fct_confint}{lm}(
  object,
  f,
  which_parm = rep(TRUE, length(coef(object))),
  level = 0.95,
  return_beta = FALSE,
  n_grid = NULL,
  k = NULL,
  len = 0.1,
  parallel = c("sequential", "multisession", "multicore", "cluster"),
  n_cores = 10L,
  ...
)

\method{fct_confint}{glm}(
  object,
  f,
  which_parm = rep(TRUE, length(coef(object))),
  level = 0.95,
  return_beta = FALSE,
  n_grid = NULL,
  k = NULL,
  len = 0.1,
  parallel = c("sequential", "multisession", "multicore", "cluster"),
  n_cores = 10L,
  ...
)

\method{fct_confint}{lms}(
  object,
  f,
  which_parm = rep(TRUE, length(coef(object))),
  level = 0.95,
  return_beta = FALSE,
  len = 0.1,
  n_grid = 0L,
  k = 1000L,
  parallel = c("sequential", "multisession", "multicore", "cluster"),
  n_cores = 10,
  ...
)

\method{fct_confint}{default}(
  object,
  f,
  which_parm = rep(TRUE, length(coef(object))),
  level = 0.95,
  ...
)
}
\arguments{
\item{object}{A fitted model object.}

\item{f}{A function taking the parameter vector as its single argument, and
returning a numeric vector.}

\item{which_parm}{Either a logical vector the same length as the coefficient
vector, with \code{TRUE} indicating a coefficient is used by \code{f}, or an integer
vector with the indices of the coefficients used by \code{f}.}

\item{level}{The confidence level required.}

\item{...}{Additional argument(s) passed to methods.}

\item{return_beta}{Logical, if \code{TRUE} returns both the confidence limits and
the parameter values used from the boundary of the parameter confidence
set.}

\item{n_grid}{Either \code{NULL} or an integer vector of length 1 or the number of
\code{TRUE}/indices in which_parm. Specifies the number of grid points in each
dimension of a grid with endpoints defined by len. If \code{NULL} or \code{0L}, will
instead sample k points uniformly on a sphere.}

\item{k}{If n_grid is \code{NULL} or \code{0L}, the number of points to sample
uniformly from a sphere.}

\item{len}{numeric, the radius of the sphere or box used to define directions
in which to look for boundary points of the parameter confidence set.}

\item{parallel}{Character, specify how futures are resolved. Default is
"sequential". Can be "multisession" to resolve in parallel in separate R
sessions, "multicore" (not supported on Windows) to resolve in parallel in
forked R processes, or "cluster" to resolve in parallel in separate R
sessions running on one or more machines.}

\item{n_cores}{An integer specifying the number of threads to use for
parallel computing.}
}
\value{
A tibble with columns estimate, conf.low, and conf.high or if
return_beta is \code{TRUE}, a list with the tibble and the beta values on the
boundary used to calculate the confidence limits.
}
\description{
Computes confidence sets of functions of model parameters by computing a
confidence set of the model parameters and returning the codomain of the
provided function given the confidence set of model parameters as domain.
}
\details{
Assume the response Y and predictors X are given by a generalized
linear model, that is, they fulfill the assumptions
\deqn{E(Y|X)=\mu(X^T\beta)}{E(Y|X)=\mu(X^T\beta)}
\deqn{V(Y|X)=\psi \nu(\mu(X^T\beta))}{V(Y|X)=\psi \nu(\mu(X^T\beta))}
\deqn{Y|X\sim\varepsilon(\theta,\nu_{\psi}).}{%
  Y|X ~ \varepsilon(\theta,\nu_(\psi)).}
Here \eqn{\mu} is the mean value function, \eqn{\nu} is the variance
function, and \eqn{\psi} is the dispersion parameter in the exponential
dispersion model
\eqn{\varepsilon(\theta,\nu_{\psi})}{\varepsilon(\theta,\nu_(\psi))}, where
\eqn{\theta} is the canonical parameter and \eqn{\nu_{\psi}}{\nu_(\psi)} is
the structure measure. Then it follows from the central limit theorem that
\deqn{\hat\beta\sim N(\beta, (X^TWX)^{-1})}{%
  \hat\beta~N(\beta, (X^TWX)^(-1))}
will be a good approximation in large samples, where \eqn{X^TWX} is the
Fisher information of the exponential dispersion model.

From this, the combinant
\deqn{(\hat\beta-\beta)^TX^TWX(\hat\beta-\beta)}{%
  (\hat\beta-\beta)^TX^TWX(\hat\beta-\beta)}
is an approximate pivot, with a \eqn{\chi_p^2} distribution. Then
\deqn{C_{\beta}=\{\beta|(\hat\beta-\beta)^TX^TWX(\hat\beta-\beta)<\chi_p^2(1-\alpha)\}}{%
  C_(\beta)=\{\beta|(\hat\beta-\beta)^TX^TWX(\hat\beta-\beta)<\chi_p^2(1-\alpha)\}}
is an approximate \eqn{(1-\alpha)}-confidence set for the parameter vector
\eqn{\beta}. Similarly, confidence sets for sub-vectors of \eqn{\beta} can
be obtained by the fact that marginal distributions of normal distributions
are again normally distributed, where the mean vector and covariance matrix
are appropriate subvectors and submatrices.

Finally, a confidence set for the transformed parameters \eqn{f(\beta)}
is obtained as
\deqn{\{f(\beta)|\beta\in C_{\beta}\}}{\{f(\beta)|\beta\in C_(\beta)\}.}
Note this is a conservative confidence set, since parameters outside the
confidence set of \eqn{\beta} can be mapped to the confidence set of the
transformed parameter.

To determine \eqn{C_{\beta}}, \code{fct_confint()} uses a convex optimization
program when f is follows DCP rules. Otherwise, it finds the boundary by
taking a number of points around \eqn{\hat\beta} and projecting them onto
the boundary. In this case, the confidence set of the transformed parameter
will only be valid if the boundary of \eqn{C_{\beta}} is mapped to the
boundary of the confidence set for the transformed parameter.

The points projected to the boundary are either laid out in a grid around
\eqn{\hat\beta}, with the number of points in each direction determined
by \code{n_grid}, or uniformly at random on a hypersphere, with the number of
points determined by \code{k}. The radius of the grid/sphere is determined by
\code{len}.

To print a progress bar with information about the fitting process, wrap
the call to fct_confint in with_progress, i.e.
\code{progressr::with_progress({result <- fct_confint(object, f)})}
}
\examples{
data <- 1:5 |>
  purrr::map(
    \(x) {
      name = paste0("cov", x);
      dplyr::tibble("{name}" := rnorm(100, 1))
    }
  ) |>
  purrr::list_cbind() |>
  dplyr::mutate(
  y = rowSums(dplyr::across(dplyr::everything())) + rnorm(100)
  )
lm <- lm(
 as.formula(
  paste0("y ~ 0 + ", paste0(names(data)[names(data) != "y"], collapse = " + "))
 ),
 data
)
fct_confint(lm, sum)
fct_confint(lm, sum, which_parm = 1:3, level = 0.5)

}
\author{
KIJA
}
