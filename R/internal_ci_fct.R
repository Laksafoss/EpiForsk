#' solve optimization problem for  CI bounds
#'
#' solve optimization problem for each coordinate of f, to obtain the uniform
#' limit.
#'
#' @param i index of point at which to solve for confidence limits
#' @param verbose flag from fct_confint
#' @param parallel flag from fct_confint
#' @param xtx_red reduced form of matrix *X^TX*
#' @param beta_hat vector of parameter estimates
#' @param which_parm vector indicating which parameters to include
#'
#' @return one row tibble with estimate and confidence limits
#'
#' @examples 1+1

ci_fct <- function(i, verbose, parallel, xtx_red, beta_hat, which_parm) {
  # find ci_lower
  beta <- CVXR::Variable(dim(xtx_red)[1])
  objective <- CVXR::Minimize(f(beta)[i])
  constraint <- CVXR::quad_form(beta_hat[which_parm] - beta, xtx_red) <=
    qchisq(level, length(beta_hat[which_parm]))
  problem <- CVXR::Problem(objective, constraints = list(constraint))
  result_lower <- CVXR::solve(problem)
  # find ci_upper
  beta <- CVXR::Variable(dim(xtx_red)[1])
  objective <- CVXR::Maximize(f(beta)[i])
  constraint <- CVXR::quad_form(beta_hat[which_parm] - beta, xtx_red) <=
    qchisq(level, length(beta_hat[which_parm]))
  problem <- CVXR::Problem(objective, constraints = list(constraint))
  result_upper <- CVXR::solve(problem)
  # collect results
  out <- tibble::tibble(
    estimate = f(beta_hat[which_parm])[i],
    ci_lower = result_lower$value,
    ci_upper = result_upper$value
  )
  if (verbose & !parallel) {
    pb$tick(tokens = list(iteration = i))
  }
  return(out)
}
