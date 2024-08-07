#' Compute empirical logit
#'
#' @param x vector containing number of looks to target
#' @param y vector containing number of looks to distractors
#' @return \code{empirical_logit} returns the empirical logit of looking to
#'   target. \code{empirical_logit_weight} returns weights for these values.
#' @export
#' @references Dale Barr's
#' \href{http://talklab.psy.gla.ac.uk/tvw/elogit-wt.html}{Walkthrough of an
#' "empirical logit" analysis in R}
empirical_logit <- function(x, y) {
  log((x + 0.5) / (y + 0.5))
}

#' @rdname empirical_logit
#' @export
empirical_logit_weight <- function(x, y) {
  var1 <- 1 / (x + 0.5)
  var2 <- 1 / (y + 0.5)
  var1 + var2
}

#' Standard error for proportions
#'
#' See http://www.r-tutor.com/elementary-statistics/interval-estimation/interval-estimate-population-proportion
#' @param proportion proportions of hits
#' @param n_possible numbers of total events
#' @return the standard errors of the proportion estimates
#' @export
se_prop <- function(proportion, n_possible) {
  spread <- proportion * (1 - proportion)
  sqrt(spread / n_possible)
}

