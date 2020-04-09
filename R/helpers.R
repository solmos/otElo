#' Compute expected win probability
#'
#'
#' @param r_team
#' @param r_opp
#' @param home_adv
#' @param s
#'
#' @return
#' @export
#'
#' @examples
getExpectedProb <- function(r_team, r_opp, home_adv, s) {
  1 / (1 + 10 ^ ((r_opp - r_team - home_adv) / s))
}


#' Compute Elo rating for next season
#'
#'
#' @param rating
#' @param c
#'
#' @return
#' @export
#'
#' @examples
getCarryOver <- function(rating, c) {
  c * rating + 1505 * (1 - c)
}


#' Compute margin of victory multiplier
#'
#' @param points_diff
#' @param elo_diff
#'
#' @return
#' @export
#'
#' @examples
getMovMultiplier <- function(points_diff, elo_diff) {
  ((abs(points_diff) + 3) ^ 0.8) / (7.5 + 0.006 * elo_diff)
}
