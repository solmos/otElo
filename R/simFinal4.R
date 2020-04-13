#' Simulate Final Four results
#'
#' @param teams Character vector of length 4 with the name of the teams
#'   that won the Playoffs. Must be in the same order of Playoffs matchups.
#' @param elo Numeric vector of length 4 with the Elo ratings of the teams
#'   that won the Playoffs. Must be in the same order of Playoffs matchups.
#' @param fit List with the point-spread model fit
#'
#' @return
#' @export
#'
#' @examples
simFinal4 <- function(teams, elo, fit) {

  ff_teams <- createMatchups(teams, elo, type = "FF")

  # Semifinals simulation
  semi1_result <- simGame(
    teams = ff_teams$semi1$teams,
    elo = ff_teams$semi1$elo,
    fit = fit, home = FALSE
  )

  semi2_result <- simGame(
    teams = ff_teams$semi2$teams,
    elo = ff_teams$semi2$elo,
    fit = fit, home = FALSE
  )

  # Semifinals winners table
  teams_final <- tibble::tibble(
    team = c(
      semi1_result$team[semi1_result$win == TRUE],
      semi2_result$team[semi2_result$win == TRUE]),
    elo = c(
      semi1_result$elo_new[semi1_result$win == TRUE],
      semi2_result$elo_new[semi2_result$win == TRUE]
    )
  )

  # Final simulation
  final <- simGame(teams_final$team, teams_final$elo, fit, home = FALSE)
  champion <- tibble::tibble(
    team = final$team[final$win == TRUE],
    elo = final$elo_new[final$win == TRUE]
  )

  list(final = teams_final, champion = champion)
}
