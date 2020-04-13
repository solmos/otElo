simSeries <- function(teams, elo, fit) {

  # Simulate first three games and update elo after each game
  game1 <- simGame(teams, elo, fit)
  elo <- game1$elo_new
  game2 <- simGame(teams, elo, fit)
  elo <- game2$elo_new
  game3 <- simGame(teams[c(2, 1)], elo[c(2, 1)], fit)
  elo <- game3$elo_new

  first3 <- dplyr::bind_rows(game1, game2, game3, .id = "game")

  # Check if any team won 3 games and finish if it did
  wins_in_3 <- first3 %>%
    dplyr::group_by(team) %>%
    dplyr::summarise(wins = sum(win))

  wins_team1 <- sum(first3$win[first3$team == teams[1]])
  wins_team2 <- sum(first3$win[first3$team == teams[2]])

  if (wins_team1 == 3 | wins_team2 == 3) {
    return(first3)
  } else {
    game4 <- simGame(teams[c(2, 1)], elo[c(2, 1)], fit)
    elo <- game4$elo_new
  }

  first4 <- dplyr::bind_rows(game1, game2, game3, game4, .id = "game")

  wins_team1 <- sum(first4$win[first4$team == teams[1]])
  wins_team2 <- sum(first4$win[first4$team == teams[2]])

  if (wins_team1 == 3 | wins_team2 == 3) {
    return(first4)
  } else {
    game5 <- simGame(teams, elo, fit)
    five_games <- dplyr::bind_rows(
      game1, game2, game3, game4, game5, .id = "game"
    )
    return(five_games)
  }
}

createMatchups <- function(teams, elo, type = c("PO", "FF")) {
  if (type == "PO") {
    list(
      a = list(teams = c(teams[1], teams[8]), elo = c(elo[1], elo[8])),
      b = list(teams = c(teams[4], teams[5]), elo = c(elo[4], elo[5])),
      c = list(teams = c(teams[3], teams[6]), elo = c(elo[3], elo[6])),
      d = list(teams = c(teams[2], teams[7]), elo = c(elo[2], elo[7]))
    )
  } else {
    list(
      semi1 = list(teams = teams[1:2], elo = elo[1:2]),
      semi2 = list(teams = teams[4:3], elo = elo[4:3])
    )
  }
}

#' Get winner from a Playoff series
#'
#' @param df Data frame with Playoff series simulation results
#'
#' @return
#' @export
#'
#' @examples
getSeriesWinner <- function(df) {
  wins <- df %>%
    dplyr::group_by(team) %>%
    dplyr::summarise(wins = sum(win))
  winner <- wins$team[wins$wins == 3]
  elo_winner <- df %>%
    dplyr::filter(team == winner & game == max(game)) %>%
    .$elo_new

  tibble::tibble(team = winner, elo = elo_winner)
}
