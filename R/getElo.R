#' Get Elo ratings for several seasons
#'
#' @param df
#' @param k
#' @param home_adv
#' @param s
#' @param carry
#'
#' @return
#' @export
#'
#' @examples
getElo <- function(df, k = 25, home_adv = 100, s = 400, carry = 0.8) {
  df <- dplyr::arrange(df, game_date)
  season_results <- split(df, df$season)

  # Start with first season
  teams <- sort(unique(season_results[[1]]$team_code_home))
  # Since it is the first season overall, all teams start with 1300 Elo points
  initial_ratings <- as.list(rep(1300, length(teams)))
  names(initial_ratings) <- teams

  first_season_ratings <- getSeasonElo(
    season_results[[1]],
    k = k,
    home_adv = home_adv,
    s = s,
    initial_elo = initial_ratings
  )

  # Use the last recorded Elo rating, not last season
  elo_final <- tibble::tibble(
    season = unique(season_results[[1]]$season),
    team = names(first_season_ratings$team_elo),
    elo = unlist(first_season_ratings$team_elo)
  )

  season_ratings <- vector("list", length(season_results))
  season_ratings[[1]] <- first_season_ratings
  for (i in 2:length(season_ratings)) {
    teams <- sort(unique(season_results[[i]]$team_code_home))

    teams_new <- teams[!teams %in% elo_final$team]
    teams_new_elo <- as.list(rep(1300, length(teams_new)))
    names(teams_new_elo) <- teams_new

    teams_old <- teams[teams %in% elo_final$team]
    teams_old_elo <- vector("list", length(teams_old))
    names(teams_old_elo) <- teams_old
    for (j in seq_along(teams_old)) {
      elo_team <- elo_final %>%
        dplyr::filter(team == teams_old[j])
      teams_old_elo[[j]] <- elo_team$elo[which.max(elo_team$season)]
    }

    teams_old_elo <- lapply(teams_old_elo, getCarryOver, c = carry)
    initial_elo <- c(teams_new_elo, teams_old_elo)

    season_ratings[[i]] <- getSeasonElo(
      season_results[[i]],
      k = k,
      home_adv = home_adv,
      s = s,
      initial_elo = initial_elo
    )

    elo_final_season <- tibble::tibble(
      season = unique(season_results[[i]]$season),
      team = names(season_ratings[[i]]$team_elo),
      elo = unlist(season_ratings[[i]]$team_elo)
    )

    elo_final <- dplyr::bind_rows(elo_final, elo_final_season)
  }

  output_df <- purrr::map_df(season_ratings, function(x) x$ratings_df)

  output_df
}
