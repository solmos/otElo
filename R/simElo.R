#' Simulate games using Elo ratings
#'
#' @param df Data frame with all the games (as in rschedule)
#' @param k Scalar indicating the k factor
#' @param home_adv Scalar indicating home court advantage (in Elo points)
#' @param s Scalar indicating the scale parameter
#' @param initial_elo Named list with each element specifying a team Elo rating
#'
#' @return
#' @export
#'
#' @examples
simElo <- function(df,  k, home_adv, s, initial_elo) {
  #browser()
  team_ratings <- initial_elo

  # Data frame to store the subsequent values obtained by the algorithm
  ratings_df <- df %>%
    dplyr::mutate(
      home_adv = NA,
      win_points_home = NA,
      win_points_away = NA,
      expected_prob_home = NA,
      expected_prob_away = NA,
      pt_diff_home = NA,
      pt_diff_away = NA,
      mov_home = NA,
      mov_away = NA,
      elo_prev_home = NA,
      elo_prev_away = NA,
      elo_new_home = NA,
      elo_new_away = NA,
      winner = NA
    )

  # Point-spread model
  pt_diff <- elohistwide$pt_diff_home
  elo_diff <- elohistwide$elo_diff_home
  pts_model <- fitModel(y = pt_diff, x = elo_diff)

  for (i in 1:nrow(df)) {
    team_home <- df$team_home[i]
    team_away <- df$team_away[i]

    elo_home <- team_ratings[[team_home]]
    elo_away <- team_ratings[[team_away]]

    # Home advantage set to 0 for Final 4 games
    h <- ifelse(df$phase[i] == "FF", 0, home_adv)

    # Find pre-game win probabilities
    expected_prob_home <- getExpectedProb(
      r_team = elo_home,
      r_opp = elo_away,
      home_adv = h,
      s = s)
    expected_prob_away <- 1 - expected_prob_home

    # Margin of victory multiplier
    elo_diff_home <- elo_home + h - elo_away
    elo_diff_away <- elo_away - elo_home - h
    points_diff_home <- predictPointDiff(pts_model, elo_diff_home)

    # MOV multiplier uses absolute value of point difference
    mov_home <- getMovMultiplier(points_diff_home, elo_diff_home)
    mov_away <- getMovMultiplier(points_diff_home, elo_diff_away)

    # Assign 1 for wins and 0 for losses
    win_points_home <- ifelse(points_diff_home >= 0, 1, 0)
    win_points_away <- ifelse(win_points_home == 0, 1, 0)

    # Update Elo ratings
    elo_home_new <- elo_home + k * (win_points_home - expected_prob_home) * mov_home
    elo_away_new <- elo_away + k * (win_points_away - expected_prob_away) * mov_away

    team_ratings[[team_home]] <- elo_home_new
    team_ratings[[team_away]] <- elo_away_new

    # TODO: Add probabilistic prediction?
    # prob_pred <- sample(
    #   x = c(team_home, team_away),
    #   size = 1,
    #   prob = c(expected_prob_home, expected_prob_away)
    # )

    winner <- ifelse(points_diff_home >= 0, team_home, team_away)

    ratings_df$home_adv[i] <- h
    ratings_df$win_points_home[i] <- win_points_home
    ratings_df$win_points_away[i] <- win_points_away
    ratings_df$expected_prob_home[i] <- expected_prob_home
    ratings_df$expected_prob_away[i] <- expected_prob_away
    ratings_df$mov_home[i] <- mov_home
    ratings_df$mov_away[i] <- mov_away
    ratings_df$elo_prev_home[i] <- elo_home
    ratings_df$elo_prev_away[i] <- elo_away
    ratings_df$elo_new_home[i] <- elo_home_new
    ratings_df$elo_new_away[i] <- elo_away_new
    ratings_df$winner[i] <- winner
    ratings_df$pt_diff_home[i] <- points_diff_home
    ratings_df$pt_diff_away[i] <- -points_diff_home
  }

  ratings_df
}
