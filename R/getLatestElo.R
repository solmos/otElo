#' Get latest Elo ratings
#'
#' \code{getLatestElo} returns the latest Elo ratings for all teams in the given season
#'
#' @param df Data frame of Elo ratings as returned by \code{getElo()}
#'   in long format.
#'
#' @return
#' @export
#'
#' @examples
getLatestElo <- function(df, .season, as.list = TRUE) {
  latest_dates <- df %>%
    dplyr::filter(season == .season) %>%
    dplyr::group_by(team) %>%
    dplyr::summarise(date = max(game_date))

  latest_elo <- df %>%
    dplyr::filter(game_date %in% unique(latest_dates$date)) %>%
    dplyr::select(season, game_date, team, team_code, elo_new) %>%
    dplyr::arrange(dplyr::desc(elo_new))

  if (as.list == TRUE) {
    latest_elo_list <- as.list(latest_elo$elo_new)
    # TODO: Should the names be full team names or team codes?
    names(latest_elo_list) <- latest_elo$team

    return(latest_elo_list)
  }

  latest_elo
}
