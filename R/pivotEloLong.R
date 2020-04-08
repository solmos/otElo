#' Convert data frame of Elo ratings from wide to long
#'
#' @param df Data frame of Elo ratings as returned by \code{getElo()}
#'
#' @return
#' @export
#'
#' @examples
pivotEloLong <- function(df) {
  df %>%
    tidyr::pivot_longer(
      cols = c(tidyselect::ends_with("home"), tidyselect::ends_with("away")),
      names_to = c(".value", "where"),
      names_pattern = "(.+_?.?)_(.+)"
    )
}
