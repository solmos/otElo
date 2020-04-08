#' Scrape future games from a round
#'
#' @param round Scalar indicating the roound code
#'
#' @return
#' @export
#'
#' @examples
extractRound <- function(round) {
  main_url <- paste0(
    "https://www.euroleague.net/main/results?gamenumber=",
    round,
    "&phasetypecode=RS&seasoncode=E2019"
  )

  round_html <- xml2::read_html(main_url)

  game_nodes <- round_html %>%
    rvest::html_node("div#main-one") %>%
    rvest::html_nodes("div.game")

  game_urls <- game_nodes %>%
    rvest::html_node("a") %>%
    rvest::html_attr("href") %>%
    paste0(main_url, .)

  game_codes <- game_urls %>%
    stringr::str_extract("gamecode=\\d+") %>%
    stringr::str_remove("gamecode=")

  team_names <- game_nodes %>%
    rvest::html_nodes("div.club") %>%
    rvest::html_node("span.name") %>%
    rvest::html_text()
  home_idx <- seq(1, (length(team_names) - 1), by = 2)
  home_teams <- team_names[home_idx]
  away_teams <- team_names[-home_idx]

  games <- tibble::tibble(
    season = 2019,
    game_code = as.integer(game_codes),
    round_code = round,
    team_home = home_teams,
    team_away = away_teams
  )

  games
}
