## code to prepare `gameresults` dataset goes here

gameresults <- eurolig::gameresults
# There are a few missing team codes corresponding to SAS (Dinamo Sassari)
# TODO: Should fix this in eurolig package
gameresults$team_code_home[which(is.na(gameresults$team_code_home))] <- "SAS"
gameresults$team_code_away[which(is.na(gameresults$team_code_away))] <- "SAS"

# Scrape results from season 2019-20
results2019 <- eurolig::extractResults(2019) %>%
  dplyr::arrange(game_code)

Sys.setlocale(category = "LC_TIME", locale = "en_US.UTF-8")
gmresults <- gameresults %>%
  dplyr::bind_rows(results2019) %>%
  dplyr::mutate(
    date = paste0(season, " ", date),
    date_time = as.POSIXct(date, format = "%Y %B %d %H:%M", tz = "CET"),
    d = lubridate::day(date_time),
    m = lubridate::month(date_time),
    y = ifelse(m >= 8, season, season + 1),
    game_date = lubridate::make_date(y, m, d)
  ) %>%
  # TODO: Better way to deal with leap years
  dplyr::mutate(
    date = ifelse(
      stringr::str_detect(date, "February 29") & is.na(date_time),
      stringr::str_replace(date, "\\d{4}", as.character(season + 1)),
      date
    ),
    date_time = as.POSIXct(date, format = "%Y %B %d %H:%M", tz = "CET"),
    d = lubridate::day(date_time),
    m = lubridate::month(date_time),
    y = ifelse(m >= 8, season, season + 1),
    game_date = lubridate::make_date(y, m, d)
  )

usethis::use_data(gmresults)
readr::write_csv(gmresults)
