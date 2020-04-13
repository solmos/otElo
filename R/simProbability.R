#' Get simulation probabilities
#'
#' @param n.sim Scalar indicating the number of simulations to perform
#' @param fgames Data frame of future games in wide format (as in rschedule)
#' @param pgames Data frame of past games in long format (as in elohist)
#' @param fit List of fitted point-spread model
#'
#' @return
#' @export
#'
#' @examples
simProbability <- function(n.sim, fgames, pgames, fit) {

  elo_sim <- vector("list", n.sim)
  for (i in seq_along(elo_sim)) {
    elo_sim[[i]] <- simSeason(fgames, pgames, fit)
  }

  playoff_teams <- purrr::map_dfr(elo_sim,
                                  ~ .$playoff_teams, .id = "simulation") %>%
    dplyr::mutate(simulation = as.integer(simulation))

  prob_playoff <- playoff_teams %>%
    dplyr::group_by(team) %>%
    dplyr::summarise(
      simulations = n.sim,
      qualified = n(),
      playoffs = qualified / simulations
    ) %>%
    dplyr::arrange(desc(playoffs))

  final4_teams <- purrr::map_dfr(
    elo_sim,
    ~ .$final4_teams, .id = "simulation"
  ) %>%
    dplyr::mutate(simulation = as.integer(simulation))

  prob_final4 <- final4_teams %>%
    dplyr::group_by(team) %>%
    dplyr::summarise(
      simulations = n.sim,
      qualified = n(),
      final4 = qualified / simulations
    ) %>%
    dplyr::arrange(desc(final4))

  final_teams <- purrr::map_dfr(
    elo_sim,
    ~ .$finals, .id = "simulation"
  ) %>%
    dplyr::mutate(simulation = as.integer(simulation))

  prob_final <- final_teams %>%
    dplyr::group_by(team) %>%
    dplyr::summarise(
      simulations = n.sim,
      qualified = n(),
      final = qualified / simulations
    ) %>%
    dplyr::arrange(desc(final))

  champion <- purrr::map_dfr(
    elo_sim,
    ~ .$champion, .id = "simulation"
  ) %>%
    dplyr::mutate(simulation = as.integer(simulation))

  prob_champion <- champion %>%
    dplyr::group_by(team) %>%
    dplyr::summarise(
      simulations = n.sim,
      qualified = n(),
      champion = qualified / simulations
    ) %>%
    dplyr::arrange(desc(champion))


  tibble::tibble(team = unique(pgames$team)) %>%
    dplyr::left_join(prob_playoff, by = "team") %>%
    dplyr::left_join(prob_final4, by = "team") %>%
    dplyr::left_join(prob_final, by = "team") %>%
    dplyr::left_join(prob_champion, by = "team") %>%
    dplyr::select(team, playoffs, final4, final, champion) %>%
    dplyr::mutate_all(.funs = ~ ifelse(is.na(.), 0, .)) %>%
    dplyr::arrange(
      dplyr::desc(champion),
      dplyr::desc(final),
      dplyr::desc(final4),
      dplyr::desc(playoffs)
    )
}
