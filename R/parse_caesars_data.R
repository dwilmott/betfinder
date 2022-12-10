parse_caesars_data <- function(caesars_data, sport, prop = FALSE, game_lines = FALSE) {

  # loop through caesars_data and extract the correct prop
  output_list <- list()
  for (e in 1:length(caesars_data)) {

    # skip the tmnt bets for now
    if (caesars_data[[e]]$type == 'TMNT') next

    # subset the game event
    game_event <- caesars_data[[e]]
    matchup <- game_event$name
    tipoff <- game_event$startTime

    # get the game lines if you're trying to do that
    # if (game_lines == TRUE) {
    #   gl_out <- parse_caesars_main(game_event = game_event, matchup = matchup, tipoff = tipoff)
    #   output_list[[length(output_list) + 1]] <-
    #     parse_caesars_main(game_event = game_event, matchup = matchup, tipoff = tipoff)
    #   next
    # }

    # extract correct props
    if (prop %in% c('first team to score', 'ftts')) {
      output_list[[length(output_list) + 1]] <-
        parse_caesars_prop(game_event = game_event, prop_name = "Next Team to Score - at Score 0-0",
                       matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('first player to score', 'fpts')) {
      output_list[[length(output_list) + 1]] <-
        parse_caesars_prop(game_event = game_event, prop_name = '|First Scorer|',
                       matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('fpts by team')) {
      output_list[[length(output_list) + 1]] <-
        parse_caesars_prop(game_event = game_event,
                       prop_regex = 'First Field Goal Scorer -', prop_not_regex = 'Exact Method',
                       matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('fpts exact method')) {
      output_list[[length(output_list) + 1]] <-
        parse_caesars_prop(game_event = game_event,
                       prop_name = 'First Field Goal - Exact Method',
                       matchup = matchup, tipoff = tipoff)
    }
  }

  # if output_list is empty, error, else return as a data.frame
  if (length(output_list) == 0) stop('no caesars ', prop, ' props returned')
  output_df <- dplyr::bind_rows(output_list)

  return(output_df)
}

