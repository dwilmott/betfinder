parse_caesars_prop <- function(game_event, prop_name = FALSE, prop_regex = NULL, prop_not_regex = NULL, matchup, tipoff) {

  label_vec <- unlist(lapply(game_event$markets, '[[', 'name'))

  if (prop_name != FALSE & prop_name %in% label_vec) {
    markets <- game_event$markets[[which(label_vec == prop_name)]]
    if (markets$active == FALSE) return()
    prop_content <- markets$selections
    outcomes_list <- list()
    for (i in prop_content) {
      new_row <- data.frame(
        name = i[['name']],
        price = i[['price']][['a']]
      )
      outcomes_list[[length(outcomes_list) + 1]] <- new_row
    }
    outcomes_df <- dplyr::bind_rows(outcomes_list)

  }
  # if (!is.null(prop_regex)) {
  #   prop_content <- game_event$games[grepl(prop_regex, label_vec), ]
  #   if (!is.null(prop_not_regex)) {
  #     prop_content <- prop_content[!grepl(prop_not_regex, prop_content$name.value), ]
  #   }
  #
  #   outcomes_df <- dplyr::bind_rows(prop_content$results)
  # }

  if (!('outcomes_df' %in% ls())) return()
  outcomes_df$matchup <- matchup
  outcomes_df$tipoff <- tipoff
  return(outcomes_df)
}
