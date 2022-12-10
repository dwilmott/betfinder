get_dk_playerids <- function(sport) {

  dk_data <- get_draftkings_data(sport)

  event_output <- list()

  for (dk in dk_data) {

    categories <- dk$eventCategories
    category_names <- unlist(lapply(categories, '[[', 'name'))
    if (!'Player Props' %in% category_names) next
    player_props <- categories[[which(category_names == 'Player Props')]]
    componentized_offers <- player_props$componentizedOffers

    offer_output <- list()
    for (o in componentized_offers) {
      subcategory <- o$subcategoryName
      offers <- o$offers[[1]]
      players_and_subcats <- unlist(lapply(offers, '[[', 'label'))
      player_ids <- unlist(lapply(offers, '[[', 'dkPlayerId'))
      offer_output[[length(offer_output) + 1]] <- data.frame(
        label = subcategory,
        players_and_subcats = players_and_subcats,
        player_name = gsub(paste0(' ', subcategory), '', players_and_subcats),
        dk_playerid = player_ids
      )
    }
    offer_output_df <- dplyr::bind_rows(offer_output)

    event_output_df <- offer_output_df %>%
      dplyr::select(player_name, dk_playerid) %>%
      dplyr::distinct()

    event_output[[length(event_output) + 1]] <- event_output_df

  }

  final <- dplyr::bind_rows(event_output) %>%
    dplyr::mutate(str_len = nchar(player_name)) %>%
    dplyr::group_by(dk_playerid) %>%
    dplyr::filter(str_len == min(str_len)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-str_len) %>%
    dplyr::distinct()

  return(final)

}

