get_fd_playerids <- function(sport) {

  fd_data <- get_fanduel_data(sport)

  event_output <- list()

  for (fd in fd_data) {

    tab_names <- names(fd)
    player_tabs <- tab_names[grepl('player_', tab_names)]
    if (length(player_tabs) == 0) next
    for (tab in player_tabs) {
      tab_content <- fd[[tab]]
      attachments <- tab_content$attachments
      markets <- attachments$markets

    }


    offer_output <- list()
    for (o in componentized_offers) {
      subcategory <- o$subcategoryName
      offers <- o$offers[[1]]
      players_and_subcats <- unlist(lapply(offers, '[[', 'label'))
      player_ids <- unlist(lapply(offers, '[[', 'fdPlayerId'))
      offer_output[[length(offer_output) + 1]] <- data.frame(
        label = subcategory,
        players_and_subcats = players_and_subcats,
        player_name = gsub(paste0(' ', subcategory), '', players_and_subcats),
        fd_playerid = player_ids
      )
    }
    offer_output_df <- dplyr::bind_rows(offer_output)

    event_output_df <- offer_output_df %>%
      dplyr::select(player_name, fd_playerid) %>%
      dplyr::distinct()

    event_output[[length(event_output) + 1]] <- event_output_df

  }

  final <- dplyr::bind_rows(event_output) %>%
    dplyr::mutate(str_len = nchar(player_name)) %>%
    dplyr::group_by(fd_playerid) %>%
    dplyr::filter(str_len == min(str_len)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-str_len) %>%
    dplyr::distinct()

  return(final)

}

