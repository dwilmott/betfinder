get_mgm_data <- function(sport, save_path = NULL, sleep_time = 0) {

  # set the main_URI by sport
  if (sport == 'nba') {
    today_no_dashes <- gsub('-', '', as.character(Sys.Date()))
    fixture_path <- paste0('https://raw.githubusercontent.com/jimtheflash/gambling_stuff/main/data/01_raw/mgm_nba_fixture_ids/',
                           today_no_dashes,
                           '.csv')
    fixtures <- unlist(read.csv(fixture_path))

  } else {
    stop('sport not supported')
  }
  # loop through those files and output some lists of event lists
  event_list <- list()
  for (f in fixtures) {
    ## this is a clunky fix and requires that we're getting fixture id's consistently, and also that mgm isn't changing up their api's on us
    game_event <- jsonlite::fromJSON(paste0('https://cds-api.itsfogo.com/bettingoffer/fixture-view?x-bwin-accessid=ZTg4YWEwMTgtZTlhYy00MWRkLWIzYWYtZjMzODI5ZDE0Mjc5&lang=en-us&country=US&userCountry=US&offerMapping=All&scoreboardMode=Full&state=Latest&fixtureIds=', f), flatten = TRUE)
    event_list[[length(event_list) + 1]] <- game_event

    if (!is.null(save_path)) {
      e <- gsub('01_raw', '', p)
      e <- gsub('[^0-9]', '', e)
      fn <- paste0(sport, '_mgm_', e, '_', as.numeric(Sys.time()), '.json')
      jsonlite::write_json(game_event, file.path(save_path, fn))
      R.utils::gzip(file.path(save_path, fn), ext='gz')
    }
  }

  # return as a list of lists (yikes!)
  return(event_list)
}
