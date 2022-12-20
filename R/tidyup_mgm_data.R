tidyup_mgm_data <- function(mgm_data, sport, prop = FALSE, game_lines = FALSE,
                                  key = get_key_path(sport = sport, prop = prop, game_lines = game_lines)) {


  if (nrow(mgm_data) < 1) stop('no mgm ', prop, ' available')

  # make the output from the input
  output_df <- mgm_data

  # game_lines
  if (game_lines == TRUE) {
    # # fix the totals first
    # totals <- output_df[output_df$Type == 'Total Points', ]
    # new_totals_list <- list()
    # for (m in unique(totals$matchup)) {
    #   mu <- totals[totals$matchup == m, ]
    #   teams <- unlist(strsplit(m, ' @ '))
    #   mu$participantName[[1]] <- teams[[1]]
    #   mu$participantName[[2]] <- teams[[2]]
    #   new_totals_list[[length(new_totals_list) + 1]] <- mu
    # }
    # new_totals <- dplyr::bind_rows(new_totals_list)
    # output_df <- dplyr::bind_rows(new_totals, output_df[output_df$Type != 'Total Points', ])
    # output_df$tidyteam <- normalize_names(as.character(output_df$participantName), key = key)
    # output_df$tidyplayer <- 'team'
    # output_df$tidytype <- gsub(' Points|Point ', '', as.character(output_df$Type))
    # output_df$tidyline <- as.numeric(output_df$line)
    # output_df$tidyou <- ifelse(output_df$tidytype == 'Total', tolower(output_df$label), NA_character_)
    # output_df$tidyamericanodds <- as.numeric(output_df$oddsAmerican)
  }

  # for each prop, append tidy team, tidy opponent, tidy odds (numeric american odds)
  if (prop %in% c('first team to score', 'ftts')) {
    # # generate tidy names and odds
    # output_df$tidyteam <- normalize_names(output_df$label, key = key)
    # if (!all(nchar(output_df$tidyteam) == 3)) {
    #   split_teams <- strsplit(output_df$matchup, ' @ ')
    #   away_teams <- unlist(lapply(split_teams, '[[', 1))
    #   home_teams <- unlist(lapply(split_teams, '[[', 2))
    #   team_name <- ifelse(output_df$label == 1, home_teams, away_teams)
    #   output_df$tidyteam <- normalize_names(as.character(team_name), key = key)
    # }
    # output_df$tidyplayer <- 'team'
    # output_df$tidyamericanodds <- as.numeric(output_df$oddsAmerican)
    # # since prop arg is flexible, set it here for output
    # output_df$prop <- 'first team to score'
  }

  if (prop %in% c('first player to score', 'fpts')) {
    hacky_player_names <- hacky_tidyup_player_names(output_df$name.value)
    output_df$tidyplayer <- normalize_names(hacky_player_names, key = key)
    output_df$tidyamericanodds <- as.numeric(output_df$americanOdds)
    output_df$prop <- 'first player to score'
  }
  if (prop %in% c('fpts by team')) {
    hacky_player_names <- hacky_tidyup_player_names(output_df$name.value)
    output_df$tidyplayer <- normalize_names(hacky_player_names, key = key)
    output_df$tidyamericanodds <- as.numeric(output_df$americanOdds)
    output_df$prop <- 'first player to score by team'
  }
  if (prop %in% c('fpts shot points')) {
    split_names <- strsplit(output_df$name.value, ' - ')
    player_name <- unlist(lapply(split_names, '[[', 1))
    player_name <- hacky_tidyup_player_names(player_name)
    player_name <- gsub('twopointer$', '', player_name)
    player_name <- gsub('threepointer$', '', player_name)
    output_df$tidyplayer <- normalize_names(player_name, key = key)
    output_df$tidyshot_points <- ifelse(grepl('Two|two', output_df$name.value), 2, 3)
    output_df$tidyamericanodds <- as.numeric(output_df$americanOdds)
    output_df$prop <- 'fpts shot points'
  }

  # tidyup the matchup! use the team abbreviations from the lookup
  matchup_list <- strsplit(output_df$matchup, ' @ ')
  output_df$tidyawayteam <- normalize_names(unlist(lapply(matchup_list, '[[', 1)), key = get_key_path(sport, 'team'))
  output_df$tidyhometeam <- normalize_names(unlist(lapply(matchup_list, '[[', 2)), key = get_key_path(sport, 'team'))

  # tidyup the date! make sure this is EST
  output_df$tidygamedatetime <- lubridate::as_datetime(output_df$tipoff) - lubridate::hours(4)
  output_df$tidygamedatetime <- lubridate::round_date(output_df$tidygamedatetime, "30 minutes")
  lubridate::tz(output_df$tidygamedatetime) <- 'EST'

  # keep the tidy columns
  names_to_keep <- names(output_df)[grepl('tidy|prop', names(output_df))]
  output_df <- output_df[, names(output_df) %in% names_to_keep]

  # stamp it up
  output_df$site <- 'mgm'
  output_df$sport <- sport
  if (!'prop' %in% names(output_df)) {
    output_df$prop <- prop
  }

  return(output_df)
}
