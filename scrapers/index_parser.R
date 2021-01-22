index_parser <- function(season, league){
  ## Scrapes the index data
  player_index_data <- 
    index_scraper(
      season = season,
      league = league,
      data_type = "ratings") %>% 
    left_join(
      index_scraper(
        season = season,
        league = league,
        data_type = "playerstats") %>% 
        select(-PId, -Pos, -Team),
      by = "Name"
    ) %>%
    left_join(
      index_scraper(
        season = season,
        league = league,
        data_type = "advstats") %>% 
        select(-PId, -Pos, -Team),
      by = "Name"
    ) %>%  
    ## Creates a converted name that has removed all foreign characters
    mutate(clean_name = stri_trans_general(Name, "Latin-ASCII"))
  
  ## Scrapes the index data
  goalie_index_data <- 
    index_scraper(
      season = season,
      league = league,
      data_type = "ratings",
      player = FALSE) %>% 
    left_join(
      index_scraper(
        season = season,
        league = league,
        data_type = "playerstats",
        player = FALSE) %>% 
        select(-PId, -Pos, -Team),
      by = "Name"
    ) %>% 
    ## Creates a converted name that has removed all foreign characters
    mutate(clean_name = stri_trans_general(Name, "Latin-ASCII"))
  
  index_data <- goalie_index_data %>% 
    full_join(player_index_data)
  
  return(index_data)
}
