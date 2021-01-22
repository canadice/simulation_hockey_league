require(XML)
require(ggplot2)
require(stringr)
require(tidyr)
require(plyr)
require(dplyr)
require(RColorBrewer)
require(lubridate)
require(stringi)
require(parallel)
require(fuzzyjoin)

setwd("./shl_player_database")
# source("../scrapers/scraper_functions.R")
source("../scrapers/team_scraper.R")
source("../scrapers/player_scraper.R")

source("../scrapers/data_scraper.R")
source("../scrapers/data_parser.R")

source("../scrapers/index_scraper.R")
source("../scrapers/index_parser.R")

source("../scrapers/user_data_parser.R")
source("../scrapers/player_data_parser.R")

smjhl_link <- "https://simulationhockey.com/forumdisplay.php?fid=5"
shl_east <- "https://simulationhockey.com/forumdisplay.php?fid=8"
shl_west <- "https://simulationhockey.com/forumdisplay.php?fid=9"


error_scraper <- function(x){
  player_information <- NULL
  attempt <- 0
  while(is.null(player_information) && attempt < 5){
    attempt <- attempt + 1
    # print(x)
    try(
      player_information <- data_scraper(x)  
    )
  }
  return(player_information)
}

smjhl <- function(){
  player_links <- 
    smjhl_link %>%
    team_scraper() %>% 
    player_scraper() %>% 
    unique()
  
  cl <- makeCluster(getOption("cl.cores", 4))
  
  clusterExport(cl, 
                varlist = c("smjhl_link",
                            "team_scraper",
                            "player_scraper",
                            "data_scraper",
                            "%>%",
                            "data_parser",
                            "user_data_parser",
                            "player_data_parser_long",
                            "player_data_parser_short",
                            "error_scraper"))
  
  player_data <- lapply(player_links, FUN = error_scraper)
  
  stopCluster(cl)
  
  player_data <- do.call(rbind.fill, player_data)
  
  return(player_data)
}

shl <- function(){
  player_links <- 
    c(shl_east, shl_west) %>%
    team_scraper() %>% 
    player_scraper() %>% 
    unique()
  
  cl <- makeCluster(getOption("cl.cores", 4))
  
  clusterExport(cl, 
                varlist = c("smjhl",
                            "team_scraper",
                            "player_scraper",
                            "data_scraper",
                            "%>%",
                            "data_parser",
                            "user_data_parser",
                            "player_data_parser_long",
                            "player_data_parser_short",
                            "error_scraper"))
  
  player_data <- lapply(player_links, FUN = error_scraper)
  
  stopCluster(cl)
  
  player_data <- do.call(rbind.fill, player_data)
  
  return(player_data)
}


{
  start_time <- Sys.time()
  
  players <- smjhl()
  # player_information <- NULL
  # attempt <- 0
  # while(is.null(player_information) && attempt < 40){
  #   attempt <- attempt + 1
  #   try(
  #     player_information <- test()  
  #   )
  # }
  
  player_information <- players %>% 
    mutate(
      clean_name = 
        stringi::stri_trans_general(
          paste(First.Name, 
                Last.Name, sep = " "
                ),
          id = "Latin-ASCII"
          )
      ) %>% 
    mutate(clean_name = 
             case_when(
               clean_name == "James \"Jimmy\" Yzerman" ~ "James Yzerman",
               clean_name == "Asclepius Perseus Flitterwind" ~ "Asclepius Perseus Flitter",
               clean_name == "Hennesey-Gallchobhar O'McGuiness" ~ "Hennesey-Gallchobhar O'Mc",
               clean_name == "Terrence \"Big Terry\" Smith" ~ "Terrence Smith",
               clean_name == "Ragnar-Alexandre Ragnarsson-Tremblay" ~ "Ragnar-Alexandre Ragnarss",
               TRUE ~ clean_name
             )
           ) %>% 
    mutate(clean_name = str_squish(clean_name)) %>% 
    ## Formats the variables 
    ## Uses standard names for positions
    ## Transforms some variables to numeric
    mutate(Position = 
             case_when(
               Position == "Goaltender" ~ "Goalie",
               Position == "LW" ~ "Left Wing",
               Position == "RHD" ~ "Right Defense",
               Position == "Right Winger" ~ "Right Wing",
               TRUE ~ Position
             ),
           nr_posts = as.numeric(str_remove_all(nr_posts, pattern = "[^0-9]")),
           nr_threads = as.numeric(str_remove_all(nr_threads, pattern = "[^0-9]")),
           reputation = as.numeric(str_remove_all(reputation, pattern = "[^0-9]")),
           tpe = as.numeric(str_remove_all(tpe, pattern = "[^0-9]")),
           Jersey.Number = as.numeric(str_remove_all(Jersey.Number, pattern = "[^0-9]")),
           Weight.lb = as.numeric(str_remove_all(Weight, pattern = "[^0-9]")),
           Weight.kg = as.numeric(str_remove_all(Weight, pattern = "[^0-9]"))*0.453592,
           Height.in = matrix(as.numeric(str_extract_all(Height, 
                                                         pattern = "[0-9]+", 
                                                         simplify = TRUE)),
                              ncol = 2)%*%matrix(c(12,1), nrow = 2),
           Height.cm = matrix(as.numeric(str_extract_all(Height, 
                                                         pattern = "[0-9]+", 
                                                         simplify = TRUE)),
                              ncol = 2)%*%matrix(c(12,1), nrow = 2)*2.54
    ) %>% 
    select(-Weight, -Height)
  
  ## Scrapes the index data
  player_index_data <- 
    index_scraper(
      season = "57",
      league = "smjhl",
      data_type = "ratings") %>% 
    left_join(
      index_scraper(
        season = "57",
        league = "smjhl",
        data_type = "playerstats") %>% 
        select(-PId, -Pos, -Team),
      by = "Name"
    ) %>%
    left_join(
      index_scraper(
        season = "57",
        league = "smjhl",
        data_type = "advstats") %>% 
        select(-PId, -Pos, -Team),
      by = "Name"
    ) %>%
    ## Creates a converted name that has removed all foreign characters
    mutate(clean_name = stri_trans_general(Name, "Latin-ASCII"))
  
  ## Scrapes the index data
  goalie_index_data <- 
    index_scraper(
      season = "57",
      league = "smjhl",
      data_type = "ratings",
      player = FALSE) %>% 
    left_join(
      index_scraper(
        season = "57",
        league = "smjhl",
        data_type = "playerstats",
        player = FALSE) %>% 
        select(-PId, -Pos, -Team),
      by = "Name"
    ) %>% 
    ## Creates a converted name that has removed all foreign characters
    mutate(clean_name = stri_trans_general(Name, "Latin-ASCII"))
  
  index_data <- goalie_index_data %>% 
    full_join(player_index_data)
  
  ## Merges the two data sets
  smjhl_data <- player_information %>% 
    stringdist_full_join(index_data,
              by = c("clean_name"),
              max_dist = 3) %>% 
    ## Removes the join variable
    select(-clean_name.x, -clean_name.y) %>% 
    group_split(Position == "Goalie", .keep = FALSE)
  
  ## Removes the columns with only NA from each of the two lists
  smjhl_data <- lapply(smjhl_data, function(y) y %>% select(where(function(x) sum(!is.na(x))>0)) )
  
  names(smjhl_data) <- c("players", "goalies")
  
  smjhl_data$schedule <- schedule_index_scraper()
  smjhl_data$standings <- standings_index_scraper()
  
  
  end_time <- Sys.time()
  end_time - start_time
}

{
  start_time <- Sys.time()
  
  players <- shl()
  # player_information <- NULL
  # attempt <- 0
  # while(is.null(player_information) && attempt < 40){
  #   attempt <- attempt + 1
  #   try(
  #     player_information <- test()  
  #   )
  # }
  
  player_information <- players %>% 
    mutate(
      clean_name = 
        stringi::stri_trans_general(
          paste(First.Name, 
                Last.Name, sep = " "
          ),
          id = "Latin-ASCII"
        )
    ) %>% 
    mutate(clean_name = 
             case_when(
               clean_name == "Gottlieb-Odilo-Dankward McZehrl" ~ "GOD McZehrl",
               clean_name == "Luc-Pierre Lespineau-Lebrunette" ~ "Luc-Pierre Lespineau-Lebr",
               clean_name == "Kalevolaripaavo Kaspertommevisnapuu" ~ "Kalevolaripaavo Kaspertom",
               clean_name == "Jean-Uhtred Ragnarsson-Tremblay" ~ "Jean-Uhtred Ragnarsson-Tr",
               clean_name == "Philipp Alexander Winter" ~ "Philipp Winter",
               TRUE ~ clean_name
             )
    ) %>% 
    mutate(clean_name = str_squish(clean_name)) %>% 
    ## Formats the variables 
    ## Uses standard names for positions
    ## Transforms some variables to numeric
    mutate(Position = 
             case_when(
               Position == "Goaltender" ~ "Goalie",
               Position == "LW" ~ "Left Wing",
               Position == "RHD" ~ "Right Defense",
               Position == "Right Winger" ~ "Right Wing",
               Position == "D" ~ "Defense",
               Position == "RW" ~ "Right Wing",
               Position == "C" ~ "Center",
               Position == "Defenseman" ~ "Defense",
               Position == "Centre" ~ "Center",
               Position == "D I think..." ~ "Defense",
               Position == "Defense[/size][/font]" ~ "Defense",
               Position == "Left-Wing" ~ "Left Wing",
               TRUE ~ Position
             ),
           nr_posts = as.numeric(str_remove_all(nr_posts, pattern = "[^0-9]")),
           nr_threads = as.numeric(str_remove_all(nr_threads, pattern = "[^0-9]")),
           reputation = as.numeric(str_remove_all(reputation, pattern = "[^0-9]")),
           tpe = as.numeric(str_remove_all(tpe, pattern = "[^0-9]")),
           Jersey.Number = as.numeric(str_remove_all(Jersey.Number, pattern = "[^0-9]")),
           Weight.lb = as.numeric(str_remove_all(Weight, pattern = "[^0-9]")),
           Weight.kg = as.numeric(str_remove_all(Weight, pattern = "[^0-9]"))*0.453592,
           Height.in = matrix(as.numeric(str_extract_all(Height, 
                                                         pattern = "[0-9]+", 
                                                         simplify = TRUE)),
                              ncol = 2)%*%matrix(c(12,1), nrow = 2),
           Height.cm = matrix(as.numeric(str_extract_all(Height, 
                                                         pattern = "[0-9]+", 
                                                         simplify = TRUE)),
                              ncol = 2)%*%matrix(c(12,1), nrow = 2)*2.54
    ) %>% 
    select(-Weight, -Height)
  
  ## Scrapes the index data
  player_index_data <- 
    index_scraper(
      season = "57",
      league = "shl",
      data_type = "ratings") %>% 
    left_join(
      index_scraper(
        season = "57",
        league = "shl",
        data_type = "playerstats") %>% 
        select(-PId, -Pos, -Team),
      by = "Name"
    ) %>%
    left_join(
      index_scraper(
        season = "57",
        league = "shl",
        data_type = "advstats") %>% 
        select(-PId, -Pos, -Team),
      by = "Name"
    ) %>%  
    ## Creates a converted name that has removed all foreign characters
    mutate(clean_name = stri_trans_general(Name, "Latin-ASCII"))
  
  ## Scrapes the index data
  goalie_index_data <- 
    index_scraper(
      season = "57",
      league = "shl",
      data_type = "ratings",
      player = FALSE) %>% 
    left_join(
      index_scraper(
        season = "57",
        league = "shl",
        data_type = "playerstats",
        player = FALSE) %>% 
        select(-PId, -Pos, -Team),
      by = "Name"
    ) %>% 
    ## Creates a converted name that has removed all foreign characters
    mutate(clean_name = stri_trans_general(Name, "Latin-ASCII"))
  
  index_data <- goalie_index_data %>% 
    full_join(player_index_data)
  
  ## Merges the two data sets
  shl_data <- player_information %>% 
    stringdist_full_join(index_data,
                         by = c("clean_name"),
                         max_dist = 3) %>% 
    ## Removes the join variable
    select(-clean_name.x, -clean_name.y) %>% 
    group_split(Position == "Goalie", .keep = FALSE)
  
  ## Removes the columns with only NA from each of the two lists
  shl_data <- lapply(shl_data, function(y) y %>% select(where(function(x) sum(!is.na(x))>0)) )
  
  names(shl_data) <- c("players", "goalies")
  
  shl_data$schedule <- schedule_index_scraper(league = "shl")
  shl_data$standings <- standings_index_scraper(league = "shl")
  
  
  end_time <- Sys.time()
  end_time - start_time
}

date_scraped <- today()

save(list = c("date_scraped", "shl_data", "smjhl_data"), file = paste("data/SHL&SMJHL ", today(),".RData", sep = ""))

