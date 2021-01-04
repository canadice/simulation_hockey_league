### Function that scrapes the index pages of the SHL site for chosen season and league
index_scraper <- function(season = "57", 
                          league = "smjhl", 
                          data_type = "ratings",
                          player = TRUE){
  ## Checks if season input is character, otherwise converts it to character
  if(!is.character(season)){
    season = as.character(season)
  }
  
  ## Creates the link to the relevant index page
  link <- paste("https://simulationhockey.com/", data_type, ".php?league=", league, "&season=", season, "&st=rs", sep = "")
  
  ## Scrapes the information from the index page
  index_page <- XML::htmlTreeParse(RCurl::getURL(link), asText = TRUE, useInternalNodes = TRUE)
  
  ## Checks which data type is asked for and checks the page for the according HTML tag
  # Loads the player data 
  if(data_type == "ratings" &&
     player){
    type <- "skaterratings"
  } else if(data_type == "ratings" &&
            !player){
    type <- "goalieratings"
  } else if(player){
    type <- "skaterdata"
  } else {
    type <- "goaliedata"
  }
  
  ## Scrapes the headers (variable names)
  header <- XML::xpathApply(doc = index_page, 
                            path = paste("//table[@id=\"", 
                                         type, 
                                         "\"]/thead/tr/th", 
                                         sep = ""), 
                            fun = XML::xmlValue) %>% 
    unlist()
  
  ## Scrapes the actual data and summarizes everything in a table
  data_table <- XML::xpathApply(doc = index_page, 
                           path = paste("//table[@id=\"", 
                                        type, 
                                        "\"]/tbody/tr/td", 
                                        sep = ""), 
                           fun = XML::xmlValue) %>% 
    unlist() %>% 
    matrix(ncol = length(header), 
           byrow = TRUE) %>% 
    as.data.frame()
  
  colnames(data_table) <- header
  
  
  ## Makes some formatting alterations to the variables
  data_table <- data_table %>% 
    mutate(PId = as.numeric(PId),
           Team = factor(Team), 
           Name = factor(Name), 
           Pos = factor(Pos, 
                        levels = c("LD", 
                                   "RD", 
                                   "LW", 
                                   "RW", 
                                   "C",
                                   "G")
                        )
           ) 
  
  if(data_type == "playerstats"){
    data_table <- data_table %>% 
      mutate(
        across(!(contains("TOI")|contains("%")) & where(is.character), as.numeric),
        across(contains("TOI"), 
               function(x) as.numeric(lubridate::ms(x, 
                                                    roll = TRUE, 
                                                    quiet = TRUE)))
      )
    if(player){
      data_table <- data_table %>% 
        mutate(
          `SH%` = as.numeric(str_remove(`SH%`, "%")),
          across(contains("%"), as.numeric)
        )
    } else {
      data_table <- data_table %>% 
        mutate(
          across(contains("%"), as.numeric)
        )
    }
        
  } else {
    data_table <- data_table %>% 
      mutate(across(where(is.character), as.numeric)) 
  }
    

  ## Apparently index can show values +/- 1 from the true player rating
  ## This if condition mitigates that and changes values to the correct ones for 
  ## all players
    ## I CAN'T FIND A DPLYR FUNCTION FOR THIS SO SEPARATE FUNCTIONS IT IS
  if(data_type == "ratings"){
    num_index <- data_table %>% 
      sapply(is.numeric) %>% 
      which()
    
    less_index <- data_table$DET < 15 
    
    more_index <- data_table$DET > 15 
    
    data_table[less_index,num_index] <- data_table[less_index,num_index]+1
    data_table[more_index,num_index] <- data_table[more_index,num_index]-1
  }
  
  return(data_table)
}

standings_index_scraper <- 
  function(season = "57", 
           league = "smjhl", 
           data_type = "standings"){
  ## Checks if season input is character, otherwise converts it to character
  if(!is.character(season)){
    season = as.character(season)
  }
  
  ## Creates the link to the relevant index page
  link <- paste("https://simulationhockey.com/", data_type, ".php?league=", league, "&season=", season, "&st=rs", sep = "")
  
  ## Scrapes the information from the index page
  index_page <- XML::htmlTreeParse(RCurl::getURL(link), asText = TRUE, useInternalNodes = TRUE)
  
  ## Scrapes the headers (variable names)
  header <- XML::xpathApply(doc = index_page, 
                            path = paste("//table[@id=\"", 
                                         data_type, 
                                         "\"]/thead/tr/th", 
                                         sep = ""), 
                            fun = XML::xmlValue) %>% 
    unlist()
  
  ## Scrapes the actual data and summarizes everything in a table
  data_table <- XML::xpathApply(doc = index_page, 
                                path = paste("//table[@id=\"", 
                                             data_type, 
                                             "\"]/tbody/tr/td", 
                                             sep = ""), 
                                fun = XML::xmlValue) %>% 
    unlist() %>% 
    matrix(ncol = length(header)/2, 
           byrow = TRUE) %>% 
    as.data.frame()
  
  colnames(data_table) <- header[1:(length(header)/2)]
  colnames(data_table)[c(1)] <- "Team"
  
  
  ## Moves the conferences together
  if(league == "shl"){
    data_table <- data_table %>% 
      mutate(across(GP:'Win%', as.numeric)) %>% 
      mutate(Conference = rep(c("East", "West"), each = 9),
             GD = GF-GA,
             'W%' = round(W/GP, 3)) %>% 
      mutate(across(where(is.character), as.factor)) %>% 
      rename('Pts%' = 'Win%',
             'Win%' = 'W%') %>% 
      relocate(Conference, Team)
    
  } else {
    data_table <- data_table %>% 
      mutate(across(GP:'Win%', as.numeric)) %>% 
      mutate(Conference = rep(c("East", "West"), each = 6),
             GD = GF-GA,
             'W%' = round(W/GP, 3)) %>% 
      mutate(across(where(is.character), as.factor)) %>% 
      rename('Pts%' = 'Win%',
             'Win%' = 'W%') %>% 
      relocate(Conference, Team)
  }
  
  
  return(data_table)
}

schedule_index_scraper <- 
  function(season = "57", 
           league = "smjhl", 
           data_type = "schedule"){
    ## Checks if season input is character, otherwise converts it to character
    if(!is.character(season)){
      season = as.character(season)
    }
    
    ## Creates the link to the relevant index page
    link <- paste("https://simulationhockey.com/", data_type, ".php?league=", league, "&season=", season, "&st=rs", sep = "")
    
    ## Scrapes the information from the index page
    index_page <- XML::htmlTreeParse(RCurl::getURL(link), asText = TRUE, useInternalNodes = TRUE)
    
    ## Scrapes the headers (variable names)
    header <- XML::xpathApply(doc = index_page, 
                              path = paste("//table[@id=\"", 
                                           data_type, 
                                           "\"]/thead/tr/th", 
                                           sep = ""), 
                              fun = XML::xmlValue) %>% 
      unlist()
    
    ## Scrapes the actual data and summarizes everything in a table
    data <- XML::xpathApply(doc = index_page, 
                                  path = paste("//table[@id=\"", 
                                               data_type, 
                                               "\"]/tbody/tr/td", 
                                               sep = ""), 
                                  fun = XML::xmlValue) %>% 
      unlist() %>% 
      matrix(ncol = length(header), 
             byrow = TRUE) %>% 
      as.data.frame()
    
    colnames(data) <- header
    
    ## Moves the conferences together
    data_table <- data %>% 
      select(-'Game link') %>% 
      mutate(across(contains("Score"), as.numeric),
             across(Overtime:Shootout, str_detect, pattern = "X")) %>% 
      mutate(Played = 
               case_when(
                 `Away Score` != 0 | `Home Score` != 0 ~ TRUE,
                 TRUE ~ FALSE
               ),
             AwayPts = 
               case_when(
                 `Away Score` > `Home Score` ~ 2,
                 `Away Score` < `Home Score` & (Overtime | Shootout) ~ 1,
                 TRUE ~ 0
               ),
             HomePts = 
               case_when(
                 `Away Score` < `Home Score` ~ 2,
                 `Away Score` > `Home Score` & (Overtime | Shootout) ~ 1,
                 TRUE ~ 0
               )
             )
    
    return(data_table)
  }


