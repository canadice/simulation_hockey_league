### Scrapes all the data from the player pages
data_scraper <- function(player){
  base_link <- "https://simulationhockey.com/"
  
  ## Reads the information from the player page, not that it is a thread with multiple posts
  player_data <- XML::htmlTreeParse(RCurl::getURL(paste(base_link, player, sep = "")), 
                                    asText = TRUE, 
                                    useInternalNodes = TRUE)
  
  ## Scrapes the team name for the player
  team_name <- XML::xpathApply(doc = player_data, 
                               path = "//div[@class='navigation']/a", 
                               fun = XML::xmlValue) %>% 
    unlist()
  
  ## Scrapes the draft class of the player
  season <- unlist(XML::xpathApply(doc = player_data,
                                   path = "//td[@class='thead']/div/strong",
                                   fun = XML::xmlValue)) %>% 
    stringr::str_extract_all(pattern = "S[0-9]+") %>% 
    unlist()
  
  if(length(season)==0){
    season <- "Unspecified"
  }
  
  ## Scrapes the total accrued TPE for the player
  tpe <- unlist(XML::xpathApply(doc = player_data,
                                path = "//td[@class='thead']/div/small",
                                fun = XML::xmlValue)) %>% 
    data_parser()
  
  ## Scrapes the user data of the user that created the post, i.e. the player "owner"
  user_data <- XML::xpathApply(doc = player_data,
                               path = "//div[@id='one']",
                               fun = XML::xmlValue)[[1]] %>% 
    stringr::str_split(pattern = "\n") %>% 
    unlist() %>% 
    stringi::stri_remove_empty() %>% 
    user_data_parser()
  
  ## Scrapes the post data from the thread
  ## Issues arise when the user has posted information in another format or 
  ## if another post in the thread includes a player update. 
  ## This is solved with two separate paths in the data parser
  player_information <- XML::xpathApply(doc = player_data,
                               path = "//div[@id='two']/div/span",
                               fun = XML::xmlValue) %>% 
    unlist()
  
  if(length(player_information) > 60 & 
     sum(player_information == "Player Information")==1){
    player_information <- player_information %>% 
      player_data_parser_long()
  } else {
    player_information <- 
      XML::xpathApply(
        doc = player_data,
        path = "//div[@id='two']",
        fun = XML::xmlValue)[[1]] %>% 
      stringr::str_split(pattern = "\n") %>% 
      unlist() %>% 
      stringi::stri_remove_empty() %>% 
      player_data_parser_short()
    
  }
  
  ## Changes the headers if they differ from the norm
  colnames(player_information)[colnames(player_information)=="Shoots"] <- "Handedness"
  colnames(player_information)[colnames(player_information)=="Height (ft.)"] <- "Height"
  colnames(player_information)[colnames(player_information)=="Weight (lbs.)"] <- "Weight"
  colnames(player_information)[colnames(player_information)=="Born"] <- "Birth Date"
  colnames(player_information)[colnames(player_information)=="DOB"] <- "Birth Date"
  
  if(!any(colnames(player_information) %>% stringr::str_detect("Player Render"))){
    player_information$`Player Render` <- NA
  }
  
  if(any(colnames(player_information) %>% stringr::str_detect("Birth Date"))){
    player_information <- player_information %>% 
      select(-`Birth Date`)
  }
  if(any(colnames(player_information) %>% stringr::str_detect("Name for Sim-Engine"))){
    player_information <- player_information %>% 
      select(-`Name for Sim-Engine`)
  }
  if(any(colnames(player_information) %>% stringr::str_detect("Former Username"))){
    player_information <- player_information %>% 
      select(-`Former Username`)
  }
  if(any(colnames(player_information) %>% stringr::str_detect("^Username"))){
    player_information <- player_information %>% 
      select(-Username)
  }
  
  return(data.frame(team = team_name[3], 
                    user_data, 
                    season = season, 
                    tpe = tpe[2], 
                    player_information))
  
}
