### Function for parsing user data from a player thread
user_data_parser <- function(user_data){
  
  user <- 
    stringr::str_remove(
      user_data[1],
      pattern = "Registered|Player Updaters|Rookie|Vancouver Whalers|SHL GM|Quebec City Citadelles|Head Office|Coach|Budget Director|Graphic Graders|IIHF Federation Head|Moderators|Fantasy League Manager|SMJHL GM|Simmer|Calgary Dragons|Head Updater|Buffalo Stampede|Owner|Media Graders|Bank Manager|Simmer|Mentor|Comissioner|HOF Committee|Chicago Syndicate|Team |Detroit Falcons|Awards Committee|SMJHL |SHL |Manhattan Rage|SMJHL HO|Edmonton Blizzard|Los Angeles Panthers|Baltimore Platoon|Site |All-Star Committee")
  
  member_status <- user_data[2]
  
  nr_posts <- stringr::str_extract_all(user_data[3], 
                                       pattern = "[0-9,]+") %>% 
    unlist()
  
  nr_threads <- stringr::str_extract_all(stringr::str_split(user_data[4], 
                                                            pattern = " Joined: ", 
                                                            simplify = TRUE)[1], 
                                         pattern = "[0-9,]+") %>% 
    unlist()
  
  joined <- stringr::str_split(user_data[4], 
                               pattern = " Joined:  ", 
                               simplify = TRUE)[2]
  
  reputation <- user_data[6]
  
  user_data <- data.frame(user, 
                          member_status,
                          nr_posts,
                          nr_threads,
                          joined,
                          reputation)
  
  return(user_data)
  
}