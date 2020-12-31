# # Scrapes the data from a player thread
# player_data_scraper <- function(player_link){
#   player_data <- XML::htmlTreeParse(RCurl::getURL(paste(base_link, player_link, sep = "")), 
#                                     asText = TRUE, 
#                                     useInternalNodes = TRUE)
#   
#   team_name <- XML::xpathApply(doc = player_data, 
#                                path = "//div[@class='navigation']/a", 
#                                fun = XML::xmlValue) %>% 
#     unlist()
#   
#   season <- unlist(XML::xpathApply(doc = player_data,
#                                    path = "//td[@class='thead']/div/strong",
#                                    fun = XML::xmlValue)) %>% 
#     stringr::str_extract_all(pattern = "S[0-9]+") %>% 
#     unlist()
#   
#   tpe <- unlist(XML::xpathApply(doc = player_data,
#                                 path = "//td[@class='thead']/div/small",
#                                 fun = XML::xmlValue)) %>% 
#     data_parser()
#   
#   user_data <- XML::xpathApply(doc = player_data,
#                                path = "//div[@id='one']",
#                                fun = XML::xmlValue)[[1]] %>% 
#     stringr::str_split(pattern = "\n") %>% 
#     unlist() %>% 
#     stringi::stri_remove_empty() %>% 
#     user_data_parser()
#   
#   temp_data <- XML::xpathApply(doc = player_data,
#                                path = "//div[@id='two']/div/span",
#                                fun = XML::xmlValue) %>% 
#     unlist()
#   
#   # Fixes another parser if the first post has some other format of the text, 
#   # but excludes cases where user has added another post with updated information in the same thread
#   if(length(temp_data) > 45 & sum(temp_data == "Player Information")==1){
#     player_stats <- temp_data %>% 
#       player_stats_parser_2()
#     
#   } else {
#     player_stats <- XML::xpathApply(doc = player_data,
#                                     path = "//div[@id='two']",
#                                     fun = XML::xmlValue)[[1]] %>% 
#       stringr::str_split(pattern = "\n") %>% 
#       unlist() %>% 
#       stringi::stri_remove_empty() %>% 
#       player_stats_parser()
#     
#     ## Checks if player has the unedited attributes
#     unedited <- colnames(player_stats) %in% c("*Aggression", "*Determination", 
#                                              "*Leadership", "*Team Player", 
#                                              "*Professionalism", "*Temperament") 
#     
#     if(sum(unedited) != 5 && 
#        (player_stats$Position != c("Goalie") && 
#         player_stats$Position != c("Goaltender"))
#        ){
#       player_stats <- player_stats %>% 
#         dplyr::mutate('*Determination' = 15,
#                '*Team Player' = 15,
#                '*Leadership' = 15,
#                '*Temperament'= 15,
#                '*Professionalism' = 15)
#     } else if((player_stats$Position == "Goalie" | 
#                player_stats$Position == "Goaltender") &&
#               sum(unedited != 5)){
#       colnames(player_stats)[colnames(player_stats)=="Aggression"] <- "*Aggression"
#     }
#     
#     colnames(player_stats)[colnames(player_stats)=="Shoots"] <- "Handedness"
#     
#   }
#   
#   
#   return(data.frame(team = team_name[3], 
#                     user_data, 
#                     season = season, 
#                     tpe = tpe[2], 
#                     player_stats))
# }
# 
# # Subfunction for specific data where user has formatted the player information differently than the standard
# player_stats_parser_2 <- function(player_stats){
#   start <- which(player_stats == "Player Information")
#   
#   end <- which(player_stats == "*Indicates attributes that cannot be edited.")
#   
#   player_data <- player_stats[start:end] %>% 
#     stringr::str_detect(pattern = ":")
#   
#   player_stats <- player_stats[start:end]
#   
#   birthplace <- min(which(stringr::str_detect(player_stats, 
#                                               pattern = "Birthplace")))
#   
#   data_part <- sapply(X = player_stats[2:birthplace], 
#                       FUN = data_parser) %>% 
#     unlist() %>% 
#     matrix(nrow = 2)
#   
#   colnames(data_part) <- data_part[1,]
#   
#   data_part <- as.data.frame(t(data_part[-1,]))
#   
#   attributes <- sapply(X = player_stats[(birthplace+2):(length(player_stats)-1)], 
#                        FUN = data_parser) %>% 
#     unlist() %>% 
#     stringr::str_replace_all(pattern = c("Offensive Ratings|Mental Ratings|Defensive Ratings|Physical Ratings"), 
#                              replacement = "") %>% 
#     stringi::stri_remove_empty() %>% 
#     stringr::str_squish() %>% 
#     matrix(nrow = 2)
#   
#   colnames(attributes) <- attributes[1,]
#   
#   attributes <- attributes[-1,] %>% 
#     sapply(as.numeric) %>% 
#     t() %>% 
#     as.data.frame()
#   
#   return(cbind(data_part, attributes))
# }
# 
# # Subfunction for specific player attributes in a player thread
# player_stats_parser <- function(player_stats){
#   attribute_split <- which(stringr::str_detect(player_stats, 
#                                                pattern = c("Player Attributes[ P]|\\)Player Attributes")))
#   
#   birthplace <- min(which(stringr::str_detect(player_stats, 
#                                               pattern = "Birthplace")))
#   # print(attribute_split)
#   # print(birthplace)
#   end <- min(which(stringr::str_detect(player_stats, 
#                                    pattern = "\\*Indicates attributes that cannot be edited")))
#   
#   if(attribute_split != birthplace){
#     data_part <- sapply(X = as.list(c(player_stats[5:(attribute_split-1)])), 
#                         FUN = data_parser) 
#   } else {
#     data_part <- sapply(X = as.list(c(player_stats[5:(attribute_split-1)], 
#                                       stringr::str_split(player_stats[attribute_split],
#                                                          pattern = "Player Attributes",
#                                                          simplify = TRUE)[1])), 
#                         FUN = data_parser)   
#   }
#   
#   colnames(data_part) <- data_part[1,]
#   
#   data_part <- as.data.frame(t(data_part[-1,]))
#   
#   if(data_part$Position %in% c("Goalie", "Goaltender")){
#     attributes <- sapply(X = as.list(c(stringr::str_split(player_stats[attribute_split],
#                                                           pattern = "Player Attributes",
#                                                           simplify = TRUE)[2], 
#                                        player_stats[(attribute_split+1):(end-1)])), 
#                          FUN = data_parser) %>% 
#       unlist() %>% 
#       stringr::str_replace_all(pattern = c("Goalie Ratings|Mental Ratings"), 
#                                replacement = " ") %>% 
#       stringr::str_split(pattern = "\\s{2,}|\\s\\*") %>% 
#       unlist() %>% 
#       stringr::str_squish() %>% 
#       stringi::stri_remove_empty() %>% 
#       matrix(nrow = 2)
#     
#     colnames(attributes) <- attributes[1,]
#     
#     attributes <- attributes[-1,] %>% 
#       sapply(as.numeric) %>% 
#       t() %>% 
#       as.data.frame()
#   } else {
#     attributes <- sapply(X = as.list(c(stringr::str_split(player_stats[attribute_split],
#                                                           pattern = "Player Attributes",
#                                                           simplify = TRUE)[2], 
#                                        player_stats[(attribute_split+1):(end-1)])), 
#                          FUN = data_parser) %>%
#       unlist() %>% 
#       stringr::str_replace_all(pattern = c("Offensive Ratings|Mental Ratings|Defensive Ratings|Physical Ratings"), 
#                                replacement = " x") %>% 
#       stringr::str_split(pattern = " x") %>% 
#       unlist() %>% 
#       stringr::str_squish() %>% 
#       stringi::stri_remove_empty() %>% 
#       matrix(nrow = 2)
#     
#     colnames(attributes) <- attributes[1,]
#     
#     attributes <- attributes[-1,] %>% 
#       sapply(as.numeric) %>% 
#       t() %>% 
#       as.data.frame()
#   }
#   
#   return(cbind(data_part, attributes))
# }
# 
# # Subfunction that parses data on the format X : Y
# data_parser <- function(x){
#   x <- x %>% 
#     stringr::str_split(pattern = ":") %>% 
#     unlist() %>% 
#     stringr::str_squish()
#   
#   return(x)
# }
# 
# # Scrapes a number of player threads from a team's roster link
# player_link_scraper <- function(roster_link){
#   player_links <- XML::htmlTreeParse(RCurl::getURL(paste(base_link, roster_link, sep = "")), 
#                                      asText = TRUE, 
#                                      useInternalNodes = TRUE) %>% 
#     XML::xpathApply(path = "//td[contains(@class,'forumdisplay_regular')]/div/span/a/@href", 
#                     fun = function(x)as.character(x)) %>% 
#     str_remove_all(pattern = "&action=newpost")
#   
#   return(player_links)
# }
# 
# # Scrapes team roster and returns player data from each roster
# team_roster_scraper <- function(team_link){
#   base_link <- "https://simulationhockey.com/"
#   
#   team_page <- XML::htmlTreeParse(RCurl::getURL(team_link), asText = TRUE, useInternalNodes = TRUE)
#   
#   # ## Moved this to inside the player_link_scraper to prevent duplicates
#   # team_name <- XML::xpathApply(doc = team_page, 
#   #                              path = "//td[@class='thead']/div/span", 
#   #                              fun = xmlValue) %>% 
#   #   unlist()
#   
#   team_roster <- XML::xpathApply(doc = team_page, 
#                                  path = "//strong/a[contains(., 'Roster')]/@href", 
#                                  fun = function(x) as.character(x))
#   
#   team_roster2 <- paste(team_roster, "&page=2", sep = "")
#   
#   team_rosters <- list(team_roster, team_roster2)
#   
#   player_links <- unlist(lapply(team_rosters, FUN = player_link_scraper))
#   
#   cl <- makeCluster(getOption("cl.cores", 4))
#   
#   clusterExport(cl, 
#                 varlist = c("base_link",
#                             "player_data_scraper",
#                             "%>%",
#                             "data_parser",
#                             "user_data_parser",
#                             "player_link_scraper",
#                             "player_stats_parser",
#                             "player_stats_parser_2"))
#   
#   player_data <- clusterApply(cl,
#                               x = player_links, 
#                               fun = player_data_scraper)
#   
#   # ## Changed to parallel computation of these scrapes
#   # player_data <- lapply(player_links, FUN = function(x) cbind(team_name, player_data_scraper(x))) 
#   
#   stopCluster(cl)
#   
#   ncols <- lapply(player_data, FUN = ncol) %>%
#     unlist()
#   
#   goalies <- do.call(rbind, player_data[which(ncols == 38)])
#   players <- do.call(rbind, player_data[which(ncols == 48)])
#   
#   
#   return(list(goalies = goalies, players = players))
# }