### Parses player information from a player's post
player_data_parser_long <- function(post){
  start <- which(post == "Player Information")
  
  birthplace <- min(which(stringr::str_detect(post, 
                                              pattern = "Birthplace")))
  ## No longer used as attributes are taken from the index
  # end <- which(post == "*Indicates attributes that cannot be edited.")
  
  player_data <- post[start:birthplace] %>% 
    stringr::str_detect(pattern = ":")
  
  post <- post[start:birthplace]
  
  birthplace <- min(which(stringr::str_detect(post, 
                                              pattern = "Birthplace")))
  
  player_information <- sapply(X = post[2:birthplace], 
                      FUN = data_parser) %>% 
    unlist() %>% 
    matrix(nrow = 2)
  
  colnames(player_information) <- player_information[1,]
  
  player_information <- as.data.frame(t(player_information[-1,]))
  
  return(player_information)
}

player_data_parser_short <- function(post){
  ## No longer used as attributes are taken from the index
  # attribute_split <- which(stringr::str_detect(post, 
  #                                              pattern = c("Player Attributes[ P]|\\)Player Attributes")))
  
  birthplace <- min(which(stringr::str_detect(post, 
                                              pattern = "Birthplace")))
  
  ## If all the data is found in the same element this splits it into the headers
  start <- 5
  info <- stringr::str_detect(post, pattern = "Player Information") %>% which()
  if(length(info) == 0){
    info <- 99
  }
  
  if(info == birthplace){
    starts <- stringr::str_locate_all(post[birthplace], pattern = "First Name|Last Name|Position|Birth Date|Born|Handedness|Shoots|Recruited By|Player Render|Jersey Number|Height|Weight|Birthplace")[[1]][,1]
    post <- post[birthplace] %>% substring(starts, c(starts[-1]-1, nchar(post[birthplace])))
    
    start <- 1
  }
  
  birthplace <- min(which(stringr::str_detect(post, 
                                              pattern = "Birthplace")))
  
  post[birthplace] <- stringr::str_split(post[birthplace], 
                                         pattern = c("Player Attributes[ P]|\\)Player Attributes|Points|Player|Free offseason|7/25|S53"))[[1]][1]
  
  ## No longer used as attributes are taken from the index
  # end <- min(which(stringr::str_detect(post, 
  #                                      pattern = "\\*Indicates attributes that cannot be edited")))
  # 
  # if(attribute_split != birthplace){
  player_information <- sapply(X = as.list(c(post[start:birthplace])),
                      FUN = data_parser)
  # } else {
  #   player_information <- sapply(X = as.list(c(post[5:birthplace])), 
  #                       FUN = data_parser)   
  # }
  
  ## If a link is used for a player render, then the link gives 3 elements instead of 2
  if(any(lapply(player_information, 
                function(x) length(x) >2) %>% 
         unlist())){
    player_information[
      lapply(player_information, 
             function(x) length(x) >2) %>% 
        unlist()
    ][[1]] <- 
      c(player_information[
        lapply(player_information, 
               function(x) length(x) >2) %>% 
          unlist()
      ][[1]][1],"link") 
    
    player_information <- 
      player_information %>% 
      unlist() %>% 
      matrix(nrow = 2)
  }
  
  
  colnames(player_information) <- player_information[1,]
  
  player_information <- as.data.frame(t(player_information[-1,]))
  
  return(player_information)
    
}
  


