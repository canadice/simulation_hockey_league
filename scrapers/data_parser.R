## Function for parsing data of format variable: value
data_parser <- function(x){
  x <- x %>% 
    stringr::str_split(pattern = ":") %>% 
    unlist() %>% 
    stringr::str_squish()
  
  return(x)
}