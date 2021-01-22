## Taken from https://docs.google.com/spreadsheets/d/1fQ81SqrV2RbuwwshThOli422nmcEY9RHAV6F6qXFy7g/edit#gid=467923313

# install.packages("googlesheets4")
library(googlesheets4)
require(stringr)
require(plyr)
require(dplyr)
require(tibble)

load(paste("shl_player_database/data/SHL&SMJHL 2021-01-19.RData", sep = ""))

eligible <- 
  read_sheet("https://docs.google.com/spreadsheets/d/1fQ81SqrV2RbuwwshThOli422nmcEY9RHAV6F6qXFy7g/edit#gid=467923313",
             range = "Sweden!A:G") %>% 
  add_row(
    Name = c("Hugo Grönroos", "Luc Blouin", "Kit Kirkstone", "Aleks Blixtstrom"),
    Position = c("G", "D", "G", "F")
  ) %>% 
  filter(
    !str_detect(Name, "St. Ark")
  )

role_key <- read_sheet("https://docs.google.com/spreadsheets/d/16U6JFYxrN5nzsqDxgVgqMkLbYWIOMgbMTlucAYyTqlc/edit#gid=1153624088",
                       sheet = "Role Weight Matrix") %>% 
  column_to_rownames(var = "Role") %>% 
  apply(MARGIN = 1, FUN = function(x)x/sum(x))

wjc_skaters <- 
  eligible %>% 
  filter(Position != "G") %>% 
  fuzzyjoin::stringdist_left_join(
    smjhl_data$players,
    by = c("Name" = "Name"),
    max_dist = 2
  ) %>% 
  select(where(~ sum(!is.na(.x))>0)) %>% 
  cbind(
    as.matrix(.[, 35:64]) %*% (as.matrix(role_key)) %>% 
      as.data.frame() %>% 
      apply(MARGIN = 1, FUN = function(x){
        index <- head(order(x, decreasing =TRUE), n = 5)
        
        paste(colnames(.)[index], round(x[index], 3))
      }) %>% 
      t()
  ) %>% 
  dplyr::relocate(`1`:`5`, .after = TPE) %>% 
  mutate(
    TPE = 
      case_when(
        is.na(TPE) ~ tpe,
        TRUE ~ TPE
      )
  ) %>% 
  arrange(Position.x, TPE)

# write.csv2(wjc_skaters, file = "F:/Google Drive/Simulation Hockey League/WJC/skaterS57.csv", row.names = FALSE)

wjc_goalies <- 
  eligible %>% 
  filter(Position == "G") %>% 
  fuzzyjoin::stringdist_left_join(
    smjhl_data$goalies,
    by = c("Name" = "Name"),
    max_dist = 2
  ) %>% 
  select(where(~ sum(!is.na(.x))>0)) %>% 
  arrange(Position.x, tpe)

# write.csv2(wjc_goalies, file = "F:/Google Drive/Simulation Hockey League/WJC/goalieS57.csv", row.names = FALSE)

googlesheets4::write_sheet(wjc_skaters, 
                           ss = "https://docs.google.com/spreadsheets/d/16U6JFYxrN5nzsqDxgVgqMkLbYWIOMgbMTlucAYyTqlc/edit#gid=1153624088",
                           sheet = "SkatersS57")

googlesheets4::write_sheet(wjc_goalies, 
                           ss = "https://docs.google.com/spreadsheets/d/16U6JFYxrN5nzsqDxgVgqMkLbYWIOMgbMTlucAYyTqlc/edit#gid=1153624088",
                           sheet = "GoaliesS57")
