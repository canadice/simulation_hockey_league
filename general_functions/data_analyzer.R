
###########################################################################
###########################################################################
###                                                                     ###
###                      FHM Test analyzer (CASINO)                     ###
###                                                                     ###
###                         CREATED: 2021-02-09                         ###
###                        LAST EDIT: 2021-02-09                        ###
###                                                                     ###
###########################################################################
###########################################################################

source("general_functions/fhm6_parser.R")

teamCasino <- 
  googlesheets4::read_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1kisvxMASJvX26djRXzDVDxMz1ODqbaaMfuwUXtVwpWw/edit#gid=1074258598",
    sheet = "Teams",
    range = "A:D"
  )

nSims <- 14

tests <- 
  lapply(
    X = paste(
      "SHL-S58-PreDay2-Test",
      #"WJC_S57-Day3-RR2-Test", 
      1:nSims %>% as.character(),
      ".lg",
      sep = ""),
    FUN = fhm6Parser
  ) 

simTeams <- 
  tests %>% 
  ### Filters out a specific team (7 is Sweden in WJC)
  lapply(
    FUN = function(x){
      x$teams #%>%
      # filter(
      #   TeamId == 7
      # )
    }
  ) %>%
  do.call(
    rbind,
    args = .
  ) %>% 
  select(
    TeamId,
    Conference.Id,
    Division.Id,
    Name,
    Nickname,
    Abbr,
    Wins,
    Losses,
    Ties,
    OTL,
    contains("Shootout"),
    Points, 
    contains("Goals"),
    sim
  )

googlesheets4::write_sheet(
  simTeams,
  ss = "https://docs.google.com/spreadsheets/d/1kisvxMASJvX26djRXzDVDxMz1ODqbaaMfuwUXtVwpWw/edit#gid=1074258598",
  sheet = "Canadice's Test")

casinoPredictions<-
  teamCasino %>% 
  full_join(
    simTeams,
    by = c("TeamId")
  ) %>% 
  group_by(
    TeamId,
    Team
  ) %>% 
  summarize(
    casino = mean(Casino),
    nTests = n(),
    over = sum(Wins > Casino),
    under = sum(Wins < Casino),
    meanW = mean(Wins) %>% round(2),
    sdW = sd(Wins) %>% round(2),
    medianW = median(Wins),
    lowQuartW = quantile(Wins, probs = 0.25),
    uppQuartW = quantile(Wins, probs = 0.75)
  )


googlesheets4::write_sheet(
  casinoPredictions,
  ss = "https://docs.google.com/spreadsheets/d/1kisvxMASJvX26djRXzDVDxMz1ODqbaaMfuwUXtVwpWw/edit#gid=1074258598",
  sheet = "Canadice's Aggregates")

simPlayers <- 
  tests %>% 
  lapply(
    FUN = function(x){
      x$players #%>% 
      # filter(
      #   TeamId == 7
      # )
    }
  ) %>% 
  do.call(
    rbind,
    args = .
  ) %>% 
  group_by(
    # sim,
    Last.Name,
    Type
  ) %>% 
  summarize(
    across(where(is.numeric), .fns = function(x) mean(x) %>% round(3))
  ) %>% 
  arrange(
    Last.Name,
    Type
  ) %>% 
  mutate(
    fantasy = 4*G + 3*A + 0.3*SOG + 0.4*HIT + 0.9*SB
  ) %>% 
  relocate(
    fantasy,
    .before = GP
  )




