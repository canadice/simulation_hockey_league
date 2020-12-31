require(XML)
require(ggplot2)
require(stringr)
require(dplyr)
require(RColorBrewer)
require(lubridate)
require(plotly)

season <- "56"
league <- "smjhl"

data_loader <- function(season = "56", league = "smjhl", data_type = "ratings"){
  if(!is.character(season)){
    season = as.character(season)
  }
  
  link <- paste("https://simulationhockey.com/", data_type, ".php?league=", league, "&season=", season, "&st=rs", sep = "")

  temp <- XML::htmlTreeParse(RCurl::getURL(link), asText = TRUE, useInternalNodes = TRUE)
  
  # Loads the player data 
  if(data_type == "ratings"){
    type <- "skaterratings"
  } else {
    type <- "skaterdata"
  } 
  
  header <- unlist(XML::xpathApply(doc = temp, 
                                   path = paste("//table[@id=\"", type, "\"]/thead/tr/th", sep = ""), fun = XML::xmlValue))
  table <- unlist(XML::xpathApply(doc = temp, 
                                  path = paste("//table[@id=\"", type, "\"]/tbody/tr/td", sep = ""), fun = XML::xmlValue))
  
  data_table <- as.data.frame(matrix(data = table, ncol = length(header), byrow = TRUE))
  colnames(data_table) <- header
  
  data_table <- data_table %>% mutate(PId = as.numeric(PId))
  
  return(data_table)
}
{
# ## Loads the individual player skills
# link <- "https://simulationhockey.com/ratings.php?league=smjhl&season=56&st=rs"
# 
# temp <- XML::htmlTreeParse(RCurl::getURL(link), asText = TRUE, useInternalNodes = TRUE)
# 
# # Loads the player data caller skaterratings
# header <- unlist(XML::xpathApply(doc = temp, 
#                                  path = "//table[@id=\"skaterratings\"]/thead/tr/th", fun = XML::xmlValue))
# table <- unlist(XML::xpathApply(doc = temp, 
#                                 path = "//table[@id=\"skaterratings\"]/tbody/tr/td", fun = XML::xmlValue))
# 
# player_skills <- as.data.frame(matrix(data = table, ncol = length(header), byrow = TRUE))
# colnames(player_skills) <- header
# 
# player_skills <- player_skills %>% mutate(PId = as.numeric(PId))
# 
# ## Loads the individual player stats
# link <- "https://simulationhockey.com/playerstats.php?league=smjhl&season=56&st=rs"
# 
# temp <- XML::htmlTreeParse(RCurl::getURL(link), asText = TRUE, useInternalNodes = TRUE)
# 
# # Loads the player data caller skaterdata
# header <- unlist(XML::xpathApply(doc = temp, 
#                                  path = "//table[@id=\"skaterdata\"]/thead/tr/th", fun = XML::xmlValue))
# table <- unlist(XML::xpathApply(doc = temp, 
#                                 path = "//table[@id=\"skaterdata\"]/tbody/tr/td", fun = XML::xmlValue))
# 
# player_stats <- as.data.frame(matrix(data = table, ncol = length(header), byrow = TRUE))
# colnames(player_stats) <- header
# 
# player_stats <- player_stats %>% mutate(PId = as.numeric(PId))
# 
# ## Loads the individual player advanced stats
# link <- "https://simulationhockey.com/advstats.php?league=smjhl&season=56&st=rs"
# 
# temp <- XML::htmlTreeParse(RCurl::getURL(link), asText = TRUE, useInternalNodes = TRUE)
# 
# # Loads the player data caller skaterdata
# header <- unlist(XML::xpathApply(doc = temp, 
#                                  path = "//table[@id=\"skaterdata\"]/thead/tr/th", fun = XML::xmlValue))
# table <- unlist(XML::xpathApply(doc = temp, 
#                                 path = "//table[@id=\"skaterdata\"]/tbody/tr/td", fun = XML::xmlValue))
# 
# player_advstats <- as.data.frame(matrix(data = table, ncol = length(header), byrow = TRUE))
# colnames(player_advstats) <- header
# 
# player_advstats <- player_advstats %>% mutate(PId = as.numeric(PId))
}

data_merge <- function(data1, data2, data3){
  player_data <- full_join(data1, data2) %>% full_join(data3) %>% 
    mutate(Team = factor(Team), 
           Name = factor(Name), 
           Pos = factor(Pos, levels = c("LD", "RD", "LW", "RW", "C")))
  
  return(player_data)
}

ratings <- data_loader(data_type = "ratings")
stats <- data_loader(data_type = "playerstats")
adv_stats <- data_loader(data_type = "advstats")

smjhl_player_data <- data_merge(ratings, stats, adv_stats)

# shl56_player_data <- data_merge(data_loader(league = "shl", data_type = "ratings"), 
#                                 data_loader(league = "shl", data_type = "playerstats"),
#                                 data_loader(league = "shl", data_type = "advstats"))
# 
# shl53_player_data <- data_merge(data_loader(season = "53", league = "shl", data_type = "ratings"), 
#                                 data_loader(season = "53", league = "shl", data_type = "playerstats"),
#                                 data_loader(season = "53", league = "shl", data_type = "advstats"))
# 
# shl54_player_data <- data_merge(data_loader(season = "54", league = "shl", data_type = "ratings"), 
#                                 data_loader(season = "54", league = "shl", data_type = "playerstats"),
#                                 data_loader(season = "54", league = "shl", data_type = "advstats"))
# 
# shl55_player_data <- data_merge(data_loader(season = "55", league = "shl", data_type = "ratings"), 
#                                 data_loader(season = "55", league = "shl", data_type = "playerstats"),
#                                 data_loader(season = "55", league = "shl", data_type = "advstats"))
# 
# shl_seasonal_player_data <- list(S53 = shl53_player_data, S54 = shl54_player_data, 
#                                  S55 = shl55_player_data, S56 = shl56_player_data)
# 
# save(shl_seasonal_player_data, file = "shl_player_data.Rdata")
load("shl_player_data.Rdata")

## Some analysis
forwards <- shl_player_data %>% 
  tibble::column_to_rownames(var = "Name") %>% 
  select(PId, AGR:DFR) %>% 
  mutate(across(.fns = as.numeric))

d <- dist(forwards[,-1], method = "manhattan")
fit <- cmdscale(d, eig = TRUE, k = 2)

distances <- data.frame(x = fit$points[,1], 
                        y = fit$points[,2], 
                        id = forwards$PId) %>% 
  left_join(shl_player_data, by = c("id" = "PId"))

ggplot(distances) + aes(x, y, fill = Pos, fg = Team, text = Name) + 
  geom_point(pch = 21, color = "black", size = 3) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_fill_manual(values = brewer.pal(n = 5, name = "Paired")) + 
  labs(x = "", y = "", title = "SHL") +
  scale_x_continuous(labels = NULL) + 
  scale_y_continuous(labels = NULL)

highlight_key(txhousing, ~city, "Select a city")
ggplotly(tooltip = c("fg", "text"))


