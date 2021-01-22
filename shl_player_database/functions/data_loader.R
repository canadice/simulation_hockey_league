load("data/shl_player_data.Rdata")
load(paste("data/SHL&SMJHL 2021-01-19.RData", sep = ""))

att_key <- read.csv2("data/attribute_key.csv")
division_key <- read.csv2("data/team_division_conference.csv")

rmdfiles <- c("data/explanation_MDS.Rmd", "data/welcome.Rmd")

### Creates a character vector of all the player statistics for use in drop down menus.
playerStatistics <- 
  c(
    "Goals" = "G",
    "Assists" = "A",
    "Points" = "P",
    "Plus-Minus" = "+/-",
    "Penalty Minutes" = "PIM",
    "Power Play Goals" = "PPG",
    "Power Play Assists" = "PPA",
    "Power Play Points" = "PPP",
    "Short Handed Goals" = "SHG",
    "Short Handed Points" = "SHP",
    "Shots On Goal" = "SOG",
    "Shooting %" = "SH%",
    "Fights Taken" = "FT",
    "Fights Won" = "FW",
    "Hits" = "HITS",
    "Giveaways" = "GVA",
    "Takeaways" = "TKA",
    "Shots Blocked" = "SB",
    "Average Offensive Game Rating" = "OGR",
    "Average Defensive Game Rating" = "DGR",
    "Average Time On Ice" = "TOI",
    "Average Time On Power Play" = "PPTOI",
    "Average Time On Penalty Kill" = "SHTOI",
    "Shooting Plus Save Percentage" = "PDO",  
    "Goals For/60 Minutes" = "GF/60",
    "Goals Against/60 Minutes" = "GA/60",
    "Shots For/60 Minutes" = "SF/60",
    "Shots Against/60 Minutes" = "SA/60",
    "Corsi For" = "CF",
    "Corsi Against" = "CA",
    "Corsi For %" = "CF%",  
    "Relative Corsi For %" = "CF% REL",
    "Fenwick For" = "FF",
    "Fenwick Against" = "FA",
    "Fenwick For %" = "FF%",
    "Relative Fenwick For %" = "FF% REL"
  )

sapply(rmdfiles, knit, quiet = T)
