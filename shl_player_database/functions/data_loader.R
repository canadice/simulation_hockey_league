load("data/shl_player_data.Rdata")
load("data/SHL&SMJHL 2021-01-03.RData")

att_key <- read.csv2("data/attribute_key.csv")

rmdfiles <- c("data/explanation_MDS.Rmd", "data/welcome.Rmd")

sapply(rmdfiles, knit, quiet = T)