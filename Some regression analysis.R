require(dplyr)
require(lme4)
require(stringr)
require(lubridate)

load("shl_player_data.Rdata")

shl_seasonal_player_data <- lapply(X = shl_seasonal_player_data, FUN = function(x) {
  x <- x %>% 
    mutate(across(!(contains("TOI")|contains("%")) & where(is.character), as.numeric),
           across(contains("TOI"), function(x) as.numeric(ms(x, roll = TRUE, quiet = TRUE))),
           `SH%` = as.numeric(str_remove(`SH%`, "%")),
           across(contains("%"), as.numeric))
  
  y <- x %>% 
    group_by(Team) %>% 
    summarize(across(AGR:DFR, .fns = mean))
  
  z <- x %>% left_join(y, by = "Team", suffix = c(".ind", ".team"))
})

names(shl_seasonal_player_data$S53)

ratings_formula <- paste("P ~ ",
                         paste0("AGR", "BRA", 
                                "ACC", "AGI", "BAL", "SPD", "STA", "STR", 
                                "FIG", "SCR", "GTO", "PAS", "PHA", "SAC",
                                "SRA", "OFR", "CHE", "FOF", "HIT", "POS",
                                "SBL", "SCH", "DFR", collapse = "*"
                                ))


model <- lm(formula = ratings_formula, data = shl_seasonal_player_data$S53)

summary(model)







