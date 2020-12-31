require(ggplot2)
require(tidyr)
require(dplyr)
require(RColorBrewer)
require(plotly)


load(file = "SMJHL 2020-12-15.RData")

att_key <- read.csv2("attribute_key.csv")

smjhl_players <- smjhl_data %>% 
  mutate(name = paste(First.Name, Last.Name),
         tpe = as.numeric(tpe)) %>%
  mutate(cap_tpe = 
           case_when(season %in% c("S58", "S57") & tpe > 350 ~ 350,
           tpe > 425 ~ 425,
           TRUE ~ tpe)) %>% 
  select(name, 
         Position, 
         Screening:X.Professionalism) %>% 
  pivot_longer(Screening:X.Professionalism, 
               names_to = "Attribute", 
               values_to = "Rating") %>% 
  left_join(att_key)

smjhl_players$abbrev <- factor(smjhl_players$abbrev, 
                               levels = c("SCR", "GTO", 'PAS', 'PHA', 'SAC', 'SRA', 'OFR',
                                          'CHE', 'HIT', 'POS', 'SCH', 'SBL', 'FOF', 'DFR',
                                          'ACC', 'AGI', 'SPD', 'STA', 'STR', 'BAL', 'FIG',
                                          'AGR', 'BRA', 'DET', 'TPL', 'LEA', 'TEM', 'PRO'))

chosen_name <- "Pablo Salvatici"

smjhl_players %>% 
  filter(name == chosen_name) %>% 
  plot_ly(type = 'scatterpolar',
          mode = "markers",
          r = ~Rating,
          theta = ~abbrev,
          text = ~Attribute,
          fill = 'toself',
          hoverinfo = "text",
          color = I("darkorange"),
          name = chosen_name) %>% 
  config(modeBarButtonsToRemove = c("pan2d", 
                                    "zoomIn2d", "zoomOut2d",
                                    "autoScale2d", "resetScale2d",
                                    "hoverClosestCartesian",
                                    "hoverCompareCartesian",
                                    "toggleSpikelines")) %>% 
  layout(polar = list(radialaxis = list(visible = TRUE,
                                        range = c(0,20))),
         showlegend = TRUE) 
























