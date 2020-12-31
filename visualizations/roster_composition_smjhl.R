require(ggplot2)
require(dplyr)
require(RColorBrewer)

load(file = "data/SMJHL 2020-12-15.RData")

test <- smjhl_player_data %>% 
  group_by(team, season) %>% 
  mutate(tpe = as.numeric(tpe)) %>% 
  mutate(cap_tpe = 
           case_when(season %in% c("S58", "S57") & tpe > 350 ~ 350,
                     tpe > 425 ~ 425,
                     TRUE ~ tpe)) %>% 
  summarize(n = n(), tpe_avg = mean(cap_tpe))

ggplot(test) + 
  aes(x = season, 
      y = tpe_avg, 
      group = team,
      fill = as.factor(n)) + 
  geom_bar(stat = "identity",
           color = "black") +
  facet_wrap(facets = vars(team)) + 
  scale_fill_manual("Number of\nPlayers", values = rev(brewer.pal(11, "RdYlBu"))) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray80"),
        panel.grid.minor.y = element_line(color = "gray75"),
        axis.title.y = element_text(angle = 0,
                                    vjust = 0.5),
        axis.text = element_text(size = 11)) +
  scale_y_continuous(expand = c(0,0), 
                     limits = c(0, 500)) +
  geom_segment(aes(x = 3.5, xend = 5.5, y = 350, yend = 350), 
               color = "black",
               size = 1) + 
  geom_segment(aes(x = 0.5, xend = 3.5, y = 425, yend = 425), 
               color = "black",
               size = 1) +
  labs(y = "Average\nTPE\n(capped)", 
       x = "Draft Class",
       caption = "Data scraped from SHL Forum, 2020-12-11")
