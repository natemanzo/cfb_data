library(cfbscrapR)
library(tidyverse)

## Define Week of interest
week_num = 7

## Pull play by play data for team of interest
pbp = cfb_pbp_data(year = 2020, team = "Alabama", epa_wpa = TRUE, week = week_num)

## Create new dataframe for only plays from scrimmage
pbp_clean = pbp %>% 
  filter(pass == 1 | rush == 1)

## Pull color and logo info for all CFB teams
team_info = as_tibble(cfb_team_info(year = 2020))
logos = team_info %>% 
  select(school, color, logos) %>%
  unnest(logos) %>%
  group_by(school) %>%
  slice(1)

pbp_logos = pbp_clean %>%
  select(offense_play) %>%
  distinct() %>%
  left_join(logos, by = c("offense_play" = "school"))

## Create a named vector to use in graphs later
cols = pbp_logos %>% pull(color)
names(cols) = pbp_logos %>% pull(offense_play) 

## Create variables with each team's name
team_1 = pbp_clean %>% select(offense_play) %>% distinct() %>% slice(1) %>% pull()
team_2 = pbp_clean %>% select(offense_play) %>% distinct() %>% slice(2) %>% pull()

## Graph Cumulative EPA
pbp_clean %>%
  group_by(offense_play) %>%
  mutate(play_number = row_number(),
         cu_epa = cummean(EPA)) %>%
  ggplot(aes(x = play_number, y = cu_epa, color = offense_play)) + 
  geom_line(size = 2) + geom_hline(yintercept = 0, linetype = 2) +
  scale_color_manual(values = cols, name = "Team") +
  ylab("EPA") + xlab("Play Number") +
  theme_minimal() + theme(panel.grid.minor = element_blank()) +
  labs(title = paste0(team_1, " vs. ", team_2, " Cumulative EPA"),
       caption = "Chart by @cfbNate |  Data from @CFB_Data via @cfbscrapr")

## Save the plot above
ggsave(filename = paste0("epa_cu_2020_wk",week_num,"_",team_1,team_2,".png"),
       dpi = 300, type = "cairo", width = 11, height = 8, units = "in")

## Graph 10-Play Moving Average EPA
pbp_clean %>%
  group_by(offense_play) %>%
  mutate(play_number = row_number(),
         ma_epa = zoo::rollapply(EPA,10,mean,align='right',fill=NA)) %>%
  ggplot(aes(x = play_number, y = ma_epa, color = offense_play)) + 
  geom_line(size = 2) + geom_hline(yintercept = 0, linetype = 2) +
  scale_color_manual(values = cols, name = "Team") +
  ylab("EPA") + xlab("Play Number") +
  theme_minimal() + theme(panel.grid.minor = element_blank()) +
  labs(title = paste0(team_1, " vs. ", team_2, " 10-Play Moving Average EPA"),
       caption = "Chart by @cfbNate |  Data from @CFB_Data via @cfbscrapr")

## Save the plot above  
ggsave(filename = paste0("epa_ma_2020_wk",week_num,"_",team_1,team_2,".png"),
      dpi = 300, type = "cairo", width = 11, height = 8, units = "in")
