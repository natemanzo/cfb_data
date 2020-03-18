library(tidyverse)
library(cfbscrapR)

pbp_2019 <- data.frame()

for(i in 1:15){
  model <- cfb_pbp_data(year = 2019, week = i, epa_wpa = TRUE, season_type = "regular")
  df <- data.frame(model)
  df = df %>% mutate(week = i)
  pbp_2019 <- bind_rows(pbp_2019, df)
}

bowl_df <- cfb_pbp_data(year = 2019, epa_wpa = TRUE, season_type = "postseason")
bowl_df = bowl_df %>% mutate(week = 16)

pbp_2019 <- bind_rows(pbp_2019, bowl_df)

pbp_2019 <- pbp_2019 %>%
  rename(adjusted_yardline = adj_yd_line,
         offense = offense_play,
         defense = defense_play)

# RB names 
pbp_2019 <- pbp_2019 %>%
  mutate(rush_player = ifelse(rush == 1, str_extract(play_text, "(.{0,25} )run |(.{0,25} )\\d{0,2} Yd Run"), NA)) %>%
  mutate(rush_player = str_remove(rush_player, " run | \\d+ Yd Run"))

# QB names
pbp_2019 <- pbp_2019 %>%
  mutate(pass_player = ifelse(pass==1,
                              ifelse(str_detect(play_text, "from"),
                                     str_extract(play_text, "(?<=from )(.{0,30})(?= \\()"),
                                     str_extract(play_text, "(.{0,30})(?= pass| sacked)")), NA))

# WR names
pbp_2019 <- pbp_2019 %>%
  mutate(receiver_player = ifelse(pass==1,
                                  ifelse(str_detect(play_text, "from"),
                                         str_extract(play_text, "(.+)(.+)(?= \\d+)"),
                                         str_extract(play_text, "(?<= to )([a-zA-Z].{0,24})(?= for \\d| for no| for a loss)")), NA)) %>%
  mutate(receiver_player = ifelse(play_type == "Sack", NA, receiver_player)) %>%
  mutate(receiver_player = ifelse(play_type == "Pass Interception Return", NA, receiver_player))  

## Position-Specific dfs --------------------------
## QBs
passers_df = pbp_2019 %>%
  filter(pass == 1) %>%
  group_by(pass_player, offense) %>%
  summarize(attempts = n(), mean_epa = mean(EPA), sum_epa = sum(EPA)) %>%
  arrange(desc(mean_epa)) %>%
  rename(team = offense) %>%
  left_join(team_id, by = c("team" = "Team")) %>%
  select(pass_player, team, attempts, mean_epa, sum_epa)

## WRs
receivers_df = pbp_2019 %>%
  filter(pass == 1, !is.na(receiver_player)) %>%
  group_by(receiver_player, offense) %>%
  summarize(catches = n(), mean_epa = mean(EPA), sum_epa = sum(EPA)) %>%
  arrange(desc(mean_epa)) %>%
  rename(team = offense) %>%
  left_join(team_id, by = c("team" = "Team")) %>%
  select(receiver_player, team, catches, mean_epa, sum_epa)

## Define Rusher Position
qbs = passers_df %>%
  filter(attempts > 10) %>%
  select(pass_player) %>%
  distinct()

qbs = as.vector(qbs$pass_player)

pbp_2019 = pbp_2019 %>%
  mutate(rush_pos = case_when(
    is.na(rush_player) ~ NA_character_,
    rush_player %in% qbs ~ "QB",
    !rush_player %in% qbs ~ "RB"))

## Create Rushers df
rushers_df = pbp_2019 %>%
  filter(rush == 1) %>%
  group_by(rush_player, offense, rush_pos) %>%
  summarize(carries = n(), mean_epa = mean(EPA), sum_epa = sum(EPA)) %>%
  arrange(desc(mean_epa)) %>%
  rename(team = offense) %>%
  left_join(team_id, by = c("team" = "Team")) %>%
  select(rush_player, team, rush_pos, carries, mean_epa, sum_epa)