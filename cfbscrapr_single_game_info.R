#devtools::install_github(repo = "saiemgilani/cfbscrapR")
library(cfbscrapR)
library(tidyverse)
#library(DataEditR)

pbp_2019 = cfb_pbp_data(year = 2020, team = "Oklahoma", epa_wpa = TRUE, week = 4)

pbp_2019 %>% filter(drive_number == 17) %>% 
  select(period, yards_to_goal, drive_number, drive_play_number, drive_result, down, distance, yards_to_goal, yards_gained, play_type, play_text, pass, rush, EPA)

pbp_2019_clean = pbp_2019 %>% 
  mutate(penalty_type = ifelse(play_type == "Penalty",
                               ifelse(str_detect(play_text, "false start"), "False Start",
                                      ifelse(str_detect(play_text, "illegal formation"), "Illegal Formation",
                                             ifelse(str_detect(play_text, "Defensive offside"), "Offsides",
                                                    ifelse(str_detect(play_text, "holding"), "Holding",NA))))
                               ,NA)) %>%
  mutate(penalty_include = ifelse(penalty_type == "Holding", 1,0)) %>%
  filter(pass == 1 | rush == 1 | penalty_include == 1)

ou_off = pbp_2019_clean %>% filter(offense_play == "Oklahoma") %>% 
  mutate(clock_minutes = ifelse(period == 1 | period == 3, clock.minutes - 15, clock.minutes)) %>%
  select(period, clock_minutes, clock.seconds, yards_to_goal, drive_number, drive_result, down, distance, yards_gained, EPA, play_type, play_text, penalty_type)

#write_csv(ou_off, "~/Personal/cfbNate/2020_OU_Stats/wk04_kansas_state_off.csv")

ou_def = pbp_2019_clean %>% filter(defense_play == "Oklahoma") %>% 
  mutate(clock_minutes = ifelse(period == 1 | period == 3, clock.minutes - 15, clock.minutes)) %>%
  select(period, clock_minutes, clock.seconds, yards_to_goal, drive_number, drive_result, down, distance, yards_gained, EPA, play_type, play_text, penalty_type)

#write_csv(ou_def, "~/Personal/cfbNate/2020_OU_Stats/wk04_kansas_state_def.csv")

#data_edit(temp)

sort(colnames(pbp_2019_clean))


# RB names 
pbp_2019_clean <- pbp_2019_clean %>%
  mutate(rush_player = ifelse(rush == 1, str_extract(play_text, "(.{0,25} )run |(.{0,25} )\\d{0,2} Yd Run"), NA)) %>%
  mutate(rush_player = str_remove(rush_player, " run | \\d+ Yd Run"))

# QB names
pbp_2019_clean <- pbp_2019_clean %>%
  mutate(pass_player = ifelse(pass==1,
                              ifelse(str_detect(play_text, "from"),
                                     str_extract(play_text, "(?<=from )(.{0,30})(?= \\()"),
                                     str_extract(play_text, "(.{0,30})(?= pass| sacked)")), NA))

# WR names
pbp_2019_clean <- pbp_2019_clean %>%
  mutate(receiver_player = ifelse(pass==1,
                                  ifelse(str_detect(play_text, "from"),
                                         str_extract(play_text, "(.+)(.+)(?= \\d+)"),
                                         str_extract(play_text, "(?<= to )([a-zA-Z].{0,24})(?= for \\d| for no| for a loss)")), NA)) %>%
  mutate(receiver_player = ifelse(play_type == "Sack", NA, receiver_player)) %>%
  mutate(receiver_player = ifelse(play_type == "Pass Interception Return", NA, receiver_player))  

## Position-Specific dfs --------------------------
## QBs
passers_df = pbp_2019_clean %>%
  filter(pass == 1, play_type != "Sack") %>%
  group_by(pass_player, offense_play) %>%
  summarize(dropbacks = n(), mean_epa = mean(EPA), sum_epa = sum(EPA), passing_yards = sum(yards_gained), ypa = mean(yards_gained)) %>%
  arrange(desc(mean_epa)) %>%
  rename(team = offense_play)

passers_basic = pbp_2019_clean %>%
  filter(pass == 1) %>%
  group_by(pass_player, offense_play, play_type) %>%
  summarize(n = n()) %>% 
  mutate(play_type = recode(play_type, 
                            "Pass Incompletion" = "Incomplete",
                            "Pass Reception" = "Complete",
                            "Passing Touchdown" = "TD",
                            "Interception Return" = "INT")) %>%
  pivot_wider(names_from = play_type, values_from = n, values_fill = list(n = 0)) %>%
  mutate(attempts = Incomplete + Complete + TD + INT,
         completions = Complete + TD,
         pct = completions/attempts)

passers_basic %>% left_join(passers_df, by = c("pass_player","offense_play" = "team")) %>%
  select(pass_player, team = offense_play, completions, attempts, pct,  ypa, passing_yards, mean_epa, sum_epa) %>%
  arrange(desc(passing_yards))


## Need to add completion percentage
## Need to reinstall cfbscrapR to see about the negative EPA play for Chandler Morris  
  
## WRs
receivers_df = pbp_2019_clean %>%
  filter(pass == 1, !is.na(receiver_player)) %>%
  group_by(receiver_player, offense_play) %>%
  summarize(catches = n(), mean_epa = mean(EPA), sum_epa = sum(EPA), yards = sum(yards_gained)) %>%
  arrange(desc(mean_epa)) %>%
  rename(team = offense_play)

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

