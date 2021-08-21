#devtools::install_github(repo = "saiemgilani/cfbfastR")
library(cfbfastR)
library(dplyr)
library(janitor)
library(stringr)
library(readr)

## Define the team you want to review
team_var <- "Oklahoma"

## Pull the games this team has played to determine the week numbers
games <- cfbfastR::cfbd_game_info(year = 2020, team = team_var) %>%
  select(game_id, season, week, season_type, start_date, home_team, away_team, home_points, away_points) %>%
  as_tibble()

## Print the games table
games

## Define the week number you want to review
week_num <- 9

## Pull the play by play data for the team and week you already defined
pbp <- cfbfastR::cfbd_pbp_data(year = 2020, team = team_var, epa_wpa = TRUE, week = week_num) %>%
  as_tibble()

## Select only the relevant Offensive plays for padding
pbp_off <- pbp %>% 
  filter(offense_play == team_var) %>% 
  filter(rush == 1 | pass == 1 | play_type == "Penalty") %>%
  mutate(drive_num = round_half_up(drive_number/2,0)) %>%
  select(id_play, period, clock.minutes, clock.seconds, yards_to_goal, drive_num, drive_result, 
         down, distance, yards_gained, EPA, play_type, 
         rusher_player_name, passer_player_name, receiver_player_name,play_text)

## Select only the relevant Defensive plays for padding
pbp_def <- pbp %>% 
  filter(defense_play == team_var) %>% 
  filter(rush == 1 | pass == 1 | play_type == "Penalty") %>%
  mutate(drive_num = round_half_up(drive_number/2,0)) %>%
  select(id_play, period, clock.minutes, clock.seconds, yards_to_goal, drive_num, drive_result, 
         down, distance, yards_gained, EPA, play_type, 
         rusher_player_name, passer_player_name, receiver_player_name,play_text)

## Select only the relevant Offensive plays for padding based on Chris Osgood's padding template (@osgoodck)
pbp_osgood <- pbp %>% 
  filter(offense_play == team_var) %>% 
  filter(rush == 1 | pass == 1 | play_type == "Penalty") %>%
  mutate(week = week_num,
         opponent = defense_play,
         passing_down = case_when(down == 2 & distance >= 8 ~ 1,
                                  down == 3 & distance >= 5 ~ 1,
                                  down == 4 & distance >= 5 ~ 1,
                                  TRUE ~ 0)) %>%
  mutate(drive_num = round_half_up(drive_number/2,0)) %>%
  select(week, opponent, period, drive_number, down, distance, yards_to_goal, passing_down, 
         rusher_player_name, passer_player_name, receiver_player_name, play_text,
         yards_gained, first_by_yards, first_by_penalty, turnover, touchdown, 
         success, epa_success, rush, pass, ep_before, ep_after, EPA)  

## Define opponent for file naming
opp_team <- pbp %>% 
  filter(offense_play != team_var) %>% 
  select(offense_play) %>% 
  distinct() %>% 
  pull() %>%
  make_clean_names() 

## Define 2 digit week for file naming
week_num_string <- str_pad(week_num, width = 2, side = "left", pad = 0)

## Write CSV outputs
write_csv(pbp_off, paste0("wk_",week_num_string,"_",opp_team,"_off.csv"))
write_csv(pbp_def, paste0("wk_",week_num_string,"_",opp_team,"_def.csv"))
write_csv(pbp_osgood, paste0("wk_",week_num_string,"_",opp_team,".csv"))

## Check the folder path where your CSVs got saved
getwd()
