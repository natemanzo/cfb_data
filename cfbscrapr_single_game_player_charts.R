#devtools::install_github(repo = "saiemgilani/cfbscrapR")
library(cfbscrapR)
library(tidyverse)
library(gt)
#library(DataEditR)

as_tibble(cfb_game_info(year = 2020, team = "Oklahoma")) %>% select(game_id, season, week, season_type, start_date, home_team, away_team)

team = "Oklahoma"
week_num = 16
pbp = cfb_pbp_data(year = 2020, team = team, epa_wpa = TRUE, week = week_num)

pbp_clean = pbp %>% 
  mutate(penalty_type = ifelse(play_type == "Penalty",
                               ifelse(str_detect(play_text, "false start"), "False Start",
                                      ifelse(str_detect(play_text, "illegal formation"), "Illegal Formation",
                                             ifelse(str_detect(play_text, "Defensive offside"), "Offsides",
                                                    ifelse(str_detect(play_text, "holding"), "Holding",NA))))
                               ,NA)) %>%
  mutate(play_yards = ifelse(str_detect(play_text, "Intentional Grounding"), yards_gained,
           ifelse(str_detect(play_text, "Penalty"), str_extract(play_text,"(?<=for )\\d+"), yards_gained))) %>%
  mutate(play_yards = as.numeric(play_yards)) %>%
  mutate(penalty_include = ifelse(penalty_type == "Holding", 1,0)) %>%
  filter(pass == 1 | rush == 1 | play_type == "Penalty")

#data_edit(temp)

## Isolate the logos from team_info
team_info = as_tibble(cfb_team_info(year = 2020))
logos = team_info %>% 
  select(school, color, logos, alt_color) %>%
  unnest(logos) %>%
  group_by(school) %>%
  slice(1)

pbp_logos = pbp_clean %>%
  select(offense_play) %>%
  distinct() %>%
  left_join(logos, by = c("offense_play" = "school"))

color_primary = pbp_logos %>% mutate(color = if_else(is.na(color), "#808080", color)) %>% pull(color)
names(color_primary) = pbp_logos %>% pull(offense_play) 

color_secondary = pbp_logos %>% pull(alt_color)
names(color_secondary) = pbp_logos %>% pull(offense_play) 

color_primary = c("Oklahoma" = "#7b0000", "Iowa State" = "yellow2")

scales::show_col(c("yellow","yellow2","yellow3","yellow4"))

# RB names 
pbp_clean <- pbp_clean %>%
  mutate(rusher_player_name = ifelse(rush == 1, str_extract(play_text, "(.{0,25} )run |(.{0,25} )\\d{0,2} Yd Run"), NA)) %>%
  mutate(rusher_player_name = str_remove(rusher_player_name, " run | \\d+ Yd Run"))

# QB names
pbp_clean <- pbp_clean %>%
  mutate(passer_player_name = ifelse(pass==1,
                              ifelse(str_detect(play_text, "from"),
                                     str_extract(play_text, "(?<=from )(.{0,30})(?= \\()"),
                                     str_extract(play_text, "(.{0,30})(?= pass| sacked)")), NA))

# WR names
pbp_clean <- pbp_clean %>%
  mutate(receiver_player_name = ifelse(pass==1,
                                  ifelse(str_detect(play_text, "from"),
                                         str_extract(play_text, "(.+)(.+)(?= \\d+)"),
                                         #str_extract(play_text, "(?<= to )([a-zA-Z].{0,24})(?= for \\d| for no| for a loss)")), NA)) %>%  ##receptions only
                                         str_extract(play_text, "(?<= to )([a-zA-Z].{0,24})(?= for \\d| for no| for a loss|,|$)")), NA)) %>% ##targets + receptions
  mutate(receiver_player_name = ifelse(play_type == "Sack", NA, receiver_player_name)) %>%
  mutate(receiver_player_name = ifelse(play_type == "Pass Interception Return", NA, receiver_player_name)) %>%
  mutate(receiver_player_name = ifelse(play_type == "Interception Return", NA, receiver_player_name)) %>%
  mutate(receiver_player_name = ifelse(str_detect(receiver_player_name, "\\d"), NA, receiver_player_name))

# pass_complete column
pbp_clean <- pbp_clean %>%
  mutate(pass_complete = case_when(rush == 1 ~ NA_character_,
                                   play_type == "Penalty" ~ NA_character_,
                                   play_type == "Sack" ~ NA_character_,
                                   play_type == "Sack Touchdown" ~ NA_character_,
                                   play_type == "Safety" ~ NA_character_,
                                   is.na(receiver_player_name) ~ "Incomplete",
                                   play_type == "Pass Incompletion" ~ "Incomplete",
                                   play_type == "Fumble Recovery (Opponent)" ~ "Complete",
                                   play_type == "Fumble Recovery (Own)" ~ "Complete",
                                   play_type == "Interception Return Touchdown" ~ "Incomplete",
                                   play_type == "Pass Interception Return" ~ "Incomplete",
                                   play_type == "Pass Reception" ~ "Complete",
                                   play_type == "Passing Touchdown" ~ "Complete",
                                   play_type == "Interception Return" ~ "Incomplete",
                                   play_type == "Interception Return Touchdown" ~ "Incomplete"))

ou_off = pbp_clean %>% filter(offense_play == "Oklahoma") %>% 
  mutate(clock_minutes = ifelse(period == 1 | period == 3, clock.minutes - 15, clock.minutes)) %>%
  select(id_play, period, clock_minutes, clock.seconds, yards_to_goal, drive_number, drive_result, down, distance, yards_gained, EPA, play_type, 
         rusher_player_name, passer_player_name, receiver_player_name,play_text)

ou_def = pbp_clean %>% filter(defense_play == "Oklahoma") %>% 
  mutate(clock_minutes = ifelse(period == 1 | period == 3, clock.minutes - 15, clock.minutes)) %>%
  select(id_play, period, clock_minutes, clock.seconds, yards_to_goal, drive_number, drive_result, down, distance, yards_gained, EPA, play_type, 
         rusher_player_name, passer_player_name, receiver_player_name,play_text)

# opp_team used to automate file naming
opp_team = pbp_clean %>% filter(offense_play != team) %>% select(offense_play) %>% distinct() %>% mutate(offense_play = tolower(offense_play)) %>% pull()

#write_csv(ou_off, paste0("C:/Users/NateAndWhitney/Documents/Nate/wk0",week_num,"_",opp_team,"_off.csv"))
#write_csv(ou_def, paste0("C:/Users/NateAndWhitney/Documents/Nate/wk0",week_num,"_",opp_team,"_def.csv"))



## Position-Specific dfs --------------------------
## Create Receivers df
receivers_df = pbp_clean %>%
  filter(pass == 1, !is.na(receiver_player_name)) %>%
  #filter(yards_gained > -20) %>%
  group_by(receiver_player_name, offense_play) %>%
  summarize(catches = n(), mean_epa = mean(EPA), sum_epa = sum(EPA), yards = sum(yards_gained)) %>%
  arrange(desc(mean_epa)) %>%
  rename(team = offense_play)


## Create Passers df
passers_df = pbp_clean %>%
  filter(pass == 1, !play_type %in% c("Sack", "Fumble Recovery (Opponent)")) %>%
  #filter(yards_gained > -20) %>%
  group_by(passer_player_name, offense_play) %>%
  summarize(dropbacks = n(), mean_epa = mean(EPA), sum_epa = sum(EPA), passing_yards = sum(play_yards), ypa = mean(play_yards)) %>%
  arrange(desc(mean_epa)) %>%
  rename(team = offense_play)

## Define Rusher Position
qbs = passers_df %>%
  filter(dropbacks > 1) %>%
  select(passer_player_name) %>%
  distinct()

qbs = as.vector(qbs$passer_player_name)

pbp_clean = pbp_clean %>%
  mutate(rush_pos = case_when(
    is.na(rusher_player_name) ~ NA_character_,
    rusher_player_name %in% qbs ~ "QB",
    !rusher_player_name %in% qbs ~ "RB"))

## Create Rushers df
rushers_df = pbp_clean %>%
  filter(rush == 1) %>%
  #filter(yards_gained > -20) %>%
  group_by(rusher_player_name, offense_play, rush_pos) %>%
  summarize(carries = n(), 
            mean_epa = mean(EPA), 
            sum_epa = sum(EPA), 
            rush_yards = sum(play_yards), 
            ypc = mean(play_yards), 
            rush_td = sum(touchdown),
            rush_fd = sum(first_by_yards)) %>%
  arrange(desc(sum_epa)) %>%
  rename(team = offense_play)


## additional QB dfs
passers_basic = pbp_clean %>%
  filter(pass == 1) %>%
  #filter(yards_gained > -20) %>%
  group_by(passer_player_name, offense_play, play_type) %>%
  summarize(n = n()) %>% 
  mutate(play_type = recode(play_type, 
                            "Pass Incompletion" = "Incomplete",
                            "Pass Reception" = "Complete",
                            "Passing Touchdown" = "TD",
                            "Interception Return" = "INT",
                            "Interception Return Touchdown" = "INT",
                            "Fumble Recovery (Opponent)" = "FumbleLost")) %>%
  pivot_wider(names_from = play_type, values_from = n, values_fill = list(n = 0)) %>%
  # mutate(INT = 0) %>%
  mutate(attempts = Incomplete + Complete + TD + INT,
         completions = Complete + TD,
         pct = completions/attempts)

sack_df = pbp_clean %>%
  filter(pass == 1, play_type %in% c("Sack", "Fumble Recovery (Opponent)", "Fumble Recovery (Own)")) %>%
  #filter(yards_gained > -20) %>%
  group_by(passer_player_name, offense_play) %>%
  summarize(sacks = n(), mean_epa_sack = mean(EPA), sum_epa_sack = sum(EPA), sack_yards = sum(yards_gained), yards_per_sack = mean(yards_gained)) %>%
  arrange(desc(mean_epa_sack)) %>%
  rename(team = offense_play)

passers_complete = passers_basic %>% left_join(passers_df, by = c("passer_player_name","offense_play" = "team")) %>%
  rename(team = offense_play, mean_epa_pass = mean_epa, sum_epa_pass = sum_epa) %>%
  select_if(names(.) %in% c("passer_player_name","FumbleLost","completions","attempts","TD","INT","pct","ypa","passing_yards","mean_epa_pass", "sum_epa_pass")) %>%
  # (passer_player_name, completions, attempts, TD, INT, FumbleLost,
  #        pct,  ypa, passing_yards) %>%
  arrange(desc(passing_yards)) %>%
  left_join(sack_df, by = c("passer_player_name", "team")) %>%
  left_join(rushers_df, by = c("passer_player_name" = "rusher_player_name", "team")) %>%
  rename(mean_epa_rush = mean_epa, sum_epa_rush = sum_epa) %>%
  mutate(across(everything(), ~replace_na(.x, 0))) %>%
  mutate(total_epa = sum_epa_pass + sum_epa_sack + sum_epa_rush) %>%
  mutate(plays = carries + sacks + attempts) %>%
  mutate(mean_epa = total_epa / plays)

## Create variables with each team's name
team_1 = pbp_clean %>% select(offense_play) %>% distinct() %>% slice(1) %>% pull()
team_2 = pbp_clean %>% select(offense_play) %>% distinct() %>% slice(2) %>% pull()

color_primary = c("Oklahoma" = "#7b0000", "Iowa State" = "black")

scales::show_col(c("yellow","yellow2","yellow3","yellow4"))

## Rushing Chart
pbp_clean %>%
  filter(rush == 1, rusher_player_name != "TEAM") %>%
  filter(yards_gained > -20) %>%
  left_join(rushers_df, by = "rusher_player_name") %>%
  mutate(rusher_mean = paste0(rusher_player_name, ": ", round(mean_epa,2))) %>%
  mutate(rusher_mean = fct_reorder(rusher_mean, mean_epa)) %>%
  ggplot(aes(x = EPA, y = rusher_mean, color = offense_play, text = play_text)) + geom_jitter(alpha = .7, height = .1, size = 3) + geom_vline(xintercept = 0, linetype = 2) +
  scale_color_manual(values = color_primary, name = "Team") +
  ylab("") + theme_bw() + theme(panel.grid.minor = element_blank(), axis.text = element_text(size = 11)) +
  labs(title = paste0("Rushing EPA Game Review: ", team_1, " vs. ", team_2),
       subtitle = "Players sorted by their Average EPA per Carry (excluding sacks)",
       caption = "Chart by @cfbNate |  Data from @CFB_Data via @cfbscrapr")

setwd("C:/Users/NateAndWhitney/Documents/Nate/NCAA/EPA")

## Save the plot above  
ggsave(filename = paste0("rushing_epa_2020_wk",week_num,"_",team_1,team_2,".png"),
       dpi = 300, type = "cairo", width = 10, height = 6, units = "in")


## Receiving Chart
shapes = c("Complete" = 19, "Incomplete" = 1)

pbp_clean %>%
  filter(pass == 1, !is.na(receiver_player_name)) %>%
  filter(yards_gained > -20) %>%
  left_join(receivers_df, by = "receiver_player_name") %>%
  mutate(receiver_mean = paste0(receiver_player_name, ": ", round(mean_epa,2))) %>%
  mutate(receiver_mean = fct_reorder(receiver_mean, mean_epa)) %>%
  ggplot(aes(x = EPA, y = receiver_mean, color = offense_play, shape = pass_complete)) + 
  geom_jitter(alpha = .7, height = .1, size = 3.5) + 
  geom_vline(xintercept = 0, linetype = 2) +
  scale_color_manual(values = color_primary, name = "Team") +
  #scale_fill_manual(values = color_secondary, name = "Team") +
  scale_shape_manual(values = shapes, name = "Complete") +
  ylab("") + theme_bw() + theme(panel.grid.minor = element_blank(), axis.text = element_text(size = 11)) + 
  labs(title = paste0("Receiving EPA Game Review: ", team_1, " vs. ", team_2),
       subtitle = "Players sorted by their Average EPA per Target",
       caption = "Chart by @cfbNate |  Data from @CFB_Data via @cfbscrapr")

ggsave(filename = paste0("receiving_epa_2020_wk",week_num,"_",team_1,team_2,".png"),
       dpi = 300, type = "cairo", width = 10, height = 6, units = "in")


pbp_clean %>%
  ggplot(aes(x = EPA, y = offense_play)) + geom_jitter(alpha = .5, height = .1, size = 3) + geom_vline(xintercept = 0, linetype = 2) +
  ylab("") + theme_bw() + theme(panel.grid.minor = element_blank())

pbp_clean %>%
  filter(EPA > 0) %>%
  group_by(offense_play) %>%
  summarize(n = n(), mean_epa = mean(EPA), sum_epa = sum(EPA))

passers_complete %>%
  filter(attempts > 0) %>%
  select(sum_epa_pass, sum_epa_rush, sum_epa_sack, total_epa, plays, mean_epa) %>% 
  ungroup() %>%
  gt() %>%
  tab_header(
    title = paste0(team_1, " vs. ", team_2),
    subtitle = "QB Comparison"
  ) %>%
  fmt_number(
    columns = vars(sum_epa_pass, sum_epa_rush, sum_epa_sack, total_epa, mean_epa)
  ) %>% 
  cols_label(
    passer_player_name = "Player",
    team = "Team",
    sum_epa_pass = "Passing EPA", 
    sum_epa_sack = "Sack EPA", 
    sum_epa_rush = "Rushing EPA", 
    total_epa = "Total EPA", 
    plays = "Plays",
    mean_epa = "EPA per play"
  ) %>%
  cols_align(
    align = "right",
    columns = vars(sum_epa_pass, sum_epa_rush, sum_epa_sack, total_epa, mean_epa, plays)
  ) %>%
  tab_source_note(
    source_note = "Table by @cfbNate | Data from @CFB_Data via @cfbscrapR"
  ) %>%
gtsave(filename = paste0("passing_epa_2020_wk",week_num,"_",team_1,team_2,".png"))

## this is not complete yet
passers_complete %>%
  filter(attempts > 1) %>%
  #select_if(names(.) %in% c("passer_player_name","FumbleLost","completions","attempts","TD","INT","pct","ypa","passing_yards","mean_epa_pass", "sum_epa_pass"))
  select_if(names(.) %in% c("TD", "INT", "FumbleLost", "sacks", "rush_td", "completions", "attempts", "pct", "passing_yards", "rush_yards", "sack_yards")) %>%
  #select(TD, INT, FumbleLost, sacks, rush_td, completions, attempts, pct, passing_yards, rush_yards, sack_yards) %>% 
  ungroup() %>%
  gt() %>%
  tab_header(
    title = paste0(team_1, " vs. ", team_2),
    subtitle = "QB Comparison - Traditional Stats"
  ) %>%
  fmt_number(
    columns = vars(sum_epa_pass, sum_epa_rush, sum_epa_sack, total_epa, mean_epa),
    suffixing = TRUE
  ) %>% 
  cols_label(
    passer_player_name = "QB",
    team = "Team",
    sum_epa_pass = "Passing EPA", 
    sum_epa_rush = "Rushing EPA", 
    sum_epa_sack = "Sack EPA", 
    total_epa = "Total EPA", 
    mean_epa = "EPA per play"
  ) %>%
  tab_source_note(
    source_note = "Table by @cfbNate | Data from @CFB_Data via @cfbscrapR"
  )

pbp_clean %>% 
  group_by(offense_play, rusher_player_name) %>% 
  summarize(total_epa = sum(EPA, na.rm = TRUE), avg_epa = mean(EPA, na.rm = TRUE), n = n()) %>% 
  arrange(desc(total_epa))

quarters_df = pbp_clean %>%
  filter(offense_play == "Oklahoma") %>%
  group_by(offense_play) %>%
  mutate(play_number = row_number()) %>%
  ungroup() %>%
  group_by(offense_play, period) %>%
  filter(row_number() == n()) %>%
  select(offense_play, play_number) %>%
  ungroup() %>%
  mutate(end_of_qtr = play_number)


## Cumulative EPA Graph
pbp_clean %>%
  group_by(offense_play) %>%
  mutate(play_number = row_number(),
         cu_epa = cummean(EPA),
         ma_epa = zoo::rollapply(EPA,10,mean,align='right',fill=NA)) %>%
  left_join(quarters_df, by = c("period","offense_play","play_number")) %>%
  ggplot(aes(x = play_number, y = cu_epa, color = offense_play)) + geom_line(size = 2) + 
  scale_color_manual(values = color_primary, name = "Offense") +
  geom_hline(yintercept = 0, linetype = 2) +
  xlab("Play Number") + ylab("Cumulative EPA") +
  labs(title = paste0("Cumulative EPA Game Review: ", team_1, " vs. ", team_2),
       caption = "Chart by @cfbNate |  Data from @CFB_Data via @cfbscrapr") +
  #geom_point(aes(x = end_of_qtr, y = ma_epa), size = 5, shape = 18) +
  theme_minimal() + theme(panel.grid.minor = element_blank())

ggsave(filename = paste0("cu_epa_2020_wk",week_num,"_",team_1,team_2,".png"),
       dpi = 300, type = "cairo", width = 10, height = 6, units = "in")


pbp_clean %>%
  filter(offense_play == team) %>%
  group_by(offense_play) %>%
  mutate(play_number = row_number(),
         cu_epa = cummean(EPA),
         ma_epa = zoo::rollapply(EPA,10,mean,align='right',fill=NA)) %>%
  left_join(quarters_df, by = c("period","offense_play","play_number")) %>%
  ggplot(aes(x = play_number, y = ma_epa, color = offense_play)) + geom_line(size = 2) + scale_color_manual(values = color_primary) +
  geom_vline(xintercept = quarters_df$end_of_qtr, linetype = 2) +
  theme_minimal() + theme(panel.grid.minor = element_blank())

# ggsave(filename = paste0("epa_",team_1,team_2,".png"), 
#        dpi = 300, type = "cairo", width = 11, height = 8, units = "in")

pbp_clean %>% filter(offense_play == "Oklahoma") %>% 
  mutate(play_number = row_number()) %>%
  filter(abs(EPA) > 2.5) %>%
  select(play_type, play_text, EPA, play_number, down, distance, yards_gained) %>% as_tibble()

shapes_qb = c("Complete" = 19, "Incomplete" = 1, "Rush" = 17, "Sack" = 4)

## QB Graph
pbp_clean %>%
  filter(passer_player_name %in% qbs | rusher_player_name %in% qbs) %>%
#  filter(yards_gained > -20) %>%
  mutate(qb_player = if_else(is.na(passer_player_name), rusher_player_name, passer_player_name)) %>% #select (-passer_player_name) %>%
  left_join(passers_complete, by = c("qb_player"="passer_player_name")) %>%
  mutate(qb_play_type = case_when(rusher_player_name %in% qbs ~ "Rush",
                                  play_type == "Sack" ~ "Sack",
                                  TRUE ~ pass_complete)) %>%
  mutate(passer_mean = paste0(qb_player, ": ", round(mean_epa,2))) %>%
  mutate(passer_mean = fct_reorder(passer_mean, mean_epa)) %>%
  ggplot(aes(x = EPA, y = passer_mean, color = offense_play, shape = qb_play_type)) + 
  geom_jitter(alpha = .7, height = .1, size = 3.5) + 
  geom_vline(xintercept = 0, linetype = 2) +
  scale_color_manual(values = color_primary, name = "Team") +
  #scale_fill_manual(values = color_secondary, name = "Team") +
  scale_shape_manual(values = shapes_qb, name = "Play Type") +
  ylab("") + theme_bw() + theme(panel.grid.minor = element_blank(), axis.text = element_text(size = 11)) + 
  labs(title = paste0("QB EPA Game Review: ", team_1, " vs. ", team_2),
       subtitle = "Players sorted by their Average EPA per Play",
       caption = "Chart by @cfbNate |  Data from @CFB_Data via @cfbscrapr")

ggsave(filename = paste0("qb_epa_2020_wk",week_num,"_",team_1,team_2,".png"),
       dpi = 300, type = "cairo", width = 10, height = 6, units = "in")

pbp_clean %>% 
  mutate(qb_play_type = case_when(rusher_player_name %in% qbs ~ "Rush",
                                  play_type == "Sack" ~ "Sack",
                                  TRUE ~ pass_complete)) %>% group_by(qb_play_type) %>% summarize(n = n())
