library(gt)
library(tidyverse)
#library(paletteer)

pbp_2020 = readr::read_rds("C:/Users/NateAndWhitney/Documents/Nate/NCAA/EPA/pbp_2020_datapackage_through_wk13_gz.rds")

## remove bad plays like 69 yard loss for Texas A&M vs Florida by id_play
bad_plays = c("401237102101987301", "401237102101995201", "401237102102855401")

## remove Texas A&M vs Florida by game_id
bad_games = 401237102

pbp_2020 = pbp_2020 %>%
  filter(!id_play %in% bad_plays) %>%
  filter(!game_id %in% bad_games)

# Add play_yards
pbp_2020 <- pbp_2020 %>%
  # mutate(penalty_type = ifelse(play_type == "Penalty",
  #                            ifelse(str_detect(play_text, "false start"), "False Start",
  #                                   ifelse(str_detect(play_text, "illegal formation"), "Illegal Formation",
  #                                          ifelse(str_detect(play_text, "Defensive offside"), "Offsides",
  #                                                 ifelse(str_detect(play_text, "holding"), "Holding",NA))))
  #                            ,NA)) %>%
  # mutate(play_yards = ifelse(str_detect(play_text, "Intentional Grounding"), yards_gained,
  #                            ifelse(str_detect(play_text, "Penalty"), str_extract(play_text,"(?<=for )\\d+"), yards_gained))) %>%
  mutate(play_yards = case_when(str_detect(play_text, "Intentional Grounding") ~ yards_gained,
                                str_detect(play_text, "intentional grounding") ~ yards_gained,
                                str_detect(play_text, "declined") ~ yards_gained,
                                str_detect(play_text, "Penalty") ~ as.numeric(str_extract(play_text,"(?<=for )\\d+")),
                                TRUE ~ yards_gained)) %>%
  mutate(play_yards = ifelse(is.na(play_yards), 
                             yes = ifelse(str_detect(play_text, "Penalty"), 
                                          yes = as.numeric(str_extract(play_text,"(?<=for a loss of )\\d+"))*(-1), 
                                          no = play_yards), 
                             no = play_yards)) %>%
  mutate(play_yards = ifelse(is.na(play_yards), 
                             yes = ifelse(str_detect(play_text, "Penalty"), 
                                          yes = ifelse(str_detect(play_text,"for no gain"), yes = 0, no = play_yards), 
                                          no = play_yards), 
                             no = play_yards)) %>%
  mutate(play_yards = ifelse(is.na(play_yards), 
                             yes = ifelse(str_detect(play_text, "Penalty"), 
                                          yes = ifelse(str_detect(play_text,"pass incomplete"), yes = 0, no = play_yards), 
                                          no = play_yards), 
                             no = play_yards)) %>%
  mutate(play_yards = as.numeric(play_yards))

pbp_2020 <- pbp_2020 %>%
  mutate(yards_clean = ifelse(is.na(yds_penalty), yards_gained, yards_gained - yds_penalty))

## Reclassify ND as ACC
pbp_2020 = pbp_2020 %>% 
  mutate(offense_conference = ifelse(offense_play == "Notre Dame", "ACC", offense_conference))

## Define Passers
passers_df = pbp_2020 %>%
  filter(pass == 1, !play_type %in% c("Sack", "Fumble Recovery (Opponent)")) %>%
  #filter(play_yards > -20) %>%
  group_by(passer_player_name, offense_play) %>%
  summarize(dropbacks = n(), mean_epa = mean(EPA), sum_epa = sum(EPA), passing_yards = sum(play_yards), ypa = mean(play_yards)) %>%
  arrange(desc(mean_epa)) %>%
  rename(team = offense_play)

## Define Rusher Position
qbs = passers_df %>%
  filter(dropbacks > 3) %>%
  select(passer_player_name) %>%
  distinct()

qbs = as.vector(qbs$passer_player_name)

pbp_2020 = pbp_2020 %>%
  mutate(rush_pos = case_when(
    is.na(rusher_player_name) ~ NA_character_,
    rusher_player_name %in% qbs ~ "QB",
    !rusher_player_name %in% qbs ~ "RB"))

## Create Rushers df
rushers_df = pbp_2020 %>%
  filter(rush == 1, !is.na(EPA)) %>%
  #filter(play_yards > -20) %>%
  group_by(rusher_player_name, offense_play, rush_pos, offense_conference) %>%
  summarize(carries = n(), 
            mean_epa = mean(EPA), 
            sum_epa = sum(EPA), 
            rush_yards = sum(yards_clean), 
            ypc = mean(play_yards), 
            rush_td = sum(touchdown),
            rush_fd = sum(first_by_yards)) %>%
  arrange(desc(sum_epa)) %>%
  rename(team = offense_play)

## define function for retreiving logos
gt_theme_pff <- function(data, ...) {
  data %>%
    # Add team logos w/ web_image
    text_transform(
      locations = cells_body(
        vars(logos)
      ),
      fn = function(x) {
        web_image(
          url = x,
          height = 20
        )
      }
    )
}

## define function for adding bar chart to table
bar_chart <- function(value, color = "red"){
  glue::glue("<span style=\"display: inline-block; direction: ltr; border-radius: 4px; padding-right: 2px; background-color: {color}; color: {color}; width: {value}%\"> &nbsp; </span>") %>% 
    as.character() %>% 
    gt::html()
}

conferences = rushers_df %>% 
  ungroup() %>% 
  select(offense_conference) %>% 
  distinct() %>% 
  filter(!is.na(offense_conference)) %>%
  arrange(offense_conference) %>% 
  pull()

#conferences = c("Pac-12")

for (conf in conferences) {

## Conference Leaders by Pos with Bar
rushers_df %>% 
  filter(offense_conference == conf) %>% 
  filter(carries > 5) %>%
  ungroup() %>% 
  left_join(logos, by = c("team" = "school")) %>%
  #filter(rush_pos == "QB") %>%
  mutate(Rank = row_number()) %>%
  mutate(
    epa_plot = ifelse(sum_epa>0, sum_epa/max(sum_epa) * 100, 0),
    #epa_plot = sum_epa/max(sum_epa) * 100,
    epa_plot = purrr::map(epa_plot, ~bar_chart(value = .x, color = "#6EB5FF"))
  ) %>%
  filter(Rank <= 20) %>%
  select(Rank, rush_pos, rusher_player_name, logos, carries, rush_yards, ypc, mean_epa, sum_epa, epa_plot) %>% 
  arrange(desc(sum_epa)) %>%
  gt %>% 
  #tab_options(table.font.names = "monospace") %>%
  tab_header(
    title = paste0(conf, " Rushing EPA Leaders"),
    subtitle = "Minimum of 50 Carries (Excluding Sacks)"
  ) %>%
  cols_label(rusher_player_name = "Rusher", 
             rush_pos = "Pos",
             logos = "Team",
             mean_epa = "Avg EPA",
             sum_epa = "Total EPA",
             carries = "Carries",
             rush_yards = "Rushing Yds",
             ypc = "YPC",
             epa_plot = "Total EPA Bar") %>%
  fmt_number(columns = vars(ypc, sum_epa), decimals = 2) %>%
  fmt_number(columns = vars(mean_epa), decimals = 3) %>%
  fmt_number(columns = vars(rush_yards), use_seps = TRUE, decimals = 0) %>%
  cols_align(columns = vars(Rank, carries), align = "right") %>%
  cols_align(columns = vars(logos), align = "center") %>%
  cols_align(columns = vars(epa_plot), align = "left") %>%
  data_color(
    columns = vars(carries, rush_yards, mean_epa, ypc, sum_epa),
    colors = scales::col_numeric(
      palette = c("white", "#6EB5FF"),
      domain = NULL)
  ) %>%
  tab_source_note(
    source_note = "Table by @cfbNate | Data from @CFB_Data via @cfbscrapR") %>%
  gt_theme_pff() %>%
  gtsave(filename = paste0("C:/Users/NateAndWhitney/Documents/Nate/NCAA/EPA/2020 Rushing EPA/rushing_leaders_",conf,"_wk15.png"))
}


## National Top Rushers Graph
rushers_df %>% ungroup() %>% left_join(logos, by = c("team" = "school")) %>%
  filter(carries >= 50) %>%
  filter(rush_pos == "RB") %>%
  mutate(Rank = row_number()) %>%
  filter(Rank < 21) %>%
  mutate(
    epa_plot = sum_epa/max(sum_epa) * 100,
    epa_plot = purrr::map(epa_plot, ~bar_chart(value = .x, color = "#6EB5FF"))
  ) %>%
  select(Rank, rush_pos, rusher_player_name, logos, carries, rush_yards, ypc, mean_epa, sum_epa, epa_plot) %>% 
  arrange(desc(sum_epa)) %>%
  gt %>% 
  #tab_options(table.font.names = "monospace") %>%
  tab_header(
    title = "NCAA Rushing EPA Leaders",
    subtitle = "Minimum of 50 Carries (Excluding Sacks)"
  ) %>%
  cols_label(rusher_player_name = "Rusher", 
             rush_pos = "Pos",
             logos = "Team",
             mean_epa = "Avg EPA",
             sum_epa = "Total EPA",
             carries = "Carries",
             rush_yards = "Rushing Yds",
             ypc = "YPC",
             epa_plot = "Total EPA Bar") %>%
  fmt_number(columns = vars(ypc, sum_epa), decimals = 2) %>%
  fmt_number(columns = vars(mean_epa), decimals = 3) %>%
  fmt_number(columns = vars(rush_yards), use_seps = TRUE, decimals = 0) %>%
  cols_align(columns = vars(Rank, carries), align = "right") %>%
  cols_align(columns = vars(logos), align = "center") %>%
  cols_align(columns = vars(epa_plot), align = "left") %>%
  data_color(
    columns = vars(carries, rush_yards, sum_epa, mean_epa, ypc),
    colors = scales::col_numeric(
      palette = c("white", "#6EB5FF"),
      domain = NULL)
  ) %>%
  tab_source_note(
    source_note = "Table by @cfbNate | Data from @CFB_Data via @cfbscrapR") %>%
  gt_theme_pff() %>%
  gtsave(filename = "C:/Users/NateAndWhitney/Documents/Nate/NCAA/EPA/2020 Rushing EPA/rushing_leaders_ncaa_rbs_wk15.png")


## Team Specific Rushing Charts
teams = c("Texas A&M")
carry_threshold = 5

for (tm in teams) {
  
rushers_df %>% 
  filter(team == tm) %>%
  filter(rusher_player_name != "TEAM") %>%
  filter(carries > carry_threshold) %>%
  ungroup() %>% 
  left_join(logos, by = c("team" = "school")) %>%
  #filter(rush_pos == "QB") %>%
  mutate(Rank = row_number()) %>%
  mutate(
    epa_plot = ifelse(sum_epa>0, sum_epa/max(sum_epa) * 100, 0),
    #epa_plot = sum_epa/max(sum_epa) * 100,
    epa_plot = purrr::map(epa_plot, ~bar_chart(value = .x, color = "#6EB5FF"))
  ) %>%
  filter(Rank <= 20) %>%
  select(Rank, rush_pos, rusher_player_name, logos, carries, rush_yards, ypc, mean_epa, sum_epa, rush_td, rush_fd, epa_plot) %>% 
  arrange(desc(sum_epa)) %>%
  gt %>% 
  #tab_options(table.font.names = "monospace") %>%
  tab_header(
    title = paste0(tm, " Rushing EPA Leaders"),
    subtitle = paste0("Minimum of ",carry_threshold," Carries (Excluding Sacks)")
  ) %>%
  cols_label(rusher_player_name = "Rusher", 
             rush_pos = "Pos",
             logos = "Team",
             mean_epa = "Avg EPA",
             sum_epa = "Total EPA",
             carries = "Carries",
             rush_yards = "Rushing Yds",
             ypc = "YPC",
             epa_plot = "Total EPA Bar") %>%
  fmt_number(columns = vars(ypc, sum_epa), decimals = 2) %>%
  fmt_number(columns = vars(mean_epa), decimals = 3) %>%
  fmt_number(columns = vars(rush_yards), use_seps = TRUE, decimals = 0) %>%
  cols_align(columns = vars(Rank, carries), align = "right") %>%
  cols_align(columns = vars(logos), align = "center") %>%
  cols_align(columns = vars(epa_plot), align = "left") %>%
  data_color(
    columns = vars(carries, rush_yards, mean_epa, ypc, sum_epa),
    colors = scales::col_numeric(
      palette = c("white", "#6EB5FF"),
      domain = NULL)
  ) %>%
  tab_source_note(
    source_note = "Table by @cfbNate | Data from @CFB_Data via @cfbscrapR") %>%
  gt_theme_pff() %>%
  gtsave(filename = paste0("C:/Users/NateAndWhitney/Documents/Nate/NCAA/EPA/2020 Rushing EPA/rushing_leaders_",tm,"_wk15.png"))
}

rushers_temp = rushers_df %>% ungroup() %>% left_join(logos, by = c("team" = "school")) %>%
  filter(carries >= 50) %>%
  filter(rush_pos == "RB") %>%
  mutate(Rank = row_number())

pbp_2020 %>% 
  filter(rusher_player_name == "Kellen Mond") %>% 
  filter(defense_play == "Florida") %>%
  select(defense_play, play_text, play_type, EPA, down, distance, yards_to_goal, drive_id, play_id) %>%
  arrange(EPA) %>%
  as_tibble()

pbp_2020 %>% 
  filter(rusher_player_name == "Kellen Mond") %>% 
  group_by(defense_play) %>%
  summarize(sum_epa = sum(EPA))


pbp_2020 %>% 
  filter(rusher_player_name == "Kellen Mond") %>% 
  group_by(offense_play, rusher_player_name, play_type) %>% 
  summarize(carries = n(),
            sum_epa = sum(EPA),
            mean_epa = mean(EPA)) %>%
  ungroup() %>%
  left_join(logos, by = c("offense_play" = "school")) %>%
  #filter(rush_pos == "QB") %>%
  # mutate(
  #   epa_plot = ifelse(sum_epa>0, sum_epa/max(sum_epa) * 100, 0),
  #   #epa_plot = sum_epa/max(sum_epa) * 100,
  #   epa_plot = purrr::map(epa_plot, ~bar_chart(value = .x, color = "#6EB5FF"))
  # ) %>%
  select(rusher_player_name, play_type, logos, carries, mean_epa, sum_epa) %>% 
  mutate(play_type = ifelse(play_type == "Fumble Recovery (Opponent)", "Lost Fumble", play_type)) %>%
  arrange(desc(sum_epa)) %>%
  gt %>% 
  #tab_options(table.font.names = "monospace") %>%
  tab_header(
    title = "Kellen Mond Rushing EPA",
    subtitle = "Excluding Sacks"
  ) %>%
  cols_label(rusher_player_name = "Rusher", 
             logos = "Team",
             play_type = "Play Type",
             mean_epa = "Avg EPA",
             sum_epa = "Total EPA",
             carries = "Carries") %>%
  fmt_number(columns = vars(sum_epa), decimals = 2) %>%
  fmt_number(columns = vars(mean_epa), decimals = 3) %>%
  cols_align(columns = vars(carries), align = "right") %>%
  cols_align(columns = vars(logos), align = "center") %>%
  data_color(
    columns = vars(carries),
    colors = scales::col_numeric(
      palette = c("white", "#6EB5FF"),
      domain = NULL)
  ) %>%
  data_color(
    columns = vars(sum_epa, mean_epa),
    colors = scales::col_numeric(
      palette = c("red","white", "#6EB5FF"),
      domain = NULL)
  ) %>%
  tab_source_note(
    source_note = "Table by @cfbNate | Data from @CFB_Data via @cfbscrapR") %>%
  gt_theme_pff() %>%
  gtsave(filename = paste0("C:/Users/NateAndWhitney/Documents/Nate/NCAA/EPA/2020 Rushing EPA/kellen_mond_wk15.png"))
