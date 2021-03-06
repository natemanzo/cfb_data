library(tidyverse)
library(gt)
# install.packages("remotes")
# remotes::install_github("jthomasmock/espnscrapeR")
library(espnscrapeR)
extrafont::loadfonts(device = "win", quiet = TRUE)

df <- read_csv("https://raw.githubusercontent.com/drmartylawrence/recruitingDraftValue/main/crootDraftDataset.csv")
#df <- read_csv("C:/Users/NateAndWhitney/Documents/Nate/NCAA/crootDraftDataset_drmartylawrence.csv")

college_team_info <- read_csv("https://raw.githubusercontent.com/natemanzo/cfb_data/master/_team_id.csv")

college_team_logo <- college_team_info %>% 
  select(team_name, college_logo = logo)

pro_team_info <- espnscrapeR::get_nfl_teams()

pro_team_logo <- pro_team_info %>%
  select(team_nickname, pro_logo = logo) %>%
  mutate(team_nickname = ifelse(team_nickname == "Jax", "Jacksonville", team_nickname))

rating_stars <- function(rating, max_rating = 5) {
  rounded_rating <- floor(rating + 0.5)  # always round up
  stars <- lapply(seq_len(max_rating), function(i) {
    if (i <= rounded_rating) fontawesome::fa("star", fill= "orange") else fontawesome::fa("star", fill= "grey")
  })
  label <- sprintf("%s out of %s", rating, max_rating)
  div_out <- htmltools::div(title = label, "aria-label" = label, role = "img", stars)
  
  as.character(div_out) %>% 
    gt::html()
}

## Top DVOE Recruits ####
df %>%
  mutate(nflTeam = case_when(name == "Saquon Barkley" ~ "Giants",
                             name == "Quinnen Williams" ~ "Jets",
                             name == "Sam Darnold" ~ "Jets",
                             TRUE ~ nflTeam)) %>%
  left_join(college_team_logo, by = c("collegeTeam" = "team_name")) %>%
  left_join(pro_team_logo, by = c("nflTeam" = "team_nickname")) %>%
  select(name, position, stars, ranking, college_logo, pro_logo, draftYear, draftPick, recDVOE) %>%
  mutate(stars = map(stars, rating_stars)) %>%
  arrange(-recDVOE) %>%
  mutate(rank = row_number()) %>%
  relocate(rank) %>%
  head(10) %>%
  gt() %>%
  espnscrapeR::gt_theme_538() %>%
  #gt_theme_espn() %>%
  text_transform(
    locations = cells_body(
      vars(college_logo, pro_logo)
    ),
    fn = function(x) {
      web_image(
        url = x,
        height = 25
      )
    }
  ) %>%
  tab_header(
    title = "Top Recruits in DVOE"
  ) %>%
  cols_label(rank = "#", 
             position = "POS",
             stars = "Rec Stars",
             ranking = "Recruit Rank",
             college_logo = "Draft School",
             pro_logo = "NFL Team", 
             draftYear = "Draft Year", 
             draftPick = "Draft Pick",
             recDVOE = "DVOE") %>%
  fmt_number(columns = vars(ranking), use_seps = TRUE, decimals = 0) %>%
  cols_align(columns = vars(college_logo, pro_logo), align = "center") %>%
  data_color(
    columns = vars(recDVOE),
    colors = scales::col_numeric(
      palette = c("white", "#6EB5FF"),
      domain = NULL)
  ) %>%
  data_color(
    columns = vars(ranking),
    colors = scales::col_numeric(
      #palette = c("#6EB5FF", "white", "white","#DC143C"),
      #palette = c("white","#DC143C"),
      palette = c("white", "#6EB5FF"),
      domain = NULL)
  ) %>%
  tab_source_note(
    source_note = "Analysis by @drmartylawrence | Table by @cfbNate"
  ) %>%
  tab_style(
    style = list(
      cell_borders(
        sides = c("bottom", "top"),
        weight = NULL
      )
    ),
    locations = list(
      cells_body(
        columns = everything(),
        rows = everything()
      )
    )
  ) #%>% gtsave("top_recruits_v2.png")

## Bottom DVOE Recruits ####
df %>%
  mutate(nflTeam = case_when(name == "Ronald Powell" ~ "NO",
                             TRUE ~ nflTeam)) %>%
  left_join(college_team_logo, by = c("collegeTeam" = "team_name")) %>%
  left_join(pro_team_logo, by = c("nflTeam" = "team_nickname")) %>%
  select(name, position, stars, ranking, college_logo, pro_logo, draftYear, draftPick, recDVOE) %>%
  mutate(stars = map(stars, rating_stars)) %>%
  arrange(recDVOE) %>%
  mutate(rank = row_number()) %>%
  relocate(rank) %>%
  head(10) %>%
  gt() %>%
  espnscrapeR::gt_theme_538() %>%
  #gt_theme_espn() %>%
  text_transform(
    locations = cells_body(
      vars(college_logo, pro_logo)
    ),
    fn = function(x) {
      if_else(is.na(x), " ", 
        web_image(
          url = x,
          height = 25
        ))
    }
  ) %>%
  tab_header(
    title = "Bottom Recruits in DVOE"
  ) %>%
  cols_label(rank = "#",
             position = "POS",
             stars = "Rec Stars",
             ranking = "Recruit Rank",
             college_logo = "Draft School",
             pro_logo = "NFL Team", 
             draftYear = "Draft Year", 
             draftPick = "Draft Pick",
             recDVOE = "DVOE") %>%
  fmt_number(columns = vars(ranking), use_seps = TRUE, decimals = 0) %>%
  fmt_missing(columns = vars(draftPick), rows = NULL, missing_text = "") %>%
  cols_align(columns = vars(college_logo, pro_logo), align = "center") %>%
  data_color(
    columns = vars(recDVOE, ranking),
    colors = scales::col_numeric(
      palette = c( "#6EB5FF","white"),
      domain = NULL)
  ) %>%
  tab_source_note(
    source_note = "Analysis by @drmartylawrence | Table by @cfbNate"
  ) %>%
  tab_style(
    style = list(
      cell_borders(
        sides = c("bottom", "top"),
        weight = NULL
      )
    ),
    locations = list(
      cells_body(
        columns = everything(),
        rows = everything()
      )
    )
  ) #%>% gtsave("bottom_recruits.png")

## DVOE by College Draft Class ####
dv_class <- read_csv("https://raw.githubusercontent.com/drmartylawrence/recruitingDraftValue/main/dvClass.csv")

dv_class %>%
  janitor::clean_names() %>%
  slice(1:10, 1633:1642) %>%
  left_join(college_team_logo, by = c("school" = "team_name")) %>%
  select(rk, college_logo, class, draft_value, exp_draft_value, dvoe) %>%
  gt() %>%
  espnscrapeR::gt_theme_538() %>%
  text_transform(
    locations = cells_body(
      vars(college_logo)
    ),
    fn = function(x) {
      if_else(is.na(x), " ", 
              web_image(
                url = x,
                height = 25
              ))
    }
  ) %>%
  tab_header(
    title = "Top and Bottom Draft Classes by DVOE"
  ) %>%
  cols_label(rk = "#",
             college_logo = "School",
             draft_value = "Draft Value",
             exp_draft_value = "Exp Draft Value",
             dvoe = "DVOE") %>%
  #fmt_number(columns = vars(ranking), use_seps = TRUE, decimals = 0) %>%
  cols_align(columns = vars(college_logo), align = "center") %>%
  data_color(
    columns = vars(draft_value, exp_draft_value),
    colors = scales::col_numeric(
      palette = c("white", "#6EB5FF"),
      domain = NULL)
  ) %>%
  data_color(
    columns = vars(dvoe),
    colors = scales::col_numeric(
      palette = c("red","white", "#6EB5FF"),
      domain = NULL)
  ) %>%    
  tab_source_note(
    source_note = "Analysis by @drmartylawrence | Table by @cfbNate"
  ) %>%
  tab_style(
    style = list(
      cell_borders(
        sides = c("bottom", "top"),
        weight = NULL
      )
    ),
    locations = list(
      cells_body(
        columns = everything(),
        rows = everything()
      )
    )
  ) %>% gtsave("top_and_bottom_classes_by_dvoe.png")

## DVOE by School ####
dv_college <- read_csv("https://raw.githubusercontent.com/drmartylawrence/recruitingDraftValue/main/dvCollege.csv")

dv_college %>%
  janitor::clean_names() %>%
  slice(1:10, 285:294) %>%
  left_join(college_team_logo, by = c("school" = "team_name")) %>%
  select(rk = x1, college_logo, draft_value, exp_draft_value, dvoe) %>%
  gt() %>%
  espnscrapeR::gt_theme_538() %>%
  text_transform(
    locations = cells_body(
      vars(college_logo)
    ),
    fn = function(x) {
      if_else(is.na(x), " ", 
              web_image(
                url = x,
                height = 25
              ))
    }
  ) %>%
  tab_header(
    title = "Top and Bottom Schools by DVOE"
  ) %>%
  cols_label(rk = "#",
             college_logo = "School",
             draft_value = "Draft Value",
             exp_draft_value = "Exp Draft Value",
             dvoe = "DVOE") %>%
  fmt_number(columns = vars(draft_value, exp_draft_value), use_seps = TRUE, decimals = 1) %>%
  cols_align(columns = vars(college_logo), align = "center") %>%
  data_color(
    columns = vars(draft_value, exp_draft_value),
    colors = scales::col_numeric(
      palette = c("white", "#6EB5FF"),
      domain = NULL)
  ) %>%
  data_color(
    columns = vars(dvoe),
    colors = scales::col_numeric(
      palette = c("red","white", "#6EB5FF"),
      domain = NULL)
  ) %>%
  tab_source_note(
    source_note = "Analysis by @drmartylawrence | Table by @cfbNate"
  ) %>%
  tab_style(
    style = list(
      cell_borders(
        sides = c("bottom", "top"),
        weight = NULL
      )
    ),
    locations = list(
      cells_body(
        columns = everything(),
        rows = everything()
      )
    )
  ) #%>% gtsave("top_and_bottom_schools_by_dvoe.png")

## DVOE by Conference ####
dv_conf <- read_csv("https://raw.githubusercontent.com/drmartylawrence/recruitingDraftValue/main/dvConference.csv")

big_east_df = tibble(conf = "Big East",
                     conf_logo = "https://a.espncdn.com/combiner/i?img=/i/teamlogos/ncaa_conf/500/big_east.png")

ncaa_logo = "https://www.freepnglogos.com/uploads/ncaa-png-logo/ncaa-png-logo-0.png"

conf_logos <- college_team_info %>%
  select(conf, conf_logo) %>%
  distinct() %>%
  rbind(big_east_df) %>%
  mutate(conf_logo = recode(conf_logo, 
                            "https://a.espncdn.com/combiner/i?img=/i/teamlogos/ncaa_conf/500/independents.png" = 
                              ncaa_logo))

dv_conf %>%
  janitor::clean_names() %>%
  mutate(conference = recode(conference, "American Athletic" = "AAC",
                             "FBS Independents" = "Independent",
                             "Mountain West" = "MWC",
                             "Mid-American" = "MAC",
                             "Conference USA" = "C-USA")) %>%
  left_join(conf_logos, by = c("conference" = "conf")) %>%
  mutate(rk = row_number()) %>%
  select(rk, conf_logo, draft_value, exp_draft_value, dvoe) %>%
  gt() %>%
  espnscrapeR::gt_theme_538() %>%
  text_transform(
    locations = cells_body(
      vars(conf_logo)
    ),
    fn = function(x) {
      if_else(is.na(x), " ", 
              web_image(
                url = x,
                height = 25
              ))
    }
  ) %>%
  tab_header(
    title = "Top and Bottom Conferences by DVOE"
  ) %>%
  cols_label(rk = "#",
             conf_logo = "Conference",
             draft_value = "Draft Value",
             exp_draft_value = "Exp Draft Value",
             dvoe = "DVOE") %>%
  fmt_number(columns = vars(draft_value, exp_draft_value), use_seps = TRUE, decimals = 1) %>%
  cols_align(columns = vars(conf_logo), align = "center") %>%
  data_color(
    columns = vars(draft_value, exp_draft_value),
    colors = scales::col_numeric(
      palette = c("white", "#6EB5FF"),
      domain = NULL)
  ) %>%
  data_color(
    columns = vars(dvoe),
    colors = scales::col_numeric(
      palette = c("red","white", "#6EB5FF"),
      domain = NULL)
  ) %>%
  tab_source_note(
    source_note = "Analysis by @drmartylawrence | Table by @cfbNate"
  ) %>%
  tab_style(
    style = list(
      cell_borders(
        sides = c("bottom", "top"),
        weight = NULL
      )
    ),
    locations = list(
      cells_body(
        columns = everything(),
        rows = everything()
      )
    )
  ) #%>% gtsave("top_and_bottom_conferences_by_dvoe.png")

## DVOE by Coach ####
dv_coach <- read_csv("https://raw.githubusercontent.com/drmartylawrence/recruitingDraftValue/main/dvRecCoach.csv")

dv_coach %>%
  janitor::clean_names() %>%
  slice(1:10, 221:230) %>%
  select(rk = x1, coach, draft_value = draft_valu, exp_draft_value, dvoe) %>%
  gt() %>%
  espnscrapeR::gt_theme_538() %>%
  tab_header(
    title = "Top and Bottom Coaches by DVOE"
  ) %>%
  cols_label(rk = "#",
             draft_value = "Draft Value",
             exp_draft_value = "Exp Draft Value",
             dvoe = "DVOE") %>%
  fmt_number(columns = vars(draft_value, exp_draft_value, dvoe), use_seps = TRUE, decimals = 1) %>%
  data_color(
    columns = vars(draft_value, exp_draft_value),
    colors = scales::col_numeric(
      palette = c("white", "#6EB5FF"),
      domain = NULL)
  ) %>%
  data_color(
    columns = vars(dvoe),
    colors = scales::col_numeric(
      palette = c("red","white", "white","#6EB5FF"),
      domain = NULL)
  ) %>%
  tab_source_note(
    source_note = "Analysis by @drmartylawrence | Table by @cfbNate"
  ) %>%
  tab_style(
    style = list(
      cell_borders(
        sides = c("bottom", "top"),
        weight = NULL
      )
    ),
    locations = list(
      cells_body(
        columns = everything(),
        rows = everything()
      )
    )
  ) #%>% gtsave("top_and_bottom_coaches_by_dvoe.png")

## DVOE by Coach - Upperclassmen ####
dv_coach_upper <- read_csv("https://raw.githubusercontent.com/drmartylawrence/recruitingDraftValue/main/dvDraftCoach.csv")

dv_coach_upper %>%
  janitor::clean_names() %>%
  slice(1:10, 219:228) %>%
  select(rk = x1, coach, draft_value, exp_draft_value, dvoe) %>%
  gt() %>%
  espnscrapeR::gt_theme_538() %>%
  tab_header(
    title = "Top and Bottom Coaches for Upperclassmen by DVOE"
  ) %>%
  cols_label(rk = "#",
             draft_value = "Draft Value",
             exp_draft_value = "Exp Draft Value",
             dvoe = "DVOE") %>%
  fmt_number(columns = vars(draft_value, exp_draft_value, dvoe), use_seps = TRUE, decimals = 1) %>%
  data_color(
    columns = vars(draft_value, exp_draft_value),
    colors = scales::col_numeric(
      palette = c("white", "#6EB5FF"),
      domain = NULL)
  ) %>%
  data_color(
    columns = vars(dvoe),
    colors = scales::col_numeric(
      palette = c("red","white", "white","#6EB5FF"),
      domain = NULL)
  ) %>%
  tab_source_note(
    source_note = "Analysis by @drmartylawrence | Table by @cfbNate"
  ) %>%
  tab_style(
    style = list(
      cell_borders(
        sides = c("bottom", "top"),
        weight = NULL
      )
    ),
    locations = list(
      cells_body(
        columns = everything(),
        rows = everything()
      )
    )
  ) #%>% gtsave("top_and_bottom_coaches_upper_by_dvoe.png")


## DVOE by Position ####
dv_pos <- read_csv("https://raw.githubusercontent.com/drmartylawrence/recruitingDraftValue/main/dvPos.csv")

dv_pos %>%
  janitor::clean_names() %>%
  mutate(rk = row_number()) %>%
  select(rk, position, draft_value, exp_draft_value, dvoe) %>%
  gt() %>%
  espnscrapeR::gt_theme_538() %>%
  tab_header(
    title = "Top and Bottom Positions by DVOE"
  ) %>%
  cols_label(rk = "#",
             draft_value = "Draft Value",
             exp_draft_value = "Exp Draft Value",
             dvoe = "DVOE") %>%
  fmt_number(columns = vars(draft_value, exp_draft_value, dvoe), use_seps = TRUE, decimals = 0) %>%
  data_color(
    columns = vars(draft_value, exp_draft_value),
    colors = scales::col_numeric(
      palette = c("white", "#6EB5FF"),
      domain = NULL)
  ) %>%
  data_color(
    columns = vars(dvoe),
    colors = scales::col_numeric(
      palette = c("red","white", "white","#6EB5FF"),
      domain = NULL)
  ) %>%
  tab_source_note(
    source_note = "Analysis by @drmartylawrence | Table by @cfbNate"
  ) %>%
  tab_style(
    style = list(
      cell_borders(
        sides = c("bottom", "top"),
        weight = NULL
      )
    ),
    locations = list(
      cells_body(
        columns = everything(),
        rows = everything()
      )
    )
  ) %>% gtsave("top_and_bottom_positions_by_dvoe.png")
