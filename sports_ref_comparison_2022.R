source("C:/Users/NateAndWhitney/Documents/Nate/R Scripts/theme_nm.R")
extrafont::loadfonts(device = "win", quiet = TRUE)
library(tidyverse)
library(rvest)
library(gt)

team_id <- read_csv("C:/Users/NateAndWhitney/Documents/Nate/NCAA/SP+/_team_id.csv")
team_id_short <- team_id %>% select(team_name, team_id)
team_id_full_all <- team_id %>%
  mutate(long_name = paste(team_name, mascot))

sports_ref <- "https://www.sports-reference.com/cfb/years/2022-rushing.html"
sports_ref_html <- sports_ref %>% read_html()

sports_ref_df <- sports_ref_html %>%
  html_table() %>%
  .[[1]] %>%
  janitor::row_to_names(1) %>%
  janitor::clean_names() %>%
  filter(rk != "Rk") %>%
  mutate_at(.vars = c("rk",
                      "g",
                      "cmp",
                      "att",
                      "pct",
                      "yds",
                      "y_a",
                      "ay_a",
                      "td",
                      "int",
                      "rate",
                      "att_2",
                      "yds_2",
                      "avg",
                      "td_2"),
            .funs = as.numeric) %>%
  mutate(tanya = (yds + yds_2 + (20*(td+td_2)) - (45*int)) / (att + att_2) )  %>%
  mutate(tanya_round = round(tanya,1))  %>%
  left_join(team_id %>% select(-conf), by = c("school" = "team_name")) %>%
  mutate(player_abbr = paste0(player, ", ", abbr_espn)) %>%
  mutate(abbr_pad = stringr::str_pad(abbr_espn, 4, side = "left")) %>%
  mutate(tanya_pad = stringr::str_pad(tanya_round, 4, side = "left")) %>%
  mutate(player_abbr_tanya = paste0(player, " | ", abbr_pad, " | ", tanya_pad)) %>%
  rename(rush_att = att_2,
         rush_yds = yds_2,
         rush_td = td_2,
         ypc = avg) %>%
  arrange(-tanya) %>%
  mutate(rk = row_number())
#mutate(across(.cols = dplyr::ends_with("_2"), .fns = paste0("rush_",.)))

through_wk <- 11

#sports_ref_df %>% write_csv(paste0("C:/Users/NateAndWhitney/Documents/Nate/NCAA/tanya_2022-10-23.csv"))
sports_ref_df %>% write_csv(paste0("C:/Users/NateAndWhitney/Documents/Nate/NCAA/NCAA TANYA/tanya_",Sys.Date(),".csv"))

#sports_ref_df <- read_csv("C:/Users/NateAndWhitney/Documents/Nate/NCAA/NCAA TANYA/tanya_2022-10-30.csv")

p5_g5_filter = "P5"

## regular chart with dots ####

sports_ref_df %>%
  filter(p5_g5 == p5_g5_filter) %>%
  #mutate(player_abbr_tanya = paste0(player_abbr, ", ", tanya_round)) %>%
  ggplot(aes(x = tanya, y = reorder(player_abbr_tanya, tanya), fill = color_prim, color = color_sec, label = tanya_round)) +
  geom_point(shape = 21, size = 4) +
  #geom_text(size = 2, family = "JetBrains Mono", fontface = "bold") +
  #ggimage::geom_image(aes(image = logo),size = .02) +
  scale_color_identity() +
  scale_fill_identity() +
  theme_nm_grayblack() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_text(size = 8)) +
  labs(x = "TANYA",
       title = paste0("Total Adjusted Net Yards per Attempt through Week ", through_wk, " | ", p5_g5_filter),
       subtitle = "Represents each QB's total yards from scrimmage per touch, adjusted for TDs and INTs",
       caption =  "TANYA = (Pass_Yds + Rush_Yds + 20*TD - 45*INT) / (Pass_Att + Rush_Att)
       Graphic by @cfbNate | Data from Sports Reference")

ggsave(filename = paste0("C:/Users/NateAndWhitney/Documents/Nate/NCAA/NCAA TANYA/tanya_",p5_g5_filter,"_",Sys.Date(),".png"),
       dpi = "retina", type = "cairo", width = 16, height = 9, units = "in")