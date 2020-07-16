library(tidyverse)
library(cfbscrapR)
library(ggimage)

## Pull CFB Rankings using cfbscrapR
cfb_rankings = tibble()
for (i in 1:16) {
  temp_df = cfb_rankings(year = 2019, week = i)
  cfb_rankings = bind_rows(cfb_rankings, temp_df)
}
cfb_rankings_final = cfbscrapR::cfb_rankings(year = 2019, season_type = "postseason")
cfb_rankings_final = cfb_rankings_final %>% mutate(week = 17)

cfb_rankings = cfb_rankings %>% rbind(cfb_rankings_final)

## Pull cfb_team_info using cfbscrapR
team_info = as_tibble(cfb_team_info(year = 2019))

## Isolate the logos from team_info
logos = team_info %>% 
  select(school, color, logos) %>%
  unnest(logos) %>%
  group_by(school) %>%
  slice(1)

## Define conference to highlight
cfb_rankings %>% select(conference) %>% distinct()
conf = "SEC"  

## create a dataframe for specified conference
conf_polls = cfb_rankings %>% 
  left_join(logos, by = "school") %>%
  filter(poll == "AP Top 25",
         conference == conf)

## create a dataframe for all other conferences
else_polls = cfb_rankings %>% 
  left_join(logos, by = "school") %>%
  filter(poll == "AP Top 25",
         conference != conf)

## AP Poll with Big 12 Teams highlighted (takes a minute to run)
ggplot() + 
  theme_bw() + scale_y_reverse(limits = c(25, 1)) +
  geom_image(data = else_polls, aes(x=week, y = rank, image = logos), size = .04, color = "gray80") +
  geom_image(data = conf_polls, aes(x=week, y = rank, image = logos), size = .04) + theme(legend.position = "none") +
  labs(title = paste0("2019 ",conf," AP Top 25 Rankings"), 
       caption = "Chart by @cfbNate
       Data from @CFB_Data via @cfbscrapr")

## Save graph in your working directory (use getwd() to determine your working directory)
path = paste0(conf,"_ap_poll.png")
ggsave(filename = path, dpi = 300, type = "cairo", width = 10, height = 6, units = "in")
