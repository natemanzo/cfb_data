library(cfbscrapR)
library(tidyverse)

## Play by play data from cfbscrapR ---------------------------
pbp_2019 <- data.frame()

for(i in 1:15){
  model <- cfb_pbp_data(year = 2019, week = i, epa_wpa = TRUE, season_type = "regular")
  df <- data.frame(model)
  df = df %>% mutate(week = i)
  pbp_2019 <- bind_rows(pbp_2019, df)
}

## Include bowl data as well - some people might want to exclude this section
bowl_df <- cfb_pbp_data(year = 2019, epa_wpa = TRUE, season_type = "postseason")
bowl_df = bowl_df %>% mutate(week = 16)
pbp_2019 <- bind_rows(pbp_2019, bowl_df)

## Clean up the data-------------------------------------------
pbp_2019 <- pbp_2019 %>%
  rename(offense = offense_play,
         defense = defense_play) %>%
  mutate(offense = recode(offense, "San José State" = "San Jose State")) %>%
  mutate(defense = recode(defense, "San José State" = "San Jose State"))

## read in team_id data----------------------------------------
## team_id is used to determine whether each team is FCS or FBS
team_id = read_csv("https://raw.githubusercontent.com/natemanzo/cfb_data/master/_team_id.csv")
team_id_short = team_id %>% select(Team, TeamID, AbbrESPN)

## calculate season long raw epa averages ---------------------
def_epa_raw = pbp_2019 %>%
  filter(rush == 1 | pass == 1) %>% 
  group_by(defense) %>%
  summarize(def_epa_raw = mean(EPA)) %>%
  arrange(def_epa_raw) %>%
  rename(Team = defense) %>%
  left_join(team_id_short, by = "Team") %>%
  filter(!is.na(TeamID)) %>%
  mutate(Rank = row_number()) %>%
  mutate(TeamRank = paste0(Team, " #", Rank))

off_epa_raw = pbp_2019 %>%
  filter(rush == 1 | pass == 1) %>% 
  group_by(offense) %>%
  summarize(off_epa_raw = mean(EPA)) %>%
  arrange(desc(off_epa_raw)) %>%
  rename(Team = offense) %>%
  left_join(team_id_short, by = "Team") %>%
  filter(!is.na(TeamID)) %>%
  mutate(Rank = row_number()) %>%
  mutate(TeamRank = paste0(Team, " #", Rank))

## Initial EPA adjustments -----------------------------------
## 1. def_epa_wk_i is calculating the average EPA allowed per defense in week i 
## 2. def_epa_wk_else is calculating the average EPA allowed per defense in all weeks EXCEPT week i
## 3. def_epa_mean_else is calculating the average EPA allowed by ALL defenses in all weeks EXCEPT week i
## 4. the difference between each team's def_epa_wk_else and def_epa_mean_else is the adjustment factor
## 5. adjustments_def captures the weekly adjustments for each defense in each week i
## 6. a single defensive adjustment is calculated for all FCS games
## 7. raw epa + adjustment factor = adjusted EPA
## 8. repeat all previous steps for offense

weeks = 1:16
adjustments_def = data.frame()
adjustments_off = data.frame()
for(i in weeks)  {
  
  ## > Def EPA Adjusted-------------------------------
  
  def_epa_wk_else = pbp_2019 %>%
    filter(week != i) %>%
    filter(rush == 1 | pass == 1) %>% 
    group_by(defense) %>%
    summarize(def_epa = mean(EPA)) %>%
    arrange(desc(def_epa)) %>%
    rename(Team = defense) %>%
    left_join(team_id_short, by = "Team") %>%
    filter(!is.na(TeamID)) %>%
    rename(def_epa_else = def_epa) %>%
    select(Team, def_epa_else)
  
  def_epa_mean_else = mean(def_epa_wk_else$def_epa_else)
  
  weekly_adjustments_def = def_epa_wk_else %>% 
    mutate(def_epa_adj_fact = def_epa_mean_else - def_epa_else) %>% 
    mutate(week = i) %>%
    select(week, defense = Team, def_epa_adj_fact)
  
  adjustments_def = rbind(adjustments_def, weekly_adjustments_def)
  
  ## > Off EPA Adjusted-------------------------------
  
  off_epa_wk_else = pbp_2019 %>%
    filter(week != i) %>%
    filter(rush == 1 | pass == 1) %>% 
    group_by(offense) %>%
    summarize(off_epa = mean(EPA)) %>%
    arrange(desc(off_epa)) %>%
    rename(Team = offense) %>%
    left_join(team_id_short, by = "Team") %>%
    filter(!is.na(TeamID)) %>%
    rename(off_epa_else = off_epa) %>%
    select(Team, off_epa_else)
  
  off_epa_mean_else = mean(off_epa_wk_else$off_epa_else)
  
  weekly_adjustments_off = off_epa_wk_else %>% 
    mutate(off_epa_adj_fact = off_epa_mean_else - off_epa_else) %>% 
    mutate(week = i) %>%
    select(week, offense = Team, off_epa_adj_fact)
  
  adjustments_off = rbind(adjustments_off, weekly_adjustments_off)
}

## > FCS def adjustments ---------------

mean_fcs_def_epa = pbp_2019 %>%
  filter(rush == 1 | pass == 1) %>% 
  rename(Team = defense) %>%
  left_join(team_id_short, by = "Team") %>%
  filter(is.na(TeamID)) %>%
  summarize(def_epa = mean(EPA)) 

mean_fbs_def_epa = pbp_2019 %>%
  filter(rush == 1 | pass == 1) %>% 
  rename(Team = defense) %>%
  left_join(team_id_short, by = "Team") %>%
  filter(!is.na(TeamID)) %>%
  summarize(def_epa = mean(EPA))

def_fcs_adj = as.numeric(mean_fbs_def_epa - mean_fcs_def_epa)

pbp_2019 = pbp_2019 %>%
  left_join(adjustments_def, by = c("defense", "week")) %>%
  mutate(def_epa_adj_fact = replace_na(def_epa_adj_fact, def_fcs_adj)) %>%
  mutate(epa_def_adj = EPA + def_epa_adj_fact)

## > FCS off adjustments ---------------

mean_fcs_off_epa = pbp_2019 %>%
  filter(rush == 1 | pass == 1) %>% 
  rename(Team = offense) %>%
  left_join(team_id_short, by = "Team") %>%
  filter(is.na(TeamID)) %>%
  summarize(off_epa = mean(EPA)) 

mean_fbs_off_epa = pbp_2019 %>%
  filter(rush == 1 | pass == 1) %>% 
  rename(Team = offense) %>%
  left_join(team_id_short, by = "Team") %>%
  filter(!is.na(TeamID)) %>%
  summarize(off_epa = mean(EPA))

off_fcs_adj = as.numeric(mean_fbs_off_epa - mean_fcs_off_epa)

pbp_2019 = pbp_2019 %>%
  left_join(adjustments_off, by = c("offense", "week")) %>%
  mutate(off_epa_adj_fact = replace_na(off_epa_adj_fact, off_fcs_adj)) %>%
  mutate(epa_off_adj = EPA + off_epa_adj_fact)

## Calculate the top offenses by initial defensive adjustments----------------

top_offenses = pbp_2019 %>%
  filter(rush == 1 | pass == 1) %>% 
  left_join(team_id_short, by = c("offense"="Team")) %>%
  filter(!is.na(TeamID)) %>%
  group_by(offense) %>%
  summarize(mean_epa_adj = mean(epa_def_adj),
            mean_epa = mean(EPA)) %>%
  mutate(epa_delta = mean_epa_adj-mean_epa) %>%
  arrange(desc(mean_epa_adj)) %>%
  mutate(Rank = row_number()) %>%
  mutate(TeamRank = paste0(offense, " #", Rank))

## Calculate the top defenses by initial offensive adjustments

top_defenses = pbp_2019 %>%
  filter(rush == 1 | pass == 1) %>% 
  left_join(team_id_short, by = c("defense"="Team")) %>%
  filter(!is.na(TeamID)) %>%
  group_by(defense) %>%
  summarize(mean_epa_adj = mean(epa_off_adj),
            mean_epa = mean(EPA)) %>%
  mutate(epa_delta = mean_epa_adj-mean_epa) %>%
  arrange(mean_epa_adj) %>%
  mutate(Rank = row_number()) %>%
  mutate(TeamRank = paste0(defense, " #", Rank))

## Run adjustment process on adjusted EPA metrics over and over until they stop changing----------------

start_time = Sys.time()   #time the big loop
iterations = 1:100         #takes about 10 minutes to run 100 iterations
weeks = 1:16

## results dataframe captures the adjusted epa for LSU's offense to monitor impact of each adjustment
results = data.frame(j = NA,offense = NA, mean_epa_adj = NA)

## results j = 0 is unadjusted EPA per play
results[1,"j"] = 0
results[1,"offense"] = "LSU"
results[1,"mean_epa_adj"] = off_epa_raw %>% filter(Team == "LSU") %>% select(off_epa_raw) %>% pull()

## results j = 1 is initial opponent adjusted EPA per play
results[2,"j"] = 1
results[2,"offense"] = "LSU"
results[2,"mean_epa_adj"] = top_offenses %>% filter(offense == "LSU") %>% select(mean_epa_adj) %>% pull()

## start the loop
for(j in iterations) {

  adjustments_def = data.frame()
  adjustments_off = data.frame()
    
  for(i in weeks)  {
    
    ## >> Def EPA Adjustments-------------------------------
    
    def_epa_wk_else = pbp_2019 %>%
      filter(week != i) %>%
      filter(rush == 1 | pass == 1) %>% 
      group_by(defense) %>%
      summarize(def_epa = mean(epa_off_adj)) %>%
      arrange(desc(def_epa)) %>%
      rename(Team = defense) %>%
      left_join(team_id_short, by = "Team") %>%
      filter(!is.na(TeamID)) %>%
      rename(def_epa_else = def_epa) %>%
      select(Team, def_epa_else)
    
    def_epa_mean_else = mean(def_epa_wk_else$def_epa_else)
    
    weekly_adjustments_def = def_epa_wk_else %>% 
      mutate(def_epa_adj_fact = def_epa_mean_else - def_epa_else) %>% 
      mutate(week = i) %>%
      select(week, defense = Team, def_epa_adj_fact)
    
    adjustments_def = rbind(adjustments_def, weekly_adjustments_def)
    
    ## >> Off EPA Adjustments-------------------------------
    
    off_epa_wk_else = pbp_2019 %>%
      filter(week != i) %>%
      filter(rush == 1 | pass == 1) %>% 
      group_by(offense) %>%
      summarize(off_epa = mean(epa_def_adj)) %>%
      arrange(desc(off_epa)) %>%
      rename(Team = offense) %>%
      left_join(team_id_short, by = "Team") %>%
      filter(!is.na(TeamID)) %>%
      rename(off_epa_else = off_epa) %>%
      select(Team, off_epa_else)
    
    off_epa_mean_else = mean(off_epa_wk_else$off_epa_else)
    
    weekly_adjustments_off = off_epa_wk_else %>% 
      mutate(off_epa_adj_fact = off_epa_mean_else - off_epa_else) %>% 
      mutate(week = i) %>%
      select(week, offense = Team, off_epa_adj_fact)
    
    adjustments_off = rbind(adjustments_off, weekly_adjustments_off)
    
  }

  ## > FCS Def adjustments ---------------
  
  mean_fcs_def_epa = pbp_2019 %>%
    filter(rush == 1 | pass == 1) %>% 
    rename(Team = defense) %>%
    left_join(team_id_short, by = "Team") %>%
    filter(is.na(TeamID)) %>%
    summarize(def_epa = mean(epa_off_adj)) 
  
  mean_fbs_def_epa = pbp_2019 %>%
    filter(rush == 1 | pass == 1) %>% 
    rename(Team = defense) %>%
    left_join(team_id_short, by = "Team") %>%
    filter(!is.na(TeamID)) %>%
    summarize(def_epa = mean(epa_off_adj))
  
  def_fcs_adj = as.numeric(mean_fbs_def_epa - mean_fcs_def_epa)
  
  pbp_2019 = pbp_2019 %>%
    select(-def_epa_adj_fact, -epa_def_adj) %>%
    left_join(adjustments_def, by = c("defense", "week")) %>%
    mutate(def_epa_adj_fact = replace_na(def_epa_adj_fact, def_fcs_adj)) %>%
    mutate(epa_def_adj = EPA + def_epa_adj_fact)
  
  ## > FCS Off adjustments ---------------
  
  mean_fcs_off_epa = pbp_2019 %>%
    filter(rush == 1 | pass == 1) %>% 
    rename(Team = offense) %>%
    left_join(team_id_short, by = "Team") %>%
    filter(is.na(TeamID)) %>%
    summarize(off_epa = mean(epa_def_adj)) 
  
  mean_fbs_off_epa = pbp_2019 %>%
    filter(rush == 1 | pass == 1) %>% 
    rename(Team = offense) %>%
    left_join(team_id_short, by = "Team") %>%
    filter(!is.na(TeamID)) %>%
    summarize(off_epa = mean(epa_def_adj))
  
  off_fcs_adj = as.numeric(mean_fbs_off_epa - mean_fcs_off_epa)
  
  pbp_2019 = pbp_2019 %>%
    select(-epa_off_adj, -off_epa_adj_fact) %>%
    left_join(adjustments_off, by = c("offense", "week")) %>%
    mutate(off_epa_adj_fact = replace_na(off_epa_adj_fact, off_fcs_adj)) %>%
    mutate(epa_off_adj = EPA + off_epa_adj_fact)
  
  ## monitor loop progress ##
  print(paste0("j = ",j))
  
  ## fill in results df for each loop
  results[j+2,"j"] = j+1
  results[j+2,"offense"] = pbp_2019 %>% filter(rush == 1 | pass == 1) %>% group_by(offense) %>% summarize(mean_epa_adj = mean(epa_def_adj)) %>% 
    arrange(desc(mean_epa_adj)) %>% filter(offense == "LSU") %>% pull(offense)
  results[j+2,"mean_epa_adj"] = pbp_2019 %>% filter(rush == 1 | pass == 1) %>% group_by(offense) %>% summarize(mean_epa_adj = mean(epa_def_adj)) %>% 
    arrange(desc(mean_epa_adj)) %>% slice(1) %>% pull(mean_epa_adj)
  
}

end_time = Sys.time()   
total_time = end_time - start_time
print(total_time)

## graph results to see impact of each iteration on LSU's opponent adjusted EPA
ggplot(results, aes(x=j, y=mean_epa_adj)) + geom_point(size = 4) + theme_bw() +
  xlab("Adjustment Iteration") + ylab("Mean Adjusted EPA") +
  labs(title = "LSU's Offensive Adjusted EPA")

## determine top defenses by adjusted EPA
top_defenses_final = pbp_2019 %>%
  filter(rush == 1 | pass == 1) %>% 
  left_join(team_id_short, by = c("defense"="Team")) %>%
  filter(!is.na(TeamID)) %>%
  group_by(defense) %>%
  summarize(mean_epa_adj = mean(epa_off_adj),
            mean_epa = mean(EPA)) %>%
  mutate(epa_delta = mean_epa_adj-mean_epa) %>%
  arrange(mean_epa_adj) %>%
  mutate(Rank = row_number()) %>%
  mutate(TeamRank = paste0(defense, " #", Rank))

## grap top defenses by adjusted EPA
top_defenses_final %>% 
  rename(delta = epa_delta) %>%
  pivot_longer(cols = contains("epa"), names_to = "epa_type", values_to = "EPA") %>% 
  filter(Rank <= 65) %>%
  ggplot(aes(x=reorder(TeamRank, desc(Rank)), y = EPA)) + geom_point(aes(shape = epa_type), size = 3) + coord_flip() +
  geom_hline(yintercept = 0, linetype = 2) +
  scale_shape_manual(name = "EPA Type", labels = c("Raw EPA", "Adj EPA"), values = c(1,16)) + theme_bw() + 
  xlab("") + ylab("Mean EPA") +
  labs(title = "2019 Top Defenses by Adjusted EPA",
       caption = "Chart by @cfbNate
       Data from @CFB_Data via @cfbscrapR") +
  theme(panel.grid.minor = element_blank())