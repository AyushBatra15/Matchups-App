library(shiny)
library(shinythemes)
library(tidyverse)
library(gt)
library(webshot)
library(ggimage)
library(scales)

matchups <- read_csv(url("https://raw.githubusercontent.com/AyushBatra15/Matchups-App/main/data/all_matchups.csv"))
defstats <- read_csv(url("https://raw.githubusercontent.com/AyushBatra15/Matchups-App/main/data/defstats.csv"))
headshots <- read_csv(url("https://raw.githubusercontent.com/AyushBatra15/Matchups-App/main/data/player_headshots.csv"))
team_info <- read_csv(url("https://raw.githubusercontent.com/AyushBatra15/Matchups-App/main/data/team_info.csv"))

defstats <- defstats %>%
  distinct(PLAYER_ID, Season, .keep_all = T)

player_mins <- defstats %>%
  select(PLAYER_ID, Season, MIN)

player_tms <- defstats %>%
  select(PLAYER_ID, Season, TEAM)

player_options <- defstats %>%
  filter(MIN >= 500) %>%
  distinct(PLAYER_NAME) %>%
  pull(PLAYER_NAME)

season_options <- defstats %>%
  distinct(Season) %>%
  pull(Season)


matchups_leaderboard <- matchups %>%
  filter(OFF_PLAYER_SZN_MIN >= 300) %>%
  group_by(DEF_PLAYER_ID, Season) %>%
  summarize(PLAYER_NAME = first(DEF_PLAYER_NAME),
            MATCHUP_DIFF = weighted.mean(OFF_PLAYER_SZN_PTS, w = PARTIAL_POSS)) %>%
  ungroup() %>%
  rename(PLAYER_ID = DEF_PLAYER_ID) %>%
  left_join(player_mins, by = c("PLAYER_ID","Season")) %>%
  left_join(player_tms, by = c("PLAYER_ID","Season")) %>%
  filter(!is.na(MIN))

matchups_leaderboard <- matchups_leaderboard %>%
  filter(MIN >= 1000)

matchups_leaderboard <- matchups_leaderboard %>%
  group_by(Season) %>%
  mutate(Pct = rank(MATCHUP_DIFF) / n(),
         Z = (MATCHUP_DIFF - mean(MATCHUP_DIFF)) / sd(MATCHUP_DIFF)) %>%
  ungroup()

def_leaders <- defstats %>%
  filter(MIN >= 1000) %>%
  group_by(Season) %>%
  mutate(Pct_Blk = rank(BLK) / n(),
         Z_Blk = (BLK - mean(BLK)) / sd(BLK),
         Pct_Rim = rank(-PLUSMINUS) / n(),
         Z_Rim = (mean(PLUSMINUS) - PLUSMINUS) / sd(PLUSMINUS),
         Pct_Defl = rank(DEFLECTIONS) / n(),
         Z_Defl = (DEFLECTIONS - mean(DEFLECTIONS)) / sd(DEFLECTIONS)) %>%
  select(PLAYER_ID, PLAYER_NAME, Season, TEAM, MIN, BLK, Pct_Blk, Z_Blk,
         PLUSMINUS, Pct_Rim, Z_Rim, DEFLECTIONS, Pct_Defl, Z_Defl)

ml <- matchups_leaderboard %>%
  select(PLAYER_ID, Season, MATCHUP_DIFF, Pct_MD = Pct, Z_MD = Z)

def_leaders <-  def_leaders %>%
  inner_join(ml, by = c("PLAYER_ID","Season"))

def_leaders <- def_leaders %>%
  mutate(Sum_Z = Z_Blk + Z_Rim + Z_Defl + Z_MD) %>%
  group_by(Season) %>%
  mutate(Pct = rank(Sum_Z) / n()) %>%
  ungroup()


         




