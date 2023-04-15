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

player_mins <- defstats %>%
  select(PLAYER_ID, numSeason, Season, MIN)

player_options <- defstats %>%
  filter(MIN >= 500) %>%
  distinct(PLAYER_NAME) %>%
  pull(PLAYER_NAME)

season_options <- defstats %>%
  distinct(Season) %>%
  pull(Season)




