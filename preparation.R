library(tidyverse)
library(hoopR)
library(nbastatR)
library(gt)
library(webshot)
library(ggimage)
library(scales)
Sys.setenv("VROOM_CONNECTION_SIZE" = 2*131072)

assign_nba_players()
assign_nba_teams()

SEASON = "2022-23"
S_TYPE = "Regular Season"

offense <- nba_leaguedashplayerstats(season = SEASON,
                                     season_type = S_TYPE,
                                     per_mode = "Per100Possessions")
offense <- offense[["LeagueDashPlayerStats"]]

pts <- offense %>%
  select(PLAYER_ID, PTS) %>%
  mutate(PTS = as.numeric(PTS))

mins <- nba_leaguedashplayerstats(season = SEASON,
                                  season_type = S_TYPE,
                                  per_mode = "Totals")
mins <- mins[["LeagueDashPlayerStats"]]

mins <- mins %>%
  select(PLAYER_ID, MIN) %>%
  mutate(MIN = round(as.numeric(MIN)))

team_ids <- df_dict_nba_teams %>%
  filter(idTeam >= 1610612737 & idTeam <= 1610612766) %>%
  pull(idTeam)


all <- tibble()
iter <- 0
for (t in team_ids) {
  tm_matchups <- nba_leagueseasonmatchups(season = SEASON,
                                          season_type = S_TYPE,
                                          def_team_id = as.character(t))
  tm_matchups <- tm_matchups[["SeasonMatchups"]]
  
  all <- rbind(all, tm_matchups)
  
  iter <- iter + 1
  print(paste("Loaded ",iter,"/30 Teams", sep = ""))
}

matchups <- all %>%
  mutate(across(c(PARTIAL_POSS : MATCHUP_TIME_SEC), as.numeric))

matchups <- matchups %>%
  left_join(pts, by = c("OFF_PLAYER_ID" = "PLAYER_ID")) %>%
  left_join(mins, by = c("OFF_PLAYER_ID" = "PLAYER_ID"))

matchups <- matchups %>%
  mutate(Season = SEASON,
         numSeason = as.numeric(str_sub(Season, 1, 4)) + 1) %>%
  select(Season, numSeason, 
         OFF_PLAYER_ID, OFF_PLAYER_NAME, DEF_PLAYER_ID, DEF_PLAYER_NAME,
         PARTIAL_POSS, PLAYER_PTS, MATCHUP_FGA, MATCHUP_FTA, MATCHUP_TOV,
         OFF_PLAYER_SZN_PTS = PTS, OFF_PLAYER_SZN_MIN = MIN)

write_csv(matchups, file = "data/matchups2023_rs.csv")






