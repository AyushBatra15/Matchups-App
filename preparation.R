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

SEASON = "2021-22"


# GET MATCHUP DATA FOR SPECIFIC YEAR ----

offense <- nba_leaguedashplayerstats(season = SEASON,
                                     season_type = "Regular Season",
                                     per_mode = "Per100Possessions")
offense <- offense[["LeagueDashPlayerStats"]]

pts <- offense %>%
  select(PLAYER_ID, PTS) %>%
  mutate(PTS = as.numeric(PTS))

mins <- nba_leaguedashplayerstats(season = SEASON,
                                  season_type = "Regular Season",
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
                                          season_type = "Regular Season",
                                          def_team_id = as.character(t))
  tm_matchups <- tm_matchups[["SeasonMatchups"]]
  
  thisTeam <- df_dict_nba_teams %>%
    filter(idTeam == t) %>%
    pull(slugTeam)
  
  tm_matchups <- tm_matchups %>%
    mutate(DEF_PLAYER_TM = thisTeam)
  
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
         DEF_PLAYER_TM, PARTIAL_POSS, PLAYER_PTS, MATCHUP_FGA, MATCHUP_FTA,
         MATCHUP_TOV, OFF_PLAYER_SZN_PTS = PTS, OFF_PLAYER_SZN_MIN = MIN)

construct_f_name <- function(season) {
  suffix <- "_rs"
  s <- as.character(as.numeric(str_sub(season, 1, 4)) + 1)
  f <- paste("data/matchups", s, suffix, ".csv",sep = "")
  return( f )
}

f <- construct_f_name(SEASON)

# write_csv(matchups, file = f)



# COMBINE MATCHUP DATA ------

# Note: Basically this section just combines the year-by-year matchups 
# (generated in above section) into one big table, but I have deleted 
# the year-by-year matchups to save space

# all_matchups <- tibble()
# for (i in c(19:23)) {
#   fname = paste("data/matchups20",i,"_rs.csv", sep = "")
#   df <- read_csv(file = fname)
#   all_matchups <- rbind(all_matchups, df)
# }

# write_csv(all_matchups,
#           file = "data/all_matchups.csv")


# OTHER DEFENSIVE STATS ------

all_blocks <- tibble()
all_rimdef <- tibble()
all_defl <- tibble()
for (i in c(2019:2023)) {
  sname <- paste(i-1,"-",as.character(str_sub(i,3,4)), sep = "")
  b <- nba_leaguedashplayerstats(season = sname,
                                 season_type = "Regular Season",
                                 per_mode = "Per100Possessions")
  b <- b[["LeagueDashPlayerStats"]]
  
  b <- b %>%
    mutate(Season = sname,
           numSeason = i)
  
  rd <- nba_leaguedashptdefend(season = sname,
                               season_type = "Regular Season",
                               defense_category = "Less+Than+6Ft")
  rd <- rd[["LeagueDashPTDefend"]]
  
  rd <- rd %>%
    mutate(Season = sname,
           numSeason = i)
  
  hustle <- nba_leaguehustlestatsplayer(season = sname,
                                        season_type = "Regular Season")
  hustle <- hustle[["HustleStatsPlayer"]]
  
  hustle <- hustle %>%
    mutate(Season = sname,
           numSeason = i)
  
  p <- nba_leaguedashplayerstats(season = sname,
                                 season_type = "Regular Season",
                                 measure_type = "Advanced")
  p <- p[["LeagueDashPlayerStats"]]
  
  p <- p %>%
    select(PLAYER_ID, POSS) %>%
    mutate(POSS = as.numeric(POSS))
  
  hustle <- hustle %>%
    left_join(p, by = "PLAYER_ID")
  
  all_blocks <- rbind(all_blocks, b)
  all_rimdef <- rbind(all_rimdef, rd)
  all_defl <- rbind(all_defl, hustle)
  
  print(paste("Loaded stats for",i))
}

blocks <- all_blocks %>%
  select(PLAYER_ID, PLAYER_NAME, TEAM = TEAM_ABBREVIATION, 
         Season, numSeason, AGE, BLK) %>%
  mutate(BLK = as.numeric(BLK),
         AGE = as.numeric(AGE))

rimdef <- all_rimdef %>%
  select(PLAYER_ID = CLOSE_DEF_PERSON_ID, Season, numSeason, 
         LT_06_PCT, PLUSMINUS) %>%
  mutate(across(c(LT_06_PCT : PLUSMINUS), as.numeric))

defl <- all_defl %>%
  mutate(across(c(DEFLECTIONS : CHARGES_DRAWN), as.numeric)) %>%
  mutate(DEFLECTIONS = 100 * (DEFLECTIONS + CHARGES_DRAWN) / POSS,
         DEFLECTIONS = round(DEFLECTIONS, 1)) %>%
  select(PLAYER_ID, Season, numSeason, MIN, DEFLECTIONS) %>%
  mutate(MIN = as.numeric(MIN))

defstats <- blocks %>%
  inner_join(defl, by = c("PLAYER_ID","Season","numSeason")) %>%
  inner_join(rimdef, by = c("PLAYER_ID","Season","numSeason")) %>%
  relocate(MIN, .after = AGE)

write_csv(defstats, 
          file = "data/defstats.csv")


