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
  distinct(PLAYER_ID, numSeason, .keep_all = T)

player_mins <- defstats %>%
  select(PLAYER_ID, numSeason, MIN)

player_tms <- defstats %>%
  select(PLAYER_ID, numSeason, TEAM)

player_options <- defstats %>%
  filter(MIN >= 500) %>%
  distinct(PLAYER_NAME) %>%
  pull(PLAYER_NAME)

team_options <- defstats %>%
  distinct(TEAM) %>%
  pull(TEAM) %>%
  sort()

team_options <- c("All Teams", team_options)

matchups_leaderboard <- matchups %>%
  filter(OFF_PLAYER_SZN_MIN >= 300) %>%
  group_by(DEF_PLAYER_ID, numSeason) %>%
  summarize(PLAYER_NAME = first(DEF_PLAYER_NAME),
            MATCHUP_DIFF = weighted.mean(OFF_PLAYER_SZN_PTS, w = PARTIAL_POSS)) %>%
  ungroup() %>%
  rename(PLAYER_ID = DEF_PLAYER_ID) %>%
  left_join(player_mins, by = c("PLAYER_ID","numSeason")) %>%
  left_join(player_tms, by = c("PLAYER_ID","numSeason")) %>%
  filter(!is.na(MIN))

matchups_leaderboard <- matchups_leaderboard %>%
  filter(MIN >= 500)

matchups_leaderboard <- matchups_leaderboard %>%
  group_by(numSeason) %>%
  mutate(Pct = rank(MATCHUP_DIFF) / n(),
         Z = (MATCHUP_DIFF - mean(MATCHUP_DIFF)) / sd(MATCHUP_DIFF)) %>%
  ungroup()

def_leaders <- defstats %>%
  filter(MIN >= 500) %>%
  group_by(numSeason) %>%
  mutate(Pct_Blk = rank(BLK) / n(),
         Z_Blk = (BLK - mean(BLK)) / sd(BLK),
         Pct_Rim = rank(-PLUSMINUS) / n(),
         Z_Rim = (mean(PLUSMINUS) - PLUSMINUS) / sd(PLUSMINUS),
         Pct_Defl = rank(DEFLECTIONS) / n(),
         Z_Defl = (DEFLECTIONS - mean(DEFLECTIONS)) / sd(DEFLECTIONS)) %>%
  select(PLAYER_ID, PLAYER_NAME, numSeason, TEAM, MIN, BLK, Pct_Blk, Z_Blk,
         PLUSMINUS, Pct_Rim, Z_Rim, DEFLECTIONS, Pct_Defl, Z_Defl)

ml <- matchups_leaderboard %>%
  select(PLAYER_ID, numSeason, MATCHUP_DIFF, Pct_MD = Pct, Z_MD = Z)

def_leaders <-  def_leaders %>%
  inner_join(ml, by = c("PLAYER_ID","numSeason"))

def_leaders <- def_leaders %>%
  mutate(Sum_Z = Z_Blk + Z_Rim + Z_Defl + Z_MD) %>%
  group_by(numSeason) %>%
  mutate(Pct = rank(Sum_Z) / n()) %>%
  ungroup()

color_scale <- col_numeric(c("#E09D9D","white","#87B87E"), domain = c(0, 100), alpha = 0.75)


ui <- fluidPage(
  
  titlePanel("NBA Defensive Stats"),
  
  mainPanel(
    navbarPage("Ayush Batra",
      tabPanel("Leaderboard",
               fluidRow(
                 column(4, align = "center",
                        
                        tags$h3("Parameters"),
                        
                        selectInput(
                          inputId = "season",
                          label = "Season:",
                          choices = 2019:2023,
                          selected = 2023
                        ),
                        
                        selectInput(
                          inputId = "team",
                          label = "Team:",
                          choices = team_options,
                          selected = "All Teams"
                        ),
                        
                        sliderInput(
                          inputId = "minimum",
                          label = "Minutes Minimum:",
                          min = 0, max = 3000,
                          value = 1000
                        ),
                        
                    )
               ),
               
               mainPanel(
                 tableOutput("leaderboard_table")
               )
               
               
          )
       )
    )
)


server <- function(input, output) {
  
  output$leaderboard_table <- render_gt( {
    
    matchups_leaderboard <- matchups %>%
      filter(OFF_PLAYER_SZN_MIN >= 300) %>%
      group_by(DEF_PLAYER_ID, numSeason) %>%
      summarize(PLAYER_NAME = first(DEF_PLAYER_NAME),
                MATCHUP_DIFF = weighted.mean(OFF_PLAYER_SZN_PTS, w = PARTIAL_POSS)) %>%
      ungroup() %>%
      rename(PLAYER_ID = DEF_PLAYER_ID) %>%
      left_join(player_mins, by = c("PLAYER_ID","numSeason")) %>%
      left_join(player_tms, by = c("PLAYER_ID","numSeason")) %>%
      filter(!is.na(MIN))
    
    matchups_leaderboard <- matchups_leaderboard %>%
      filter(MIN >= input$minimum)
    
    matchups_leaderboard <- matchups_leaderboard %>%
      filter(numSeason == input$season) %>%
      mutate(Pct = rank(MATCHUP_DIFF) / n(),
             Z = (MATCHUP_DIFF - mean(MATCHUP_DIFF)) / sd(MATCHUP_DIFF)) 
    
    def_leaders <- defstats %>%
      filter(MIN >= input$minimum) %>%
      group_by(numSeason) %>%
      mutate(Pct_Blk = rank(BLK) / n(),
             Z_Blk = (BLK - mean(BLK)) / sd(BLK),
             Pct_Rim = rank(-PLUSMINUS) / n(),
             Z_Rim = (mean(PLUSMINUS) - PLUSMINUS) / sd(PLUSMINUS),
             Pct_Defl = rank(DEFLECTIONS) / n(),
             Z_Defl = (DEFLECTIONS - mean(DEFLECTIONS)) / sd(DEFLECTIONS)) %>%
      select(PLAYER_ID, PLAYER_NAME, numSeason, TEAM, MIN, BLK, Pct_Blk, Z_Blk,
             PLUSMINUS, Pct_Rim, Z_Rim, DEFLECTIONS, Pct_Defl, Z_Defl)
    
    ml <- matchups_leaderboard %>%
      select(PLAYER_ID, numSeason, MATCHUP_DIFF, Pct_MD = Pct, Z_MD = Z)
    
    def_leaders <-  def_leaders %>%
      inner_join(ml, by = c("PLAYER_ID","numSeason"))
    
    def_leaders <- def_leaders %>%
      mutate(Sum_Z = Z_Blk + Z_Rim + Z_Defl + Z_MD) %>%
      group_by(numSeason) %>%
      mutate(Pct = rank(Sum_Z) / n()) %>%
      ungroup()
    
    if (input$team != "All Teams") {
      def_leaders <- def_leaders %>%
        filter(TEAM == input$team)
    }
    
    def_leaders %>%
      mutate(PLAYER_ID = as.numeric(PLAYER_ID)) %>%
      mutate(Pct_Blk = round(100*Pct_Blk),
             Pct_Rim = round(100*Pct_Rim),
             Pct_Defl = round(100*Pct_Defl),
             Pct_MD = round(100*Pct_MD),
             Pct = round(100*Pct)) %>%
      arrange(-Sum_Z) %>%
      left_join(headshots, by = c("PLAYER_ID" = "idPlayer")) %>%
      left_join(team_info, by = c("TEAM" = "slugTeam")) %>%
      select(urlPlayerHeadshot, PLAYER_NAME, numSeason, logo, MIN, BLK, Pct_Blk, 
             PLUSMINUS, Pct_Rim, DEFLECTIONS, Pct_Defl, MATCHUP_DIFF, Pct_MD, 
             Sum_Z, Pct) %>%
      gt() %>%
      tab_header(title = "NBA Defensive Composite",
                 subtitle = "Matchup Difficulty = average season PTS/100 of opponent matchup") %>%
      text_transform(
        locations = cells_body(c(urlPlayerHeadshot, logo)),
        fn = function(x){
          web_image(
            url = x,
            height = px(35)
          )
        }
      ) %>%
      cols_label(urlPlayerHeadshot = "",
                 PLAYER_NAME = "Player",
                 numSeason = "Season",
                 logo = "Tm",
                 Pct_Blk = "PCT",
                 PLUSMINUS = "+/-",
                 Pct_Rim = "PCT",
                 DEFLECTIONS = "DEFL",
                 Pct_Defl = "PCT",
                 MATCHUP_DIFF = "MATCHUP DIFF",
                 Pct_MD = "PCT",
                 Sum_Z = "Score",
                 Pct = "PCT") %>%
      fmt_number(columns = c(MATCHUP_DIFF), decimals = 1) %>%
      fmt_number(columns = c(Sum_Z), decimals = 2) %>%
      fmt_percent(columns = c(PLUSMINUS), decimals = 1) %>%
      tab_spanner(label = "Blocks / 100",
                  columns = c(BLK, Pct_Blk)) %>%
      tab_spanner(label = "Rim DFG% +/-",
                  columns = c(PLUSMINUS, Pct_Rim)) %>%
      tab_spanner(label = "Deflections / 100",
                  columns = c(DEFLECTIONS, Pct_Defl)) %>%
      tab_spanner(label = "Matchup Difficulty",
                  columns = c(MATCHUP_DIFF, Pct_MD)) %>%
      tab_spanner(label = "Composite",
                  columns = c(Sum_Z, Pct)) %>%
      data_color(columns = c(Pct_Blk, Pct_Rim, Pct_Defl, Pct_MD, Pct),
                 colors = color_scale) %>%
      opt_row_striping() %>%
      tab_source_note("By Ayush Batra | Data from NBA.com")
  }, width = 1000)
        
    
    
}

shinyApp(ui = ui, server = server)
    
         




