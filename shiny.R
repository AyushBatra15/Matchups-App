library(shiny)
library(shinythemes)
library(tidyverse)
library(gt)
library(webshot)
library(ggimage)
library(scales)
library(ggbeeswarm)

myTheme <- function() {
  theme_minimal() %+replace%
    theme(plot.background = element_rect(fill = "white", color = NA),
          plot.title = element_text(size = 18, hjust = 0.5, vjust = 2),
          plot.subtitle = element_text(size = 12, hjust = 0.5, vjust = 1),
          plot.caption = element_text(size = 9, hjust = 1, vjust = -5),
          axis.title.x = element_text(size = 15, vjust = -2, margin = margin(0,0,2,0)),
          axis.title.y = element_text(size = 15,  angle = 90, vjust = 3,
                                      margin = margin(0,0,0,2)),
          axis.text = element_text(size = 13),
          strip.text = element_text(size = 15, margin = margin(3,0,3,0)),
          legend.title = element_text(size = 13, hjust = 0.5),
          legend.text = element_text(size = 12),
          legend.box.background = element_blank())
}

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
  filter(MIN >= 800) %>%
  distinct(PLAYER_NAME) %>%
  pull(PLAYER_NAME) %>%
  sort()

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
                        
                        selectInput(
                          inputId = "sort_element",
                          label = "Sort By:",
                          choices = c("Composite","Blocks","Rim Defense",
                                      "Deflections","Matchup Difficulty"),
                          selected = "Composite"
                        ),
                        
                    )
               ),
               
               mainPanel(
                 tableOutput("leaderboard_table")
               )
          ),
      
      tabPanel("By Player",
               fluidRow(
                 column(4, align = "center",
                        
                        tags$h3("Parameters"),
                        
                        selectInput(
                          inputId = "player_name",
                          label = "Player: ",
                          choices = player_options,
                          selected = "LeBron James"
                        ),
                        
                        selectInput(
                          inputId = "player_season",
                          label = "Season for Most Common Matchups: ",
                          choices = 2019:2023,
                          selected = 2023,
                        )
                 )
               ),
               
               mainPanel(
                 tableOutput("player_table"),
                 br(),
                 plotOutput("skills_plot", inline = TRUE),
                 br(),
                 tableOutput("common_matchups_tbl")
               )
      ),
      
      tabPanel("Player Comparison",
               fluidRow(
                 column(4, align = "center",
                        
                        tags$h3("Parameters"),
                        
                        selectInput(
                          inputId = "player1",
                          label = "Player 1: ",
                          choices = player_options,
                          selected = "LeBron James"
                        ),
                        
                        selectInput(
                          inputId = "player2",
                          label = "Player 2: ",
                          choices = player_options,
                          selected = "Kevin Durant"
                        ),
                        
                        selectInput(
                          inputId = "comp_season",
                          label = "Season: ",
                          choices = 2019:2023,
                          selected = 2023
                        )
                   
                 )
               ),
               
               mainPanel(
                 tableOutput("comp_table"),
                 br(),
                 plotOutput("skills_comp", inline = TRUE),
                 br(),
                 plotOutput("matchups_comp", inline = TRUE)
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
    
    def_leaders <- def_leaders %>%
      mutate(PLAYER_ID = as.numeric(PLAYER_ID)) %>%
      mutate(Pct_Blk = round(100*Pct_Blk),
             Pct_Rim = round(100*Pct_Rim),
             Pct_Defl = round(100*Pct_Defl),
             Pct_MD = round(100*Pct_MD),
             Pct = round(100*Pct))
    
    if (input$sort_element == "Composite") {
      def_leaders <- def_leaders %>%
        arrange(-Sum_Z)
    }
    
    else if (input$sort_element == "Blocks") {
      def_leaders <- def_leaders %>%
        arrange(-BLK, -Sum_Z)
    }
    
    else if (input$sort_element == "Rim Defense") {
      def_leaders <- def_leaders %>%
        arrange(PLUSMINUS, -Sum_Z)
    }
    
    else if (input$sort_element == "Deflections") {
      def_leaders <- def_leaders %>%
        arrange(-DEFLECTIONS, -Sum_Z)
    }
    
    else if (input$sort_element == "Matchup Difficulty") {
      def_leaders <- def_leaders %>%
        arrange(-MATCHUP_DIFF, -Sum_Z)
    }
    
    def_leaders %>%
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
  
  output$player_table <- render_gt( {
    
    player_board <- matchups %>%
      filter(OFF_PLAYER_SZN_MIN >= 300) %>%
      group_by(DEF_PLAYER_ID, numSeason) %>%
      summarize(PLAYER_NAME = first(DEF_PLAYER_NAME),
                MATCHUP_DIFF = weighted.mean(OFF_PLAYER_SZN_PTS, w = PARTIAL_POSS)) %>%
      ungroup() %>%
      rename(PLAYER_ID = DEF_PLAYER_ID) %>%
      left_join(player_mins, by = c("PLAYER_ID","numSeason")) %>%
      left_join(player_tms, by = c("PLAYER_ID","numSeason")) %>%
      filter(!is.na(MIN))
    
    player_board <- player_board %>%
      filter(MIN >= 800)
    
    player_board <- player_board %>%
      group_by(numSeason) %>%
      mutate(Pct = rank(MATCHUP_DIFF) / n(),
             Z = (MATCHUP_DIFF - mean(MATCHUP_DIFF)) / sd(MATCHUP_DIFF)) %>%
      ungroup()
    
    player_def <- defstats %>%
      filter(MIN >= 800) %>%
      group_by(numSeason) %>%
      mutate(Pct_Blk = rank(BLK) / n(),
             Z_Blk = (BLK - mean(BLK)) / sd(BLK),
             Pct_Rim = rank(-PLUSMINUS) / n(),
             Z_Rim = (mean(PLUSMINUS) - PLUSMINUS) / sd(PLUSMINUS),
             Pct_Defl = rank(DEFLECTIONS) / n(),
             Z_Defl = (DEFLECTIONS - mean(DEFLECTIONS)) / sd(DEFLECTIONS)) %>%
      select(PLAYER_ID, PLAYER_NAME, numSeason, TEAM, MIN, BLK, Pct_Blk, Z_Blk,
             PLUSMINUS, Pct_Rim, Z_Rim, DEFLECTIONS, Pct_Defl, Z_Defl)
    
    pb <- player_board %>%
      select(PLAYER_ID, numSeason, MATCHUP_DIFF, Pct_MD = Pct, Z_MD = Z)
    
    player_def <- player_def %>%
      inner_join(pb, by = c("PLAYER_ID","numSeason"))
    
    player_def <- player_def %>%
      mutate(Sum_Z = Z_Blk + Z_Rim + Z_Defl + Z_MD) %>%
      group_by(numSeason) %>%
      mutate(Pct = rank(Sum_Z) / n()) %>%
      ungroup() %>%
      filter(PLAYER_NAME == input$player_name)
    
    player_def %>%
      mutate(PLAYER_ID = as.numeric(PLAYER_ID)) %>%
      mutate(Pct_Blk = round(100*Pct_Blk),
             Pct_Rim = round(100*Pct_Rim),
             Pct_Defl = round(100*Pct_Defl),
             Pct_MD = round(100*Pct_MD),
             Pct = round(100*Pct)) %>%
      arrange(numSeason) %>%
      left_join(headshots, by = c("PLAYER_ID" = "idPlayer")) %>%
      left_join(team_info, by = c("TEAM" = "slugTeam")) %>%
      select(urlPlayerHeadshot, PLAYER_NAME, numSeason, logo, MIN, BLK, Pct_Blk, 
             PLUSMINUS, Pct_Rim, DEFLECTIONS, Pct_Defl, MATCHUP_DIFF, Pct_MD, 
             Sum_Z, Pct) %>%
      gt() %>%
      tab_header(title = paste(input$player_name, "Defensive Stats"),
                 subtitle = "Matchup Difficulty = average season PTS/100 of opponent matchup | Only Seasons with at least 800 Minutes Displayed") %>%
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
  
  output$skills_plot <- renderPlot( {
    player_board <- matchups %>%
      filter(OFF_PLAYER_SZN_MIN >= 300) %>%
      group_by(DEF_PLAYER_ID, numSeason) %>%
      summarize(PLAYER_NAME = first(DEF_PLAYER_NAME),
                MATCHUP_DIFF = weighted.mean(OFF_PLAYER_SZN_PTS, w = PARTIAL_POSS)) %>%
      ungroup() %>%
      rename(PLAYER_ID = DEF_PLAYER_ID) %>%
      left_join(player_mins, by = c("PLAYER_ID","numSeason")) %>%
      left_join(player_tms, by = c("PLAYER_ID","numSeason")) %>%
      filter(!is.na(MIN))
    
    player_board <- player_board %>%
      filter(MIN >= 800)
    
    player_board <- player_board %>%
      group_by(numSeason) %>%
      mutate(Pct = rank(MATCHUP_DIFF) / n(),
             Z = (MATCHUP_DIFF - mean(MATCHUP_DIFF)) / sd(MATCHUP_DIFF)) %>%
      ungroup()
    
    player_def <- defstats %>%
      filter(MIN >= 800) %>%
      group_by(numSeason) %>%
      mutate(Pct_Blk = rank(BLK) / n(),
             Z_Blk = (BLK - mean(BLK)) / sd(BLK),
             Pct_Rim = rank(-PLUSMINUS) / n(),
             Z_Rim = (mean(PLUSMINUS) - PLUSMINUS) / sd(PLUSMINUS),
             Pct_Defl = rank(DEFLECTIONS) / n(),
             Z_Defl = (DEFLECTIONS - mean(DEFLECTIONS)) / sd(DEFLECTIONS)) %>%
      select(PLAYER_ID, PLAYER_NAME, numSeason, TEAM, MIN, BLK, Pct_Blk, Z_Blk,
             PLUSMINUS, Pct_Rim, Z_Rim, DEFLECTIONS, Pct_Defl, Z_Defl)
    
    pb <- player_board %>%
      select(PLAYER_ID, numSeason, MATCHUP_DIFF, Pct_MD = Pct, Z_MD = Z)
    
    player_def <- player_def %>%
      inner_join(pb, by = c("PLAYER_ID","numSeason"))
    
    player_def <- player_def %>%
      mutate(Sum_Z = Z_Blk + Z_Rim + Z_Defl + Z_MD) %>%
      group_by(numSeason) %>%
      mutate(Pct = rank(Sum_Z) / n()) %>%
      ungroup() %>%
      filter(PLAYER_NAME == input$player_name)
    
    min_yr <- min(player_def$numSeason)
    max_yr <- max(player_def$numSeason)
    
    player_def <- player_def %>%
      select(PLAYER_NAME, TEAM, numSeason, Pct_Blk, Pct_Rim, Pct_Defl, Pct_MD, Pct_Comp = Pct) %>%
      pivot_longer(cols = c(Pct_Blk : Pct_Comp),
                   names_prefix = "Pct_",
                   names_to = "Category",
                   values_to = "Pct") %>%
      mutate(Category = case_when(
        Category == "Comp" ~ "Composite",
        Category == "Blk" ~ "Blocks",
        Category == "Rim" ~ "Rim Defense",
        Category == "Defl" ~ "Deflections",
        Category == "MD" ~ "Matchup Diff"
      )) %>%
      mutate(Category = factor(Category,
                               levels = c("Composite","Blocks","Rim Defense",
                                          "Deflections","Matchup Diff"))) %>%
      mutate(Pct = 100*Pct)
    
    player_def %>%
      ggplot(aes(x = numSeason, y = Pct, color = Category)) +
      geom_point(aes(size = Category), show.legend = F) +
      geom_line(show.legend = F) +
      geom_text(data = player_def %>% filter(numSeason == max(numSeason)),
                aes(x = numSeason + 0.1, y = Pct, label = Category),
                hjust = 'left', show.legend = F, size = 5) +
      scale_x_continuous(breaks = seq(min_yr, max_yr, 1)) +
      scale_y_continuous(limits = c(0, 100),
                         breaks = seq(0,100,25)) +
      scale_size_manual(values = c(6,4,4,4,4),
                        guide = "none") +
      # scale_color_discrete(labels = c("Composite",
      #                                 "Blocks",
      #                                 "Rim Defense",
      #                                 "Deflections",
      #                                 "Matchup Diff")) +
      labs(x = "Year",
           y = "Percentile",
           color = "Category",
           title = paste(input$player_name,"Defensive Progression")) +
      myTheme() +
      coord_cartesian(clip = 'off') +
      theme(plot.margin = margin(0.1,1,0.25,0.1, unit = "in"))
  }, height = 400, width = 500)
  
  output$common_matchups_tbl <- render_gt( {
    season_pts <- matchups %>%
      distinct(OFF_PLAYER_ID, numSeason, OFF_PLAYER_SZN_PTS)
    
    common <- matchups %>%
      filter(DEF_PLAYER_NAME == input$player_name,
             numSeason == input$player_season) %>%
      group_by(OFF_PLAYER_ID, DEF_PLAYER_ID, numSeason) %>%
      summarize(OFF_PLAYER_NAME = first(OFF_PLAYER_NAME),
                DEF_PLAYER_NAME = first(DEF_PLAYER_NAME),
                PARTIAL_POSS = sum(PARTIAL_POSS)) %>%
      ungroup() %>%
      arrange(-PARTIAL_POSS) %>%
      head(10) %>%
      add_column(Rk = c(1:10)) %>%
      left_join(season_pts, by = c("OFF_PLAYER_ID", "numSeason"))
    
    common %>%
      left_join(headshots, by = c("OFF_PLAYER_ID" = "idPlayer")) %>%
      left_join(headshots, by = c("DEF_PLAYER_ID" = "idPlayer"),
                suffix = c(".off",".def")) %>%
      select(Rk, urlPlayerHeadshot.off, OFF_PLAYER_NAME, OFF_PLAYER_SZN_PTS,
             urlPlayerHeadshot.def, DEF_PLAYER_NAME, numSeason, PARTIAL_POSS) %>%
      gt() %>%
      text_transform(
        locations = cells_body(c(urlPlayerHeadshot.off, urlPlayerHeadshot.def)),
        fn = function(x){
          web_image(
            url = x,
            height = px(35)
          )
        }
      ) %>%
      tab_header(title = paste(input$player_name, "Most Common Defensive Matchups: ", input$player_season)) %>%
      cols_label(urlPlayerHeadshot.off = "",
                 OFF_PLAYER_NAME = "Offense",
                 OFF_PLAYER_SZN_PTS = "Season PTS/100",
                 urlPlayerHeadshot.def = "",
                 DEF_PLAYER_NAME = "Defense",
                 numSeason = "Season",
                 PARTIAL_POSS = "Possessions") %>%
      opt_row_striping() %>%
      tab_source_note("By Ayush Batra | Data from NBA.com")
  }, width = 700)
  
  output$comp_table <- render_gt( {
    comp_m <- matchups %>%
      filter(OFF_PLAYER_SZN_MIN >= 300) %>%
      group_by(DEF_PLAYER_ID, numSeason) %>%
      summarize(PLAYER_NAME = first(DEF_PLAYER_NAME),
                MATCHUP_DIFF = weighted.mean(OFF_PLAYER_SZN_PTS, w = PARTIAL_POSS)) %>%
      ungroup() %>%
      rename(PLAYER_ID = DEF_PLAYER_ID) %>%
      left_join(player_mins, by = c("PLAYER_ID","numSeason")) %>%
      left_join(player_tms, by = c("PLAYER_ID","numSeason")) %>%
      filter(!is.na(MIN))
    
    comp_m <- comp_m %>%
      filter(MIN >= 800)
    
    comp_board <- comp_m %>%
      group_by(numSeason) %>%
      mutate(Pct = rank(MATCHUP_DIFF) / n(),
             Z = (MATCHUP_DIFF - mean(MATCHUP_DIFF)) / sd(MATCHUP_DIFF)) %>%
      ungroup()
    
    comp_def <- defstats %>%
      filter(MIN >= 800) %>%
      group_by(numSeason) %>%
      mutate(Pct_Blk = rank(BLK) / n(),
             Z_Blk = (BLK - mean(BLK)) / sd(BLK),
             Pct_Rim = rank(-PLUSMINUS) / n(),
             Z_Rim = (mean(PLUSMINUS) - PLUSMINUS) / sd(PLUSMINUS),
             Pct_Defl = rank(DEFLECTIONS) / n(),
             Z_Defl = (DEFLECTIONS - mean(DEFLECTIONS)) / sd(DEFLECTIONS)) %>%
      select(PLAYER_ID, PLAYER_NAME, numSeason, TEAM, MIN, BLK, Pct_Blk, Z_Blk,
             PLUSMINUS, Pct_Rim, Z_Rim, DEFLECTIONS, Pct_Defl, Z_Defl)
    
    comp_board <- comp_board %>%
      select(PLAYER_ID, numSeason, MATCHUP_DIFF, Pct_MD = Pct, Z_MD = Z)
    
    comp_def <- comp_def %>%
      inner_join(comp_board, by = c("PLAYER_ID","numSeason"))
    
    comp_def <- comp_def %>%
      mutate(Sum_Z = Z_Blk + Z_Rim + Z_Defl + Z_MD) %>%
      group_by(numSeason) %>%
      mutate(Pct = rank(Sum_Z) / n()) %>%
      ungroup() %>%
      filter(PLAYER_NAME %in% c(input$player1, input$player2),
             numSeason == input$comp_season)
    
    comp_def %>%
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
      tab_header(title = paste(input$player1, "vs", input$player2, input$comp_season,": Defense"),
                 subtitle = "Matchup Difficulty = average season PTS/100 of opponent matchup | Only Seasons with at least 800 Minutes Displayed") %>%
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
  
  output$skills_comp <- renderPlot({
    matchup_diff <- matchups %>%
      filter(OFF_PLAYER_SZN_MIN >= 300) %>%
      group_by(DEF_PLAYER_ID, numSeason) %>%
      summarize(PLAYER_NAME = first(DEF_PLAYER_NAME),
                MATCHUP_DIFF = weighted.mean(OFF_PLAYER_SZN_PTS, w = PARTIAL_POSS)) %>%
      ungroup() %>%
      rename(PLAYER_ID = DEF_PLAYER_ID) %>%
      left_join(player_mins, by = c("PLAYER_ID","numSeason")) %>%
      left_join(player_tms, by = c("PLAYER_ID","numSeason")) %>%
      filter(!is.na(MIN)) %>%
      filter(MIN >= 800,
             numSeason == input$comp_season)
    
    other_def <- defstats %>%
      filter(MIN >= 800,
             numSeason == input$comp_season)
    
    avg_md <- matchup_diff %>%
      summarize(avg = mean(MATCHUP_DIFF)) %>%
      mutate(stat = "MATCHUP_DIFF") %>%
      select(stat, avg)
    
    avg_def <- defstats %>%
      summarize(avg_BLK = mean(BLK),
                avg_DEFLECTIONS = mean(DEFLECTIONS),
                avg_PLUSMINUS = mean(PLUSMINUS)) %>%
      pivot_longer(cols = c(avg_BLK : avg_PLUSMINUS),
                   names_prefix = "avg_",
                   names_to = "stat",
                   values_to = "avg")
    
    avg_stats <- rbind(avg_md, avg_def)
    
    avg_stats <- avg_stats %>%
      rename(value = avg) %>%
      mutate(PLAYER_NAME = "avg") %>%
      select(PLAYER_NAME, stat, value)
    
    p_md <- matchup_diff %>%
      filter(PLAYER_NAME %in% c(input$player1, input$player2)) %>%
      mutate(stat = "MATCHUP_DIFF") %>%
      select(PLAYER_NAME, stat, value = MATCHUP_DIFF)
    
    p_def <- other_def %>%
      filter(PLAYER_NAME %in% c(input$player1, input$player2)) %>%
      select(PLAYER_NAME, BLK, DEFLECTIONS, PLUSMINUS) %>%
      pivot_longer(!PLAYER_NAME,
                   names_to = "stat",
                   values_to = "value")
    
    p_stats <- rbind(p_md, p_def)
    
    all_stats <- rbind(avg_stats, p_stats)
    
    all_stats %>%
      mutate(PLAYER_NAME = ifelse(PLAYER_NAME == "avg",
                                  "NBA Average",
                                  PLAYER_NAME)) %>%
      mutate(PLAYER_NAME = factor(PLAYER_NAME,
                                  levels = c("NBA Average", 
                                             input$player1, 
                                             input$player2))) %>%
      mutate(stat = case_when(
        stat == "MATCHUP_DIFF" ~ "Matchup Difficulty",
        stat == "BLK" ~ "Blocks",
        stat == "PLUSMINUS" ~ "Rim DFG +/-",
        stat == "DEFLECTIONS" ~ "Deflections"
      )) %>%
      mutate(stat = factor(stat,
                           levels = c("Blocks","Rim DFG +/-",
                                      "Deflections","Matchup Difficulty"))) %>%
      ggplot(aes(x = PLAYER_NAME, y = value, fill = PLAYER_NAME)) +
      geom_col(width = 0.8, color = 'black', show.legend = F) +
      scale_fill_manual(values = c("#808080","#F8766D","#00BFC4")) +
      facet_wrap(~stat, scales = "free", ncol = 2) +
      labs(x = "",
           y = "",
           title = paste(input$player1, "vs", input$player2, input$comp_season, "Defensive Skills")) +
      myTheme()
  }, height = 700, width = 700)
  
  output$matchups_comp <- renderPlot({
    player_matchups <- matchups %>%
      filter(OFF_PLAYER_SZN_MIN >= 300,
             numSeason == input$comp_season)
    
    md_avg <- weighted.mean(player_matchups$OFF_PLAYER_SZN_PTS,
                            w = player_matchups$PARTIAL_POSS)
    
    player_matchups <- player_matchups %>%
      filter(DEF_PLAYER_NAME %in% c(input$player1, input$player2))
    
    player_matchups %>%
      filter(PARTIAL_POSS >= 10) %>%
      left_join(player_tms, by = c("DEF_PLAYER_ID" = "PLAYER_ID","numSeason")) %>%
      left_join(team_info, by = c("TEAM" = "slugTeam")) %>%
      ggplot(aes(x = DEF_PLAYER_NAME,
                 y = OFF_PLAYER_SZN_PTS)) +
      geom_hline(yintercept = md_avg, linetype = 'dashed') +
      geom_quasirandom(aes(fill = color1, size = PARTIAL_POSS),
                       shape = 21, color = 'black') +
      scale_fill_identity() +
      labs(x = "Player",
           y = "Opponent Season PTS/100",
           size = "Poss",
           title = paste(input$player1,"vs",input$player2,": Matchup Difficulty")) +
      myTheme()
  }, height = 600, width = 600)
  
}

shinyApp(ui = ui, server = server)
    
         




