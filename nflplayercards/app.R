library(shiny)
library(tidyverse)
library(gt)

# sysfonts::font_add_google("Chivo", "Chivo")
# showtext::showtext_auto()

color_palette <- c("#FF1100", "#FFFFFF", "#00690E")
color_palette_reversed <- c("#00690E", "#FFFFFF", "#FF1100")

passers_csv <- read_csv("passers.csv")
qb_comps_csv <- read_csv("qb_comps.csv")
qb_gamelogs_csv <- read_csv("qb_gamelogs.csv")

passers <- passers_csv %>%
  select(name) %>%
  rename(Player = name) %>%
  distinct()

qb_comps <- qb_comps_csv %>%
  select(full_name) %>%
  rename(Player = full_name) %>%
  distinct()

qb_gamelogs <- qb_gamelogs_csv %>%
  select(name) %>%
  rename(Player = name) %>%
  distinct()

passers_years <- passers_csv %>%
  select(name, season)

qb_comps_years <- qb_comps_csv %>%
  select(full_name, season)

qb_gamelogs_years <- qb_gamelogs_csv %>%
  select(name, season)

# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(
    tags$style(HTML("@import url('https://fonts.googleapis.com/css2?family=Chivo&display=swap');
                    
                    /* Change header text to imported font */
                    .tbl, h5, h2 {
                    font-family: 'Chivo';
                    }"),
                    "@media only screen and (max-width: 600px) {
                    #img {
                      width: 330px !important;
                      height: 220px !important;
                    }
                    }"
      )
  ),
  tags$head(tags$style(".shiny-output-error{visibility: hidden}")),
  
  tags$head(tags$style(".shiny-output-error:after{content: 'Please select quarterback.'; visibility: visible}")),
  
  # Application title
  titlePanel(title = h2("NFL Quarterback Cards", align = "left"), windowTitle = "NFL Quarterback Cards"),
  
  # Sidebar with a slider input for number of bins
  navbarPage("NFL Quarterbacks",
             tabPanel("Cards",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("player", label = "Player:", 
                                      choices = passers[1]),
                          selectInput("year", label = "Year:", 
                                      choices = NULL),
                          width = 12),
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                          imageOutput("img")
                        )
                      )
                      ),
             
             tabPanel("Comparisons",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("percentiles", label = "Stats as percentiles?", 
                                      choices = c("No","Yes")),
                          selectInput("comp_player_1", label = "Player 1:", 
                                      choices = qb_comps[1], selected = NULL),
                          selectInput("year_1", label = "Year 1:",
                                      choices = NULL),
                          selectInput("comp_player_2", label = "Player 2:", 
                                      choices = qb_comps[1]),
                          selectInput("year_2", label = "Year 2:", 
                                      choices = NULL),
                          selectInput("comp_player_3", label = "Player 3:", 
                                      choices = qb_comps[1]),
                          selectInput("year_3", label = "Year 3:", 
                                      choices = NULL),
                          selectInput("comp_player_4", label = "Player 4:", 
                                      choices = qb_comps[1]),
                          selectInput("year_4", label = "Year 4:", 
                                      choices = NULL),
                          width = 3),
                        mainPanel(
                          gt_output("comp"),
                          hr(),
                          print(h5("EPA = nflfastR expected points added per play (efficiency)")),
                          print(h5("CPOE = nflfastR completion % over expected (accuracy)")),
                          print(h5("DVOA = F.O. defense-adjusted value over average (efficiency)")),
                          print(h5("DYAR = F.O. defense-adjusted yards above replacement (gross production)")),
                          print(h5("PFF = individual PFF offense grade")),
                          print(h5("QBR = ESPN total QBR; measure of win probability added per play")),
                          print(h5("Sack% = sack rate (sacks/total dropbacks)")),
                          print(h5("AirYds = Air yards per attempt")),
                          print(h5("CP = nflfastR completion probability based on situation & air yards")),
                          print(h5("PFF Recv = team PFF receiving grade")),
                          print(h5("PFF Pblk = team PFF pass block grade")),
                          print(h5("PA/G = team points allowed per game")),
                          width = 9
                        )
                      )),
             tabPanel("Multi-Season",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("individual_percentiles", label = "Stats as percentiles?", 
                                      choices = c("No","Yes")),
                          selectInput("individual_player", label = "Player:", 
                                      choices = qb_comps[1], selected = NULL),
                          width = 12),
                        mainPanel(
                          gt_output("individual"),
                          hr(),
                          print(h5("EPA = nflfastR expected points added per play (efficiency)")),
                          print(h5("CPOE = nflfastR completion % over expected (accuracy)")),
                          print(h5("DVOA = F.O. defense-adjusted value over average (efficiency)")),
                          print(h5("DYAR = F.O. defense-adjusted yards above replacement (gross production)")),
                          print(h5("PFF = individual PFF offense grade")),
                          print(h5("QBR = ESPN total QBR; measure of win probability added per play")),
                          print(h5("Sack% = sack rate (sacks/total dropbacks)")),
                          print(h5("AirYds = Air yards per attempt")),
                          print(h5("CP = nflfastR completion probability based on situation & air yards")),
                          print(h5("PFF Recv = team PFF receiving grade")),
                          print(h5("PFF Pblk = team PFF pass block grade")),
                          print(h5("PA/G = team points allowed per game")),
                          width = 12)
                      )),
             tabPanel("Game Logs",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("individual_percentiles_gamelog", label = "Stats as percentiles?", 
                                      choices = c("No","Yes")),
                          selectInput("individual_player_gamelog", label = "Player:", 
                                      choices = qb_gamelogs[1], selected = NULL),
                          selectInput("year_gamelog", label = "Year:", 
                                      choices = NULL),
                          width = 12),
                        mainPanel(
                          gt_output("gamelog"),
                          hr(),
                          print(h5("EPA = nflfastR expected points added (efficiency)")),
                          print(h5("CPOE = nflfastR completion % over expected (accuracy)")),
                          print(h5("DAKOTA = composite statistic that combines EPA/Play and CPOE")),
                          width = 12)
                      )),
             tabPanel("Similarity Scores",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("player_similarity", label = "Player:", 
                                      choices = qb_comps[1], selected = NULL),
                          selectInput("year_similarity", label = "Year:", 
                                      choices = NULL),
                          selectInput("similarity_context", label = "Take context into account?", 
                                      choices = c("No","Yes")),
                          width = 12),
                        mainPanel(
                          gt_output("similarity"),
                          hr(),
                          print(h5("Similarity = mean of 1 minus the absolute percentile difference in each category")),
                          print(h5("EPA = nflfastR expected points added per play (efficiency)")),
                          print(h5("CPOE = nflfastR completion % over expected (accuracy)")),
                          print(h5("DVOA = F.O. defense-adjusted value over average (efficiency)")),
                          print(h5("DYAR = F.O. defense-adjusted yards above replacement (gross production)")),
                          print(h5("PFF = individual PFF offense grade")),
                          print(h5("QBR = ESPN total QBR; measure of win probability added per play")),
                          print(h5("Sack% = sack rate (sacks/total dropbacks)")),
                          print(h5("AirYds = Air yards per attempt")),
                          print(h5("CP = nflfastR completion probability based on situation & air yards")),
                          print(h5("PFF Recv = team PFF receiving grade")),
                          print(h5("PFF Pblk = team PFF pass block grade")),
                          print(h5("PA/G = team points allowed per game")),
                          width = 12)
                      ))
             )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  observeEvent(input$player,
               {
                 updateSelectInput(session, input = "year",
                                   choices = (passers_years %>%
                                                filter(name == input$player) %>%
                                                pull(season)))
                 })
  
  observeEvent(input$comp_player_1,
               {
                 updateSelectInput(session, input = "year_1",
                                   choices = (qb_comps_years %>%
                                                filter(full_name == input$comp_player_1) %>%
                                                pull(season)))
               })
  
  observeEvent(input$comp_player_2,
               {
                 updateSelectInput(session, input = "year_2",
                                   choices = (qb_comps_years %>%
                                                filter(full_name == input$comp_player_2) %>%
                                                pull(season)))
               })
  
  observeEvent(input$comp_player_3,
               {
                 updateSelectInput(session, input = "year_3",
                                   choices = (qb_comps_years %>%
                                                filter(full_name == input$comp_player_3) %>%
                                                pull(season)))
               })
  
  observeEvent(input$comp_player_4,
               {
                 updateSelectInput(session, input = "year_4",
                                   choices = (qb_comps_years %>%
                                                filter(full_name == input$comp_player_4) %>%
                                                pull(season)))
               })
  
  observeEvent(input$individual_player_gamelog,
               {
                 updateSelectInput(session, input = "year_gamelog",
                                   choices = (qb_gamelogs_years %>%
                                                filter(name == input$individual_player_gamelog) %>%
                                                pull(season)))
               })
  
  observeEvent(input$player_similarity,
               {
                 updateSelectInput(session, input = "year_similarity",
                                   choices = (qb_comps_years %>%
                                                filter(full_name == input$player_similarity) %>%
                                                pull(season)))
               })
   
  output$img <- renderImage({
    
    year <- input$year
    player <- input$player
    
    list(src = paste0("Cards/", year, "/", player, ".png"),
       contentType = 'image/png',
       width = 660*1.4,
       height = 360*1.4,
       alt = "Please enter valid quarterback/year combo.")
  }, deleteFile = FALSE)
  
  output$comp <- render_gt({
    
    year_1 <- input$year_1
    year_2 <- input$year_2
    year_3 <- input$year_3
    year_4 <- input$year_4
    player_1 <- input$comp_player_1
    player_2 <- input$comp_player_2
    player_3 <- input$comp_player_3
    player_4 <- input$comp_player_4
    percentiles <- input$percentiles

    if (percentiles == "Yes"){
      
      qb_comps_csv %>%
        filter((full_name == player_1 & season == year_1) |
                 (full_name == player_2 & season == year_2) |
                 (full_name == player_3 & season == year_3) |
                 (full_name == player_4 & season == year_4)) %>%
        select(headshot_url:pa_pct) %>%
        gt() %>%
        gtExtras::gt_theme_538() %>%
        cols_label(
          headshot_url = "",
          team_logo_espn = "Team",
          season = "Season",
          full_name = "Player",
          EPA_pct = "EPA",
          CPOE_pct = "CPOE",
          DVOA_pct = "DVOA",
          DYAR_pct = "DYAR",
          PFF_pct = "PFF",
          qbr_pct = "QBR",
          sack_rate_pct = "Sack%",
          air_yards_pct = "AirYds",
          cp_pct = "CP",
          recv_pct = "PFF Recv",
          pblk_pct = "PFF Pblk",
          pa_pct = "PA/G"
        ) %>%
        tab_header(
          title = "Quarterback Season Comparison",
          subtitle = "Stats reported as percentiles for single seasons 2006-2021 (0 = lowest, 100 = highest)"
        ) %>%
        tab_source_note("Table: @Tucker_TnL, Data: #nflverse, PFF, ESPN, Football Outsiders") %>%
        cols_align(align = "center", columns = 1:17) %>%
        fmt_number(columns = c(6:17), decimals = 1) %>%
        data_color(
          columns = c(6:11), 
          colors =
            scales::col_numeric(
              palette = (
                palette = color_palette
              ) %>% as.character(),
              domain = 0:100
            )
        ) %>%
        data_color(
          columns = c(12), 
          colors =
            scales::col_numeric(
              palette = (
                palette = color_palette_reversed
              ) %>% as.character(),
              domain = 0:100
            )
        ) %>%
        text_transform(
          locations = cells_body(columns = 1),
          fn = function(x){
            map(x, ~web_image(
              url = .x,
              height = 75
            ))
          }
        ) %>%
        text_transform(
          locations = cells_body(columns = 4),
          fn = function(x){
            map(x, ~web_image(
              url = .x,
              height = 75
            ))
          }
        ) %>%
        tab_spanner(label = "Performance", columns = 6:12) %>%
        tab_spanner(label = "Context", columns = 13:17)
    }
    
    
    else{
      
      qb_comps_csv %>%
        filter((full_name == player_1 & season == year_1) |
                 (full_name == player_2 & season == year_2) |
                 (full_name == player_3 & season == year_3) |
                 (full_name == player_4 & season == year_4)) %>%
        select(headshot_url:Plays, EPA:pa) %>%
        gt() %>%
        gtExtras::gt_theme_538() %>%
        cols_label(
          headshot_url = "",
          team_logo_espn = "Team",
          season = "Season",
          full_name = "Player",
          EPA = "EPA",
          CPOE = "CPOE",
          DVOA = "DVOA",
          DYAR = "DYAR",
          PFF = "PFF",
          qbr_total = "QBR",
          sack_rate = "Sack%",
          air_yards = "AirYds",
          cp = "CP",
          grades_recv = "PFF Recv",
          grades_pblk = "PFF Pblk",
          pa = "PA/G"
        ) %>%
        tab_header(
          title = "Quarterback Season Comparison",
          subtitle = "Color scale = single seasons 2006-2021"
        ) %>%
        tab_source_note("Table: @Tucker_TnL, Data: #nflverse, PFF, ESPN, Football Outsiders") %>%
        cols_align(align = "center", columns = 1:17) %>%
        data_color(
          columns = 6,
          colors =
            scales::col_numeric(
              palette = (
                palette = color_palette
              ) %>% as.character(),
              domain = c(min(qb_comps_csv$EPA, na.rm = T),max(qb_comps_csv$EPA, na.rm = T))
            )
        ) %>%
        data_color(
          columns = 7, 
          colors =
            scales::col_numeric(
              palette = (
                palette = color_palette
              ) %>% as.character(),
              domain = c(min(qb_comps_csv$CPOE, na.rm = T),max(qb_comps_csv$CPOE, na.rm = T))
            )
        ) %>%
        data_color(
          columns = 8, 
          colors =
            scales::col_numeric(
              palette = (
                palette = color_palette
              ) %>% as.character(),
              domain = c(min(qb_comps_csv$DVOA, na.rm = T),max(qb_comps_csv$DVOA, na.rm = T))
            )
        ) %>%
        data_color(
          columns = 9, 
          colors =
            scales::col_numeric(
              palette = (
                palette = color_palette
              ) %>% as.character(),
              domain = c(min(qb_comps_csv$DYAR, na.rm = T),max(qb_comps_csv$DYAR, na.rm = T))
            )
        ) %>%
        data_color(
          columns = 10, 
          colors =
            scales::col_numeric(
              palette = (
                palette = color_palette
              ) %>% as.character(),
              domain = c(min(qb_comps_csv$PFF, na.rm = T),max(qb_comps_csv$PFF, na.rm = T))
            )
        ) %>%
        data_color(
          columns = 11, 
          colors =
            scales::col_numeric(
              palette = (
                palette = color_palette
              ) %>% as.character(),
              domain = c(min(qb_comps_csv$qbr_total, na.rm = T),max(qb_comps_csv$qbr_total, na.rm = T))
            )
        ) %>%
        data_color(
          columns = 12, 
          colors =
            scales::col_numeric(
              palette = (
                palette = color_palette_reversed
              ) %>% as.character(),
              domain = c(min(qb_comps_csv$sack_rate, na.rm = T),max(qb_comps_csv$sack_rate, na.rm = T))
            )
        ) %>%
        text_transform(
          locations = cells_body(columns = 1),
          fn = function(x){
            map(x, ~web_image(
              url = .x,
              height = 75
            ))
          }
        ) %>%
        text_transform(
          locations = cells_body(columns = 4),
          fn = function(x){
            map(x, ~web_image(
              url = .x,
              height = 75
            ))
          }
        ) %>%
        fmt_number(columns = c(6), decimals = 2) %>%
        fmt_number(columns = c(7,10:11,13,15:17), decimals = 1) %>%
        fmt_percent(columns = c(8,12), decimals = 1) %>%
        fmt_percent(columns = c(14), decimals = 0) %>%
        tab_spanner(label = "Performance", columns = 6:12) %>%
        tab_spanner(label = "Context", columns = 13:17)
    }
    
    
  })
  
  output$individual <- render_gt({
    
    player <- input$individual_player
    percentiles <- input$individual_percentiles
    
    if (percentiles == "Yes"){
      
      qb_comps_csv %>%
        filter(full_name == player) %>%
        select(season, team_wordmark, Plays:pa_pct) %>%
        gt() %>%
        gtExtras::gt_theme_538() %>%
        cols_label(
          team_wordmark = "Team",
          season = "Season",
          EPA_pct = "EPA",
          CPOE_pct = "CPOE",
          DVOA_pct = "DVOA",
          DYAR_pct = "DYAR",
          PFF_pct = "PFF",
          sack_rate_pct = "Sack%",
          air_yards_pct = "AirYds",
          cp_pct = "CP",
          qbr_pct = "QBR",
          recv_pct = "PFF Recv",
          pblk_pct = "PFF Pblk",
          pa_pct = "PA/G"
        ) %>%
        tab_header(
          title = paste0(player, " Advanced Season Stats"),
          subtitle = "Stats reported as percentiles for single seasons 2006-2021 (0 = lowest, 100 = highest); 300+ plays to qualify"
        ) %>%
        tab_source_note("Table: @Tucker_TnL, Data: #nflverse, PFF, ESPN, Football Outsiders") %>%
        cols_align(align = "center", columns = 1:15) %>%
        fmt_number(columns = c(4:15), decimals = 1) %>%
        data_color(
          columns = c(4:9), 
          colors =
            scales::col_numeric(
              palette = (
                palette = color_palette
              ) %>% as.character(),
              domain = 0:100
            )
        ) %>%
        data_color(
          columns = c(10), 
          colors =
            scales::col_numeric(
              palette = (
                palette = color_palette_reversed
              ) %>% as.character(),
              domain = 0:100
            )
        ) %>%
        text_transform(
          locations = cells_body(columns = 2),
          fn = function(x){
            map(x, ~web_image(
              url = .x,
              height = 25
            ))
          }
        ) %>%
        tab_spanner(label = "Performance", columns = 4:10) %>%
        tab_spanner(label = "Context", columns = 11:15)
      
    }
    
    
    else{
      
      qb_comps_csv %>%
        filter(full_name == player) %>%
        select(season, team_wordmark, Plays, EPA:pa) %>%
        gt() %>%
        gtExtras::gt_theme_538() %>%
        cols_label(
          team_wordmark = "Team",
          season = "Season",
          EPA = "EPA",
          CPOE = "CPOE",
          DVOA = "DVOA",
          DYAR = "DYAR",
          PFF = "PFF",
          qbr_total = "QBR",
          sack_rate = "Sack%",
          air_yards = "AirYds",
          cp = "CP",
          grades_recv = "PFF Recv",
          grades_pblk = "PFF Pblk",
          pa = "PA/G"
        ) %>%
        tab_header(
          title = paste0(player, " Advanced Season Stats"),
          subtitle = "Color scale = single seasons 2006-2021; 300+ plays to qualify"
        ) %>%
        tab_source_note("Table: @Tucker_TnL, Data: #nflverse, PFF, ESPN, Football Outsiders") %>%
        cols_align(align = "center", columns = 1:15) %>%
        data_color(
          columns = 4,
          colors =
            scales::col_numeric(
              palette = (
                palette = color_palette
              ) %>% as.character(),
              domain = c(min(qb_comps_csv$EPA, na.rm = T),max(qb_comps_csv$EPA, na.rm = T))
            )
        ) %>%
        data_color(
          columns = 5, 
          colors =
            scales::col_numeric(
              palette = (
                palette = color_palette
              ) %>% as.character(),
              domain = c(min(qb_comps_csv$CPOE, na.rm = T),max(qb_comps_csv$CPOE, na.rm = T))
            )
        ) %>%
        data_color(
          columns = 6, 
          colors =
            scales::col_numeric(
              palette = (
                palette = color_palette
              ) %>% as.character(),
              domain = c(min(qb_comps_csv$DVOA, na.rm = T),max(qb_comps_csv$DVOA, na.rm = T))
            )
        ) %>%
        data_color(
          columns = 7, 
          colors =
            scales::col_numeric(
              palette = (
                palette = color_palette
              ) %>% as.character(),
              domain = c(min(qb_comps_csv$DYAR, na.rm = T),max(qb_comps_csv$DYAR, na.rm = T))
            )
        ) %>%
        data_color(
          columns = 8, 
          colors =
            scales::col_numeric(
              palette = (
                palette = color_palette
              ) %>% as.character(),
              domain = c(min(qb_comps_csv$PFF, na.rm = T),max(qb_comps_csv$PFF, na.rm = T))
            )
        ) %>%
        data_color(
          columns = 9, 
          colors =
            scales::col_numeric(
              palette = (
                palette = color_palette
              ) %>% as.character(),
              domain = c(min(qb_comps_csv$qbr_total, na.rm = T),max(qb_comps_csv$qbr_total, na.rm = T))
            )
        ) %>%
        data_color(
          columns = 10, 
          colors =
            scales::col_numeric(
              palette = (
                palette = color_palette_reversed
              ) %>% as.character(),
              domain = c(min(qb_comps_csv$sack_rate, na.rm = T),max(qb_comps_csv$sack_rate, na.rm = T))
            )
        ) %>%
        text_transform(
          locations = cells_body(columns = 2),
          fn = function(x){
            map(x, ~web_image(
              url = .x,
              height = 25
            ))
          }
        ) %>%
        fmt_number(columns = c(4), decimals = 2) %>%
        fmt_number(columns = c(5,8:9,11,13:15), decimals = 1) %>%
        fmt_number(columns = c(7), decimals = 0) %>%
        fmt_percent(columns = c(6,10), decimals = 1) %>%
        fmt_percent(columns = c(12), decimals = 0) %>%
        tab_spanner(label = "Performance", columns = 4:10) %>%
        tab_spanner(label = "Context", columns = 11:15)
    }
    
    
  })
  
  output$gamelog <- render_gt({
    
    player <- input$individual_player_gamelog
    percentiles <- input$individual_percentiles_gamelog
    year <- input$year_gamelog

    if (percentiles == "Yes"){
      
      if (year > 2005){
        
        qb_gamelogs_csv %>%
          filter(name == player, season == year) %>%
          select(week, team_wordmark, plays_pct:cpoe_pct, dakota_pct, pass_att_pct:sack_epa_pct) %>%
          gt() %>%
          gtExtras::gt_theme_538() %>%
          cols_label(
            week = "Week",
            team_wordmark = "Opp",
            plays_pct = "Plays",
            dropbacks_pct = "Dropbacks",
            total_epa_pct = "Total EPA",
            epa_play_pct = "EPA/Play",
            scrambles_pct = "Plays",
            scramble_epa_pct = "EPA",
            rushes_pct = "Plays",
            rush_epa_pct = "EPA",
            pass_att_pct = "Plays",
            pass_epa_pct = "EPA",
            sacks_pct = "Plays",
            sack_epa_pct = "EPA",
            cpoe_pct = "CPOE",
            dakota_pct = "DAKOTA"
          ) %>%
          tab_header(
            title = paste0(player, " Advanced Game Log, ", year),
            subtitle = "Stats reported as percentiles for single games 2006-2021 (0 = lowest, 100 = highest); 5+ dropbacks to qualify"
          ) %>%
          tab_source_note("Table: @Tucker_TnL, Data: #nflverse") %>%
          cols_align(align = "center", columns = 1:16) %>%
          fmt_number(columns = 5:16, decimals = 1) %>%
          data_color(
            columns = c(5:8,10,12,14,16), 
            colors =
              scales::col_numeric(
                palette = (
                  palette = color_palette
                ) %>% as.character(),
                domain = 0:100
              )
          )  %>%
          fmt_number(columns = 3:16, decimals = 1) %>%
          tab_spanner(label = "Overall", columns = 5:8) %>%
          tab_spanner(label = "Pass Att", columns = 9:10) %>%
          tab_spanner(label = "Rushes", columns = 11:12) %>%
          tab_spanner(label = "Scrambles", columns = 13:14) %>%
          tab_spanner(label = "Sacks", columns = 15:16) %>%
          text_transform(
            locations = cells_body(columns = 2),
            fn = function(x){
              map(x, ~web_image(
                url = .x,
                height = 25
              ))
            }
          )
      }
      
      else{
        
        qb_gamelogs_csv %>%
          filter(name == player, season == year) %>%
          select(week, team_wordmark, plays_pct:cpoe_pct, dakota_pct, pass_att_pct:sack_epa_pct) %>%
          gt() %>%
          gtExtras::gt_theme_538() %>%
          cols_label(
            week = "Week",
            team_wordmark = "Opp",
            plays_pct = "Plays",
            dropbacks_pct = "Dropbacks",
            total_epa_pct = "Total EPA",
            epa_play_pct = "EPA/Play",
            scrambles_pct = "Plays",
            scramble_epa_pct = "EPA",
            rushes_pct = "Plays",
            rush_epa_pct = "EPA",
            pass_att_pct = "Plays",
            pass_epa_pct = "EPA",
            sacks_pct = "Plays",
            sack_epa_pct = "EPA",
            cpoe_pct = "CPOE",
            dakota_pct = "DAKOTA"
          ) %>%
          tab_header(
            title = paste0(player, " Advanced Game Log, ", year),
            subtitle = "Stats reported as percentiles for single games 2006-2021 (0 = lowest, 100 = highest); 5+ dropbacks to qualify"
          ) %>%
          tab_source_note("Table: @Tucker_TnL, Data: #nflverse") %>%
          cols_align(align = "center", columns = 1:16) %>%
          fmt_number(columns = 5:16, decimals = 1) %>%
          data_color(
            columns = c(5:8,10,12,14,16), 
            colors =
              scales::col_numeric(
                palette = (
                  palette = color_palette
                ) %>% as.character(),
                domain = 0:100
              )
          )  %>%
          fmt_number(columns = 3:16, decimals = 1) %>%
          tab_spanner(label = "Overall", columns = 5:8) %>%
          tab_spanner(label = "Pass Att", columns = 9:10) %>%
          tab_spanner(label = "Rushes", columns = 11:12) %>%
          tab_spanner(label = "Scrambles", columns = 13:14) %>%
          tab_spanner(label = "Sacks", columns = 15:16) %>%
          text_transform(
            locations = cells_body(columns = 2),
            fn = function(x){
              map(x, ~web_image(
                url = .x,
                height = 25
              ))
            }
          ) %>%
          cols_hide(columns = 7)
          
      }
      
    }
    
    
    else{
      
      if (year > 2005){
        
        qb_gamelogs_csv %>%
          filter(name == player, season == year) %>%
          select(week, team_wordmark, plays:cpoe, dakota, pass_att:sack_epa) %>%
          gt() %>%
          gtExtras::gt_theme_538() %>%
          cols_label(
            week = "Week",
            team_wordmark = "Opp",
            plays = "Plays",
            dropbacks = "Dropbacks",
            total_epa = "Total EPA",
            epa_play = "EPA/Play",
            scrambles = "Plays",
            scramble_epa = "EPA",
            rushes = "Plays",
            rush_epa = "EPA",
            pass_att = "Plays",
            pass_epa = "EPA",
            sacks = "Plays",
            sack_epa = "EPA",
            cpoe = "CPOE",
            dakota = "DAKOTA"
          ) %>%
          tab_header(
            title = paste0(player, " Advanced Game Log, ", year),
            subtitle = "Color scale = single games 2006-2021; 5+ dropbacks to qualify"
          ) %>%
          tab_source_note("Table: @Tucker_TnL, Data: #nflverse") %>%
          cols_align(align = "center", columns = 1:16) %>%
          fmt_number(columns = 5:16, decimals = 1) %>%
          data_color(
            columns = 5, 
            colors =
              scales::col_numeric(
                palette = (
                  palette = color_palette
                ) %>% as.character(),
                domain = c(min(qb_gamelogs_csv$total_epa, na.rm = T),max(qb_gamelogs_csv$total_epa, na.rm = T))
              )
          ) %>%
          data_color(
            columns = 6, 
            colors =
              scales::col_numeric(
                palette = (
                  palette = color_palette
                ) %>% as.character(),
                domain = c(min(qb_gamelogs_csv$epa_play, na.rm = T),max(qb_gamelogs_csv$epa_play, na.rm = T))
              )
          ) %>%
          data_color(
            columns = 7, 
            colors =
              scales::col_numeric(
                palette = (
                  palette = color_palette
                ) %>% as.character(),
                domain = c(min(qb_gamelogs_csv$cpoe, na.rm = T),max(qb_gamelogs_csv$cpoe, na.rm = T))
              )
          ) %>%
          data_color(
            columns = 8, 
            colors =
              scales::col_numeric(
                palette = (
                  palette = color_palette
                ) %>% as.character(),
                domain = c(min(qb_gamelogs_csv$dakota, na.rm = T),max(qb_gamelogs_csv$dakota, na.rm = T))
              )
          ) %>%
          data_color(
            columns = 10, 
            colors =
              scales::col_numeric(
                palette = (
                  palette = color_palette
                ) %>% as.character(),
                domain = c(min(qb_gamelogs_csv$pass_epa, na.rm = T),max(qb_gamelogs_csv$pass_epa, na.rm = T))
              )
          ) %>%
          data_color(
            columns = 12, 
            colors =
              scales::col_numeric(
                palette = (
                  palette = color_palette
                ) %>% as.character(),
                domain = c(min(qb_gamelogs_csv$rush_epa, na.rm = T),max(qb_gamelogs_csv$rush_epa, na.rm = T))
              )
          ) %>%
          data_color(
            columns = 14, 
            colors =
              scales::col_numeric(
                palette = (
                  palette = color_palette
                ) %>% as.character(),
                domain = c(min(qb_gamelogs_csv$scramble_epa, na.rm = T),max(qb_gamelogs_csv$scramble_epa, na.rm = T))
              )
          ) %>%
          data_color(
            columns = 16, 
            colors =
              scales::col_numeric(
                palette = (
                  palette = color_palette
                ) %>% as.character(),
                domain = c(min(qb_gamelogs_csv$sack_epa, na.rm = T),max(qb_gamelogs_csv$sack_epa, na.rm = T))
              )
          ) %>%
          fmt_number(columns = c(9,11,13,15), decimals = 0) %>%
          fmt_number(columns = c(5,7,10,12,14,16), decimals = 1) %>%
          fmt_number(columns = c(6,8), decimals = 2) %>%
          tab_spanner(label = "Overall", columns = 5:8) %>%
          tab_spanner(label = "Pass Att", columns = 9:10) %>%
          tab_spanner(label = "Rushes", columns = 11:12) %>%
          tab_spanner(label = "Scrambles", columns = 13:14) %>%
          tab_spanner(label = "Sacks", columns = 15:16) %>%
          text_transform(
            locations = cells_body(columns = 2),
            fn = function(x){
              map(x, ~web_image(
                url = .x,
                height = 25
              ))
            }
          )
      }
      
      else{
        
        qb_gamelogs_csv %>%
          filter(name == player, season == year) %>%
          select(week, team_wordmark, plays:cpoe, dakota, pass_att:sack_epa) %>%
          gt() %>%
          gtExtras::gt_theme_538() %>%
          cols_label(
            week = "Week",
            team_wordmark = "Opp",
            plays = "Plays",
            dropbacks = "Dropbacks",
            total_epa = "Total EPA",
            epa_play = "EPA/Play",
            scrambles = "Plays",
            scramble_epa = "EPA",
            rushes = "Plays",
            rush_epa = "EPA",
            pass_att = "Plays",
            pass_epa = "EPA",
            sacks = "Plays",
            sack_epa = "EPA",
            cpoe = "CPOE",
            dakota = "DAKOTA"
          ) %>%
          tab_header(
            title = paste0(player, " Advanced Game Log, ", year),
            subtitle = "Color scale = single games 2006-2021; 5+ dropbacks to qualify"
          ) %>%
          tab_source_note("Table: @Tucker_TnL, Data: #nflverse") %>%
          cols_align(align = "center", columns = 1:16) %>%
          fmt_number(columns = 5:16, decimals = 1) %>%
          data_color(
            columns = 5, 
            colors =
              scales::col_numeric(
                palette = (
                  palette = color_palette
                ) %>% as.character(),
                domain = c(min(qb_gamelogs_csv$total_epa, na.rm = T),max(qb_gamelogs_csv$total_epa, na.rm = T))
              )
          ) %>%
          data_color(
            columns = 6, 
            colors =
              scales::col_numeric(
                palette = (
                  palette = color_palette
                ) %>% as.character(),
                domain = c(min(qb_gamelogs_csv$epa_play, na.rm = T),max(qb_gamelogs_csv$epa_play, na.rm = T))
              )
          ) %>%
          data_color(
            columns = 7, 
            colors =
              scales::col_numeric(
                palette = (
                  palette = color_palette
                ) %>% as.character(),
                domain = c(min(qb_gamelogs_csv$cpoe, na.rm = T),max(qb_gamelogs_csv$cpoe, na.rm = T))
              )
          ) %>%
          data_color(
            columns = 8, 
            colors =
              scales::col_numeric(
                palette = (
                  palette = color_palette
                ) %>% as.character(),
                domain = c(min(qb_gamelogs_csv$dakota, na.rm = T),max(qb_gamelogs_csv$dakota, na.rm = T))
              )
          ) %>%
          data_color(
            columns = 10, 
            colors =
              scales::col_numeric(
                palette = (
                  palette = color_palette
                ) %>% as.character(),
                domain = c(min(qb_gamelogs_csv$pass_epa, na.rm = T),max(qb_gamelogs_csv$pass_epa, na.rm = T))
              )
          ) %>%
          data_color(
            columns = 12, 
            colors =
              scales::col_numeric(
                palette = (
                  palette = color_palette
                ) %>% as.character(),
                domain = c(min(qb_gamelogs_csv$rush_epa, na.rm = T),max(qb_gamelogs_csv$rush_epa, na.rm = T))
              )
          ) %>%
          data_color(
            columns = 14, 
            colors =
              scales::col_numeric(
                palette = (
                  palette = color_palette
                ) %>% as.character(),
                domain = c(min(qb_gamelogs_csv$scramble_epa, na.rm = T),max(qb_gamelogs_csv$scramble_epa, na.rm = T))
              )
          ) %>%
          data_color(
            columns = 16, 
            colors =
              scales::col_numeric(
                palette = (
                  palette = color_palette
                ) %>% as.character(),
                domain = c(min(qb_gamelogs_csv$sack_epa, na.rm = T),max(qb_gamelogs_csv$sack_epa, na.rm = T))
              )
          ) %>%
          fmt_number(columns = c(9,11,13,15), decimals = 0) %>%
          fmt_number(columns = c(5,7,10,12,14,16), decimals = 1) %>%
          fmt_number(columns = c(6,8), decimals = 2) %>%
          tab_spanner(label = "Overall", columns = 5:8) %>%
          tab_spanner(label = "Pass Att", columns = 9:10) %>%
          tab_spanner(label = "Rushes", columns = 11:12) %>%
          tab_spanner(label = "Scrambles", columns = 13:14) %>%
          tab_spanner(label = "Sacks", columns = 15:16) %>%
          text_transform(
            locations = cells_body(columns = 2),
            fn = function(x){
              map(x, ~web_image(
                url = .x,
                height = 25
              ))
            }
          ) %>%
          cols_hide(columns = 7)
        
      }
      
    }
    
    
  })
  
  output$similarity <- render_gt({
    
    comp_player <- qb_comps_csv %>%
      filter(full_name == input$player_similarity & season == input$year_similarity)
    
    if (input$similarity_context == "No"){
      
      qb_comps_csv %>%
        mutate(
          EPA_comp = 100 - abs(EPA_pct-comp_player$EPA_pct),
          CPOE_comp = 100 - abs(CPOE_pct-comp_player$CPOE_pct),
          DVOA_comp = 100 - abs(DVOA_pct-comp_player$DVOA_pct),
          DYAR_comp = 100 - abs(DYAR_pct-comp_player$DYAR_pct),
          PFF_comp = 100 - abs(PFF_pct-comp_player$PFF_pct),
          qbr_comp = 100 - abs(qbr_pct-comp_player$qbr_pct),
          sack_rate_comp = 100 - abs(sack_rate_pct-comp_player$sack_rate_pct),
          avg_comp = (EPA_comp+CPOE_comp+DVOA_comp+DYAR_comp+PFF_comp+qbr_comp+sack_rate_comp)/700
        ) %>%
        arrange(desc(avg_comp)) %>%
        head(10) %>%
        select(headshot_url:Plays, EPA:pa, avg_comp) %>%
        gt() %>%
        gtExtras::gt_theme_538() %>%
        cols_label(
          headshot_url = "",
          team_logo_espn = "Team",
          season = "Season",
          full_name = "Player",
          EPA = "EPA",
          CPOE = "CPOE",
          DVOA = "DVOA",
          DYAR = "DYAR",
          PFF = "PFF",
          qbr_total = "QBR",
          sack_rate = "Sack%",
          air_yards = "AirYds",
          cp = "CP",
          grades_recv = "PFF Recv",
          grades_pblk = "PFF Pblk",
          pa = "PA/G",
          avg_comp = "Similarity"
        ) %>%
        tab_header(
          title = paste0("Most Similar Seasons to ", comp_player$full_name[1], ", ", comp_player$season[1]),
          subtitle = "Similarity = Average percentile similarity of all categories, contextual factors excluded"
        ) %>%
        tab_source_note("Table: @Tucker_TnL, Data: #nflverse, PFF, ESPN, Football Outsiders") %>%
        cols_align(align = "center") %>%
        data_color(
          columns = 6,
          colors =
            scales::col_numeric(
              palette = (
                palette = color_palette
              ) %>% as.character(),
              domain = c(min(qb_comps_csv$EPA, na.rm = T),max(qb_comps_csv$EPA, na.rm = T))
            )
        ) %>%
        data_color(
          columns = 7, 
          colors =
            scales::col_numeric(
              palette = (
                palette = color_palette
              ) %>% as.character(),
              domain = c(min(qb_comps_csv$CPOE, na.rm = T),max(qb_comps_csv$CPOE, na.rm = T))
            )
        ) %>%
        data_color(
          columns = 8, 
          colors =
            scales::col_numeric(
              palette = (
                palette = color_palette
              ) %>% as.character(),
              domain = c(min(qb_comps_csv$DVOA, na.rm = T),max(qb_comps_csv$DVOA, na.rm = T))
            )
        ) %>%
        data_color(
          columns = 9, 
          colors =
            scales::col_numeric(
              palette = (
                palette = color_palette
              ) %>% as.character(),
              domain = c(min(qb_comps_csv$DYAR, na.rm = T),max(qb_comps_csv$DYAR, na.rm = T))
            )
        ) %>%
        data_color(
          columns = 10, 
          colors =
            scales::col_numeric(
              palette = (
                palette = color_palette
              ) %>% as.character(),
              domain = c(min(qb_comps_csv$PFF, na.rm = T),max(qb_comps_csv$PFF, na.rm = T))
            )
        ) %>%
        data_color(
          columns = 11, 
          colors =
            scales::col_numeric(
              palette = (
                palette = color_palette
              ) %>% as.character(),
              domain = c(min(qb_comps_csv$qbr_total, na.rm = T),max(qb_comps_csv$qbr_total, na.rm = T))
            )
        ) %>%
        data_color(
          columns = 12, 
          colors =
            scales::col_numeric(
              palette = (
                palette = color_palette_reversed
              ) %>% as.character(),
              domain = c(min(qb_comps_csv$sack_rate, na.rm = T),max(qb_comps_csv$sack_rate, na.rm = T))
            )
        ) %>%
        data_color(
          columns = 18, 
          colors =
            scales::col_numeric(
              palette = paletteer::paletteer_d(
                palette = "ggsci::blue_material"
              ) %>% as.character(),
              domain = NULL
            )
        ) %>%
        text_transform(
          locations = cells_body(columns = 1),
          fn = function(x){
            map(x, ~web_image(
              url = .x,
              height = 75
            ))
          }
        ) %>%
        text_transform(
          locations = cells_body(columns = 4),
          fn = function(x){
            map(x, ~web_image(
              url = .x,
              height = 75
            ))
          }
        ) %>%
        fmt_number(columns = c(6), decimals = 2) %>%
        fmt_number(columns = c(7,10:11,13,15:17), decimals = 1) %>%
        fmt_percent(columns = c(8,12,18), decimals = 1) %>%
        fmt_percent(columns = c(14), decimals = 0) %>%
        tab_spanner(label = "Performance", columns = 6:12) %>%
        tab_spanner(label = "Context", columns = 13:17)
    }
    
    else{
      
      qb_comps_csv %>%
        mutate(
          EPA_comp = 100 - abs(EPA_pct-comp_player$EPA_pct),
          CPOE_comp = 100 - abs(CPOE_pct-comp_player$CPOE_pct),
          DVOA_comp = 100 - abs(DVOA_pct-comp_player$DVOA_pct),
          DYAR_comp = 100 - abs(DYAR_pct-comp_player$DYAR_pct),
          PFF_comp = 100 - abs(PFF_pct-comp_player$PFF_pct),
          qbr_comp = 100 - abs(qbr_pct-comp_player$qbr_pct),
          sack_rate_comp = 100 - abs(sack_rate_pct-comp_player$sack_rate_pct),
          air_yards_comp = 100 - abs(air_yards_pct-comp_player$air_yards_pct),
          cp_comp = 100 - abs(cp_pct-comp_player$cp_pct),
          recv_comp = 100 - abs(recv_pct-comp_player$recv_pct),
          pblk_comp = 100 - abs(pblk_pct-comp_player$pblk_pct),
          pa_comp = 100 - abs(pa_pct-comp_player$pa_pct),
          avg_comp = (EPA_comp+CPOE_comp+DVOA_comp+DYAR_comp+PFF_comp+qbr_comp+sack_rate_comp+
                        air_yards_comp+cp_comp+recv_comp+pblk_comp+pa_comp)/1200
        ) %>%
        arrange(desc(avg_comp)) %>%
        head(10) %>%
        select(headshot_url:Plays, EPA:pa, avg_comp) %>%
        gt() %>%
        gtExtras::gt_theme_538() %>%
        cols_label(
          headshot_url = "",
          team_logo_espn = "Team",
          season = "Season",
          full_name = "Player",
          EPA = "EPA",
          CPOE = "CPOE",
          DVOA = "DVOA",
          DYAR = "DYAR",
          PFF = "PFF",
          qbr_total = "QBR",
          sack_rate = "Sack%",
          air_yards = "AirYds",
          cp = "CP",
          grades_recv = "PFF Recv",
          grades_pblk = "PFF Pblk",
          pa = "PA/G",
          avg_comp = "Similarity"
        ) %>%
        tab_header(
          title = paste0("Most Similar Seasons to ", comp_player$full_name[1], ", ", comp_player$season[1]),
          subtitle = "Similarity = Average percentile similarity of all categories, contextual factors included"
        ) %>%
        tab_source_note("Table: @Tucker_TnL, Data: #nflverse, PFF, ESPN, Football Outsiders") %>%
        cols_align(align = "center") %>%
        data_color(
          columns = 6,
          colors =
            scales::col_numeric(
              palette = (
                palette = color_palette
              ) %>% as.character(),
              domain = c(min(qb_comps_csv$EPA, na.rm = T),max(qb_comps_csv$EPA, na.rm = T))
            )
        ) %>%
        data_color(
          columns = 7, 
          colors =
            scales::col_numeric(
              palette = (
                palette = color_palette
              ) %>% as.character(),
              domain = c(min(qb_comps_csv$CPOE, na.rm = T),max(qb_comps_csv$CPOE, na.rm = T))
            )
        ) %>%
        data_color(
          columns = 8, 
          colors =
            scales::col_numeric(
              palette = (
                palette = color_palette
              ) %>% as.character(),
              domain = c(min(qb_comps_csv$DVOA, na.rm = T),max(qb_comps_csv$DVOA, na.rm = T))
            )
        ) %>%
        data_color(
          columns = 9, 
          colors =
            scales::col_numeric(
              palette = (
                palette = color_palette
              ) %>% as.character(),
              domain = c(min(qb_comps_csv$DYAR, na.rm = T),max(qb_comps_csv$DYAR, na.rm = T))
            )
        ) %>%
        data_color(
          columns = 10, 
          colors =
            scales::col_numeric(
              palette = (
                palette = color_palette
              ) %>% as.character(),
              domain = c(min(qb_comps_csv$PFF, na.rm = T),max(qb_comps_csv$PFF, na.rm = T))
            )
        ) %>%
        data_color(
          columns = 11, 
          colors =
            scales::col_numeric(
              palette = (
                palette = color_palette
              ) %>% as.character(),
              domain = c(min(qb_comps_csv$qbr_total, na.rm = T),max(qb_comps_csv$qbr_total, na.rm = T))
            )
        ) %>%
        data_color(
          columns = 12, 
          colors =
            scales::col_numeric(
              palette = (
                palette = color_palette_reversed
              ) %>% as.character(),
              domain = c(min(qb_comps_csv$sack_rate, na.rm = T),max(qb_comps_csv$sack_rate, na.rm = T))
            )
        ) %>%
        data_color(
          columns = 18, 
          colors =
            scales::col_numeric(
              palette = paletteer::paletteer_d(
                palette = "ggsci::blue_material"
              ) %>% as.character(),
              domain = NULL
            )
        ) %>%
        text_transform(
          locations = cells_body(columns = 1),
          fn = function(x){
            map(x, ~web_image(
              url = .x,
              height = 75
            ))
          }
        ) %>%
        text_transform(
          locations = cells_body(columns = 4),
          fn = function(x){
            map(x, ~web_image(
              url = .x,
              height = 75
            ))
          }
        ) %>%
        fmt_number(columns = c(6), decimals = 2) %>%
        fmt_number(columns = c(7,10:11,13,15:17), decimals = 1) %>%
        fmt_percent(columns = c(8,12,18), decimals = 1) %>%
        fmt_percent(columns = c(14), decimals = 0) %>%
        tab_spanner(label = "Performance", columns = 6:12) %>%
        tab_spanner(label = "Context", columns = 13:17)
    }
    
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)