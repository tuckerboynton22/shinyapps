#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(nflreadr)
library(tidyverse)
library(ggtext)
library(plotly)

pbp <- read_csv("game_charts.csv")

teams <- load_teams()

games <- load_schedules()

create_breakdown <- function(pbp, season_entered, week_entered, team_entered, teams, games){
    
    game_breakdown <- pbp %>%
        filter(posteam == team_entered, week == week_entered, season == season_entered) %>%
        filter(!is.na(posteam), play_type_nfl != "END_QUARTER", play_type_nfl != "GAME_START",
               play_type_nfl != "END_GAME", play_type_nfl != "TIMEOUT") %>%
        mutate(
            play_type_nfl = case_when(
                play_type_nfl == "PUNT" | play_type_nfl == "KICK_OFF" ~ "PUNT/KO",
                play_type_nfl == "FIELD_GOAL" | play_type_nfl == "XP_KICK" | play_type_nfl == "PAT2" ~ "FG/PAT",
                TRUE ~ play_type_nfl
            ),
            epa = round(epa, 2)
        ) %>%
        arrange(abs(epa))
    
    game_breakdown$play_type_nfl <- factor(game_breakdown$play_type_nfl,
                                           levels = c("PASS","RUSH","SACK","PENALTY","FG/PAT","PUNT/KO"))
    
    team_name <- teams %>%
        filter(team_abbr == team_entered) %>%
        select(team_name) %>%
        as.character()
    
    game <- games %>%
        filter(week == week_entered, season == season_entered,
               home_team == team_entered | away_team == team_entered)
    
    x <- game_breakdown$play_type_nfl
    y <- game_breakdown$epa
    hover_text <- paste0("EPA: ", game_breakdown$epa, "\n",
                   "Down: ", game_breakdown$down, ", Distance: ", game_breakdown$ydstogo, "\n",
                   "Q", game_breakdown$qtr, " ",
                   game_breakdown$desc)
    
    data <- data.frame(x, y, hover_text)
    
    fig <- plot_ly(data, x = ~x, y = ~y, type = 'bar', text = hover_text,
                   hoverinfo = "text",
                   marker = list(color = ~game_breakdown$epa,
                                 showscale = TRUE,
                                 line = list(color = "black",
                                             width = 1.5)))
    fig <- fig %>% layout(title = paste0(team_name, " Possession Report - Week ",
                                         week_entered, ", ", season_entered, "\n",
                                         game$home_team[1], ": ", game$home_score[1],
                                         ", ", game$away_team[1], ": ", game$away_score[1]),
                          xaxis = list(title = ""),
                          yaxis = list(title = "EPA"))
    
    return(fig)
}

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

    # Application title
    titlePanel(title = h2("NFL Possession Game Breakdowns", align = "left"), windowTitle = "NFL Possession Game Breakdowns"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("team", label = "Team:", 
                        choices = teams[[1]]),
            selectInput("season", label = "Season:", 
                        choices = 2021:2017),
            selectInput("week", label = "Week:", 
                        choices = 1:21)),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("breakdown_plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$breakdown_plot <- renderPlotly({

        team <- input$team
        season <- input$season
        week <- input$week

        create_breakdown(pbp, season, week, team, teams, games)
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
