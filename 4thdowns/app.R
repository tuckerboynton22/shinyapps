#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(ggplot2)
library(tidyverse)
library(readxl)
library(ggimage)
library(nflfastR)
library(ggtext)
library(statar)
library(nfl4th)
library(DT)
library(plotly)

options(scipen = 99999)

fourth_downs <- read_csv("fourth_2021.csv") %>%
  mutate(season = 2021) %>%
  rbind(read_csv("fourth_2020.csv") %>% mutate(season = 2020)) %>%
  rbind(read_csv("fourth_2019.csv") %>% mutate(season = 2019)) %>%
  rbind(read_csv("fourth_2018.csv") %>% mutate(season = 2018)) %>%
  rbind(read_csv("fourth_2017.csv") %>% mutate(season = 2017)) %>%
  rbind(read_csv("fourth_2016.csv") %>% mutate(season = 2016)) %>%
  rbind(read_csv("fourth_2015.csv") %>% mutate(season = 2015)) %>%
  rbind(read_csv("fourth_2014.csv") %>% mutate(season = 2014)) %>%
  mutate(
    playdesc = paste0("Q", qtr, " ", str_trunc(desc, width = 10))
  ) %>%
  arrange(posteam, week)

calculate_decision <- function(row, graph_type){
  
  # row <- fourth_downs %>%
  #   filter(week == 15, posteam == "NE", qtr == 4, game_seconds_remaining == 540)
  
  if (graph_type == "interactive"){
    
    row_trimmed <- row %>%
      mutate(rows = 2601) %>%
      uncount(rows)
    
    row_trimmed$first_down_prob <- rep(seq(0, 1, 0.02), 51)
    row_trimmed$fg_make_prob <- rep(seq(0, 1, 0.02), each = 51)
    row_trimmed$punt_wp <- ifelse(is.na(row_trimmed$punt_wp), 0, row_trimmed$punt_wp)
    row_trimmed$xval_go <- row_trimmed$wp_succeed * row_trimmed$first_down_prob + (row_trimmed$wp_fail * (1-row_trimmed$first_down_prob))
    row_trimmed$xval_kick <- row_trimmed$make_fg_wp * row_trimmed$fg_make_prob + (row_trimmed$miss_fg_wp * (1-row_trimmed$fg_make_prob))
    row_trimmed <- row_trimmed %>%
      mutate(go_boost = ifelse(xval_kick > punt_wp, (xval_go-xval_kick)*100, (xval_go-punt_wp)*100),
             suggestion = case_when(
               go_boost > 0 ~ "Go",
               xval_kick > punt_wp ~ "FG",
               TRUE ~ "Punt"
             ))
  
    row_trimmed
    
  }
  else{
    
    row_trimmed <- row %>%
      mutate(rows = 10201) %>%
      uncount(rows)
    
    row_trimmed$first_down_prob <- rep(seq(0, 1, 0.01), 101)
    row_trimmed$fg_make_prob <- rep(seq(0, 1, 0.01), each = 101)
    row_trimmed$punt_wp <- ifelse(is.na(row_trimmed$punt_wp), 0, row_trimmed$punt_wp)
    row_trimmed$xval_go <- row_trimmed$wp_succeed * row_trimmed$first_down_prob + (row_trimmed$wp_fail * (1-row_trimmed$first_down_prob))
    row_trimmed$xval_kick <- row_trimmed$make_fg_wp * row_trimmed$fg_make_prob + (row_trimmed$miss_fg_wp * (1-row_trimmed$fg_make_prob))
    row_trimmed <- row_trimmed %>%
      mutate(go_boost = ifelse(xval_kick > punt_wp, (xval_go-xval_kick)*100, (xval_go-punt_wp)*100),
             suggestion = case_when(
               go_boost > 0 ~ "Go",
               xval_kick > punt_wp ~ "FG",
               TRUE ~ "Punt"
             ))
    
    row_trimmed
  }
  
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("@import url('https://fonts.googleapis.com/css2?family=Chivo&display=swap');
                    
                    /* Change header text to imported font */
                    h1 {
                    font-family: 'Chivo';
                    }"))
  ),
   
  # Application title
  # titlePanel(title = h1("4th Down Decision Making", align = "center"), windowTitle = "4th Down Decision Making"),
   
   # Sidebar with a slider input for number of bins 
  navbarPage("4th Down Decisions",
               tabPanel("Individual Decisions",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput("year", label = "Season", choices = fourth_downs %>% select(season) %>% distinct()),
                            selectInput("posteam", label = "Offensive Team", choices = fourth_downs %>% select(posteam) %>% distinct()),
                            selectInput("week_ind", label = "Week", choices = fourth_downs %>% select(week) %>% distinct()),
                            selectInput("desc", label = "Play", choices = NULL),
                            selectInput("colors", label = "Color palette", choices = c("Stoplight", "Hulk")),
                            selectInput("graph", label = "Interactive graph?", choices = c("No","Yes"))),
                          mainPanel(
                            conditionalPanel(
                              condition = "input.graph == 'No'",
                              plotOutput("plot2", height = "500px", width = "750px")),
                            conditionalPanel(
                              condition = "input.graph == 'Yes'",
                              plotlyOutput("plot3", height = "500px", width = "750px")),
                            hr(),
                            print(h5(tags$a(href="https://www.nfl4th.com/index.html", "WP projections based on @benbbaldwin's 4th down model")))
                          ))),
               tabPanel("Team Summaries",
                        sidebarLayout(
                          sidebarPanel(
                            sliderInput("season", label = "Year:",
                                        min = 2014,
                                        max = 2021,
                                        value = c(2021,2021),
                                        sep = ""),
                            sliderInput("week",
                                        "Weeks:",
                                        min = 1,
                                        max = 21,
                                        value = c(1,21)),
                            sliderInput("wp",
                                        "Pre-snap win probability:",
                                        min = 0,
                                        max = 1,
                                        value = c(0.05,0.95)),
                            sliderInput("uncertainty",
                                        "Min. strength of suggestion (percentage points):",
                                        min = 0,
                                        max = 5,
                                        value = 1),
                            numericInput("ydstogo",
                                         "Max. yards to go:",
                                         value = 99),
                            numericInput("seconds",
                                         "Min. game seconds remaining:",
                                         value = 0),
                            radioButtons("type",
                                         "Recommendation is:",
                                         choices = c("Go", "Kick", "Either"))),
                          mainPanel(
                            plotOutput("plot", height = "500px", width = "750px"),
                            hr(),
                            DTOutput("fourth_table"),
                            hr(),
                            print(h5("Expected win prob boost = projected WP boost relative to opposite decision")),
                            print(h5("Correct decision rate = share of decisions with positive xWP boost")),
                            print(h5(tags$a(href="https://www.nfl4th.com/index.html", "WP projections based on @benbbaldwin's 4th down model")))
                            ))
               )
             )
  )

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
    toListen <- reactive({
      list(input$year, input$week_ind, input$posteam)
    })
    observeEvent(toListen(), {
      
      year <- input$year
      week_ind <- input$week_ind
      team <- input$posteam
      
      updateSelectInput(session, input = "desc",
                        choices = (fourth_downs %>%
                                     filter(season == year,
                                            week == week_ind,
                                            posteam == team) %>%
                                     pull(playdesc)))
    })
   
   output$plot <- renderPlot({
     
     multiplier = case_when(
       input$type == "Go" ~ 1,
       input$type == "Kick" ~ -1,
       TRUE ~ 0
     )
     
     fourth_grouped <- fourth_downs %>%
       filter(abs(xwp_boost) >= input$uncertainty, week >= input$week[1],
              week <= input$week[2], game_seconds_remaining >= input$seconds,
              ydstogo <= input$ydstogo, wp >= input$wp[1], wp <= input$wp[2],
              season >= input$season[1], season <= input$season[2], go_boost*multiplier >= 0) %>%
       group_by(posteam) %>%
       summarize(
         xwp_boost = mean(xwp_boost),
         fourth_success = mean(fourth_success),
         n = n()
       ) %>%
       rename(team_abbr = posteam) %>%
       merge(teams_colors_logos)
     
     fourth_grouped %>%
       ggplot(aes(fourth_success, xwp_boost)) +
       geom_image(image = fourth_grouped$team_logo_espn, asp = 16/9) +
       labs(x = "Correct Decision Rate",
            y = "Expected Win Probability Boost per 4th Down",
            caption = "Figure: @Tucker_TnL, Data: @nflfastR, nfl4th",
            subtitle = paste0("4th & < ", input$ydstogo+1, ", Wks ", 
                              input$week[1], "-", input$week[2], ", Win probability ",
                              100*input$wp[1], "-", 100*input$wp[2], "%, ", input$seconds,
                              "+ seconds remaining, Model says '", input$type, "' by at least ",
                              input$uncertainty, " pp"),
            title = paste0("4th Down Decisions, ", input$season[1], "-", input$season[2])
       ) +
       ggthemes::theme_fivethirtyeight() +
       theme(
         legend.position = "none",
         plot.title = element_markdown(hjust = 0.5),
         plot.subtitle = element_markdown(size = 10, hjust = 0.5),
         axis.title.x = element_text(size = 14),
         axis.title.y = element_text(size = 14),
         axis.text.x = element_text(size = 10),
         axis.text.y = element_text(size = 10)
       ) +
       scale_y_continuous(breaks = scales::pretty_breaks()) +
       scale_x_continuous(breaks = scales::pretty_breaks())
   })
   
   output$fourth_table <- DT::renderDT({
     
     multiplier = case_when(
       input$type == "Go" ~ 1,
       input$type == "Kick" ~ -1,
       TRUE ~ 0
     )
     
     fourth_downs %>%
       filter(abs(xwp_boost) >= input$uncertainty, week >= input$week[1],
              week <= input$week[2], game_seconds_remaining >= input$seconds,
              ydstogo <= input$ydstogo, wp >= input$wp[1], wp <= input$wp[2],
              season >= input$season[1], season <= input$season[2], go_boost*multiplier >= 0) %>%
       group_by(posteam) %>%
       rename(Team = posteam) %>%
       summarize(
         'Mean xWP Boost' = round(mean(xwp_boost), 2),
         'Correct Decision Rate' = round(mean(fourth_success), 2),
         n = n()
       ) %>%
       DT::datatable(rownames = FALSE, options=list(order=list(2,'desc'), pageLength=32))
  
   })
   
   output$plot2 <- renderPlot({
     
     year <- input$year
     week_ind <- input$week_ind
     team <- input$posteam
     play_desc <- input$desc
     
     row <- fourth_downs %>%
       filter(season == year, week == week_ind, posteam == team, playdesc == play_desc) %>%
       head(1)
     
     recommendation <- ifelse(row$go_boost[1] > 0, paste0("Rec: Go, +", round(row$go_boost[1],1)), paste0("Rec: Kick, +", round(-row$go_boost[1],1)))
     actual <- if_else(row$go[1] == 100, "Actual: Go", "Actual: Kick")
     label <- paste0(recommendation, "\n", actual)
     
     new_row <- calculate_decision(row, "basic")
     
     punt_color <- ifelse(input$colors == "Stoplight", "red", "#8f1f90")
     fg_color <- 	ifelse(input$colors == "Stoplight", "yellow", "#000000")
     go_color <- ifelse(input$colors == "Stoplight", "darkgreen", "#0a8c28")
     
     new_row %>%
       ggplot(aes(first_down_prob, fg_make_prob)) +
       geom_point(aes(alpha = abs(go_boost)), color = case_when(
         new_row$suggestion == "Punt" ~ punt_color,
         new_row$suggestion == "FG" ~ fg_color,
         TRUE ~ go_color
       )) +
       geom_point(aes(row$first_down_prob, row$fg_make_prob), color = "black", size = 6) +
       annotate("label", x = row$first_down_prob, y = row$fg_make_prob + 0.1, label = label, size = 3) +
       annotate("label", x = mean(new_row$first_down_prob[new_row$suggestion == "Go"]), y = mean(new_row$fg_make_prob[new_row$suggestion == "Go"]), label = "Go") +
       annotate("label", x = mean(new_row$first_down_prob[new_row$suggestion == "Punt"]), y = mean(new_row$fg_make_prob[new_row$suggestion == "Punt"]), label = "Punt") +
       annotate("label", x = mean(new_row$first_down_prob[new_row$suggestion == "FG"]), y = mean(new_row$fg_make_prob[new_row$suggestion == "FG"]), label = "FG") +
       labs(x = "1st Down Conversion Probability",
            y = "FG Make Probability",
            caption = "Figure: @Tucker_TnL, Data: #nflverse, Model: nfl4th",
            subtitle = paste0("Play: ", team, " vs. ", new_row$defteam[1], ", Wk ", week_ind, " ", year, ", 4th & ",
                              new_row$ydstogo[1], " from ", new_row$yrdln[1], ", ",
                              new_row$posteam_score[1], "-", new_row$defteam_score[1],
                              ", ", play_desc, " Black dot = actual model projection"),
            title = "Model Recommendation Based on FG Make & 1st Down Conversion Probabilities",
            alpha = "Projected WP Boost"
       ) +
       ggthemes::theme_fivethirtyeight() +
       theme(
         # legend.position = "none",
         plot.title = element_markdown(hjust = 0.5),
         plot.subtitle = element_markdown(size = 10, hjust = 0.5),
         axis.title.x = element_text(size = 14),
         axis.title.y = element_text(size = 14),
         axis.text.x = element_text(size = 10),
         axis.text.y = element_text(size = 10)
       ) +
       scale_y_continuous(breaks = scales::pretty_breaks()) +
       scale_x_continuous(breaks = scales::pretty_breaks())
   })
   
   output$plot3 <- renderPlotly({
     
     year <- input$year
     week_ind <- input$week_ind
     team <- input$posteam
     play_desc <- input$desc
     
     row <- fourth_downs %>%
       filter(season == year, week == week_ind, posteam == team, playdesc == play_desc) %>%
       head(1)
     
     recommendation <- ifelse(row$go_boost[1] > 0, paste0("Rec: Go, +", round(row$go_boost[1],1)), paste0("Rec: Kick, +", round(-row$go_boost[1],1)))
     actual <- if_else(row$go[1] == 100, "Actual: Go", "Actual: Kick")
     label <- paste0(recommendation, "\n", actual)
     
     new_row <- calculate_decision(row, "interactive")
     
     punt_color <- ifelse(input$colors == "Stoplight", "red", "#8f1f90")
     fg_color <- 	ifelse(input$colors == "Stoplight", "yellow", "#000000")
     go_color <- ifelse(input$colors == "Stoplight", "darkgreen", "#0a8c28")
     hover_text <- paste0(
       "MODEL REC: ", new_row$suggestion, "<br>",
       "1st down conversion prob: ", new_row$first_down_prob, "<br>",
       "FG make prob: ", new_row$fg_make_prob, "<br>",
       "Projected WP boost: ", round(abs(new_row$go_boost),2)
     )
     
     ggplotly(new_row %>%
                ggplot(aes(first_down_prob, fg_make_prob, text = hover_text)) +
                geom_point(aes(alpha = abs(go_boost)), color = case_when(
                  new_row$suggestion == "Punt" ~ punt_color,
                  new_row$suggestion == "FG" ~ fg_color,
                  TRUE ~ go_color
                )) +
                geom_point(aes(row$first_down_prob, row$fg_make_prob), color = "black", size = 6) +
                annotate("label", x = row$first_down_prob, y = row$fg_make_prob + 0.1, label = label, size = 3) +
                annotate("label", x = mean(new_row$first_down_prob[new_row$suggestion == "Go"]), y = mean(new_row$fg_make_prob[new_row$suggestion == "Go"]), label = "Go") +
                annotate("label", x = mean(new_row$first_down_prob[new_row$suggestion == "Punt"]), y = mean(new_row$fg_make_prob[new_row$suggestion == "Punt"]), label = "Punt") +
                annotate("label", x = mean(new_row$first_down_prob[new_row$suggestion == "FG"]), y = mean(new_row$fg_make_prob[new_row$suggestion == "FG"]), label = "FG") +
                labs(x = "1st Down Conversion Probability",
                     y = "FG Make Probability",
                     caption = "Figure: @Tucker_TnL, Data: #nflverse, Model: nfl4th",
                     title = ,
                     alpha = "Projected WP Boost"
                ) +
                ggthemes::theme_fivethirtyeight() +
                theme(
                  # legend.position = "none",
                  plot.title = element_markdown(size = 14, hjust = 0.5),
                  plot.subtitle = element_markdown(size = 10, hjust = 0.5),
                  axis.title.x = element_text(size = 14),
                  axis.title.y = element_text(size = 14),
                  axis.text.x = element_text(size = 10),
                  axis.text.y = element_text(size = 10)
                ) +
                scale_y_continuous(breaks = scales::pretty_breaks()) +
                scale_x_continuous(breaks = scales::pretty_breaks()), tooltip = "text") %>%
       layout(
         title = paste0("Model Recommendation Based on FG Make & 1st Down Conversion Probabilities<br><sup>",
                        "Play: ", team, " vs. ", new_row$defteam[1], ", Wk ", week_ind, " ", year, ", 4th & ",
                        new_row$ydstogo[1], " from ", new_row$yrdln[1], ", ",
                        new_row$posteam_score[1], "-", new_row$defteam_score[1],
                        ", ", play_desc, " Black dot = actual model projection</sup>")
       )
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

