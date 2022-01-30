#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(reactable)
library(htmltools)
library(nflreadr)

GnYlRd <- function(x) rgb(colorRamp(c("#f87274", "#ffeb84", "#63be7b"))(x), maxColorValue = 255)

create_table <- function(dbs, weeklo, weekhi, downlo, downhi, ydslo, ydshi, qtrlo, qtrhi, marginlo, marginhi, year){
  
  data <- load_pbp(seasons = as.numeric(year))
  
  passers <- data %>%
    filter(down <= downhi, down >= downlo, !is.na(name), !is.na(qb_epa), pass == 1 | rush == 1,
           week >= weeklo, week <= weekhi, ydstogo >= ydslo, ydstogo <= ydshi,
           qtr >= qtrlo, qtr <= qtrhi, score_differential >= marginlo, score_differential <= marginhi,
           !is.na(qb_epa)) %>%
    group_by(name) %>%
    summarize(
      Plays = n(),
      Dropbacks = sum(pass),
      Sacks = round(mean(qb_epa * sack, na.rm = T), digits = 3),
      Scrambles = round(mean(qb_epa * qb_scramble, na.rm = T), digits = 3),
      Rushes = round(mean(qb_epa * rush, na.rm = T), 3),
      Passes = round(mean(qb_epa * pass - qb_epa * qb_scramble - qb_epa * sack, na.rm = T), 3),
      Total = round(mean(qb_epa, na.rm = T), 3)
    ) %>%
    filter(Dropbacks >= dbs) %>%
    arrange(desc(Total)) %>%
    rename(Quarterback = name)
  
  reactable(passers,
            pagination = FALSE,
            defaultSorted = "Total",
            defaultSortOrder = "desc",
            columns = list(
              Quarterback = colDef(minWidth = 150, footer = "Average"),
              Total = colDef(footer = function(values) sprintf("%.3f", mean(values)), format = colFormat(digits = 3), align = "center",
                             style = function(value) {
                               if (!is.numeric(value)) return()
                               normalized <- (value - min(passers$Total)) / (max(passers$Total) - min(passers$Total))
                               color <- GnYlRd(normalized)
                               list(background = color)
                             }),
              Scrambles = colDef(footer = function(values) sprintf("%.3f", mean(values)), format = colFormat(digits = 3), align = "center",
                                 style = function(value) {
                                   if (!is.numeric(value)) return()
                                   normalized <- (value - min(passers$Scrambles)) / (max(passers$Scrambles) - min(passers$Scrambles))
                                   color <- GnYlRd(normalized)
                                   list(background = color)
                                 }),
              Sacks = colDef(footer = function(values) sprintf("%.3f", mean(values)), format = colFormat(digits = 3), align = "center",
                             style = function(value) {
                               if (!is.numeric(value)) return()
                               normalized <- (value - min(passers$Sacks)) / (max(passers$Sacks) - min(passers$Sacks))
                               color <- GnYlRd(normalized)
                               list(background = color)
                             }),
              Rushes = colDef(footer = function(values) sprintf("%.3f", mean(values)), format = colFormat(digits = 3), align = "center",
                              style = function(value) {
                                if (!is.numeric(value)) return()
                                normalized <- (value - min(passers$Rushes)) / (max(passers$Rushes) - min(passers$Rushes))
                                color <- GnYlRd(normalized)
                                list(background = color)
                              }),
              Passes = colDef(footer = function(values) sprintf("%.3f", mean(values)), format = colFormat(digits = 3), align = "center",
                              style = function(value) {
                                if (!is.numeric(value)) return()
                                normalized <- (value - min(passers$Passes)) / (max(passers$Passes) - min(passers$Passes))
                                color <- GnYlRd(normalized)
                                list(background = color)
                              }),
              Plays = colDef(align = "center"),
              Dropbacks = colDef(align = "center")
            ),
            defaultColDef = colDef(footerStyle = list(fontWeight = "bold")),
            fullWidth = TRUE,
            bordered = TRUE)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel(title = h1("QB EPA Breakdown", align = "center"), windowTitle = "QB EPA Breakdown"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("year", label = "Year:", 
                  choices = list("2021" = 2021, "2020" = 2020, "2019" = 2019, "2018" = 2018, "2017" = 2017,
                                 "2016" = 2016, "2015" = 2015, "2014" = 2014, "2013" = 2013,
                                 "2012" = 2012, "2011" = 2011, "2010" = 2010, "2009" = 2009,
                                 "2008" = 2008, "2007" = 2007, "2006" = 2006, "2005" = 2005,
                                 "2004" = 2004, "2003" = 2003, "2002" = 2002, "2001" = 2001,
                                 "2000" = 2000, "1999" = 1999), 
                  selected = 1),
      numericInput("dropbacks",
                   "Minimum dropbacks:",
                   value = 300),
      sliderInput("weeks",
                  "Weeks:",
                  min = 1,
                  max = 21,
                  value = c(0,21)),
      sliderInput("down",
                  "Down:",
                  min = 1,
                  max = 4,
                  value = c(1,4)),
      sliderInput("distance",
                  "Yards to go:",
                  min = 1,
                  max = 99,
                  value = c(1,99)),
      sliderInput("quarter",
                  "Quarter:",
                  min = 1,
                  max = 5,
                  value = c(1,5)),
      sliderInput("margin",
                  "Score margin (Off-Def):",
                  min = -60,
                  max = 60,
                  value = c(-60,60)),
      submitButton(text = "Submit")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h5("Total EPA on play type divided by total plays; Rows sum to total EPA/play", align = "center"),
      reactableOutput("table")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$table <- renderReactable(
    create_table(input$dropbacks, input$weeks[1], input$weeks[2],
                 input$down[1], input$down[2], input$distance[1], input$distance[2],
                 input$quarter[1], input$quarter[2], input$margin[1], input$margin[2], input$year)
  )
}

# Run the application 
shinyApp(ui = ui, server = server)