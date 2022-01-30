#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggridges)
library(ggtext)
library(ggplot2)
library(tidyverse)

passes <- read_csv("passes.csv")

passers <- passes %>%
    select(name) %>%
    unique() %>%
    arrange(name)

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
    titlePanel(title = h1("Air Yard Comparisons"), windowTitle = "Air Yard Comparisons"),
    

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("passer1", label = "Passer 1:", 
                        choices = passers, selected = "T.Brady"),
            selectInput("passer2", label = "Passer 2:", 
                        choices = passers, selected = "P.Manning"),
            sliderInput("seasons",
                        "Seasons:",
                        min = 2006,
                        max = 2021,
                        value = c(2006,2021),
                        sep = "")),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        passer1 = input$passer1
        passer2 = input$passer2
        season_low = input$seasons[1]
        season_high = input$seasons[2]
        
        passes %>%
            filter(name == passer1 | name == passer2, between(season, season_low, season_high)) %>%
            mutate(
                color = case_when(
                    name == passer1 ~ "#D50A0A",
                    TRUE ~ "#04328C"
                )
            ) %>%
            ggplot(aes(x = air_yards, color = color, fill = color)) +
            geom_density(alpha = 0.5, show.legend = FALSE) +
            scale_color_identity(aesthetics = c("fill", "color"))+
            ggthemes::theme_fivethirtyeight() +
            labs(x = "Air yards",
                 y = "Density",
                 caption = "Figure: @Tucker_TnL, Data: @nflfastR",
                 subtitle = paste0("<span style='color:#04328C'>", passer2,
                 "</span> vs. <span style='color:#D50A0A'>", passer1, "</span>, ",
                 season_low, "-", season_high),
                 title = "Air Yard Distribution") +
            ggthemes::theme_fivethirtyeight()+
            theme(
                legend.position = "none",
                plot.title = element_markdown(size = 22, hjust = 0.5),
                plot.subtitle = element_markdown(size = 12, hjust = 0.5),
                axis.title.x = element_text(size = 14),
                axis.title.y = element_text(size = 14),
                axis.text.x = element_text(size = 10),
                axis.text.y = element_text(size = 10)
            ) +
            xlim(-10, 40)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
