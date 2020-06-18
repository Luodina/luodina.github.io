## app.R ##
library(shinydashboard)
library(DT)
library(ggplot2)
library(leaflet)

source("test.R")

library(tidyverse)
library(gganimate)

ui <- dashboardPage(
    dashboardHeader(title = "Basic dashboard"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("Map", tabName = "map", icon = icon("th")),
            menuItem("Data", tabName = "data", icon = icon("database"))
        )
    ),
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "dashboard",
                    fluidRow(
                        box(width=12, plotOutput("bar1")),
                        
                    ),
                    fluidRow(
                        box(width=6, plotOutput("pie")),
                        box(width=6, plotOutput("bar2"))
                    ),
                    fluidRow(
                        box(width=6, plotOutput("plot1")),
                        box(width=6, sliderInput("slider", "Slider:", 1,100,1))
                    )
            ),
            
            # Second tab content
            tabItem(tabName = "map",
                fluidPage(
                    leafletOutput("mymap"),

                    selectizeInput("selected1",
                                   "Select Item to Display",
                                   unique(listings$neighbourhood)),

                    selectizeInput("selected2",
                                   "Select Item to Display",
                                   unique(listings$room_type))
                    #p(),
                    #actionButton("recalc", "New points")
                )
            ),
            
            # Third tab content
            tabItem(tabName = "data",
                    fluidRow(
                        box(width=3, checkboxGroupInput("show_vars", "Columns in diamonds to show:",
                                                        names(listings), selected = names(listings))),
                        box(width=9, dataTableOutput("table1"))    
                    )
                    
            )
            
        )
    )
)

server <- function(input, output) {
    output$plot1 <- renderPlot({
        data <- histdata[seq_len(input$slider)]
        hist(data)
    })
    
    output$bar1 <-renderPlot({
        ggplot(listings,aes(neighbourhood)) + geom_bar(aes(fill=room_type))
    })
    
    output$mymap <- renderLeaflet({
        leaflet(data = listings[listings$neighbourhood == input$selected1
                                &listings$room_type == input$selected2,]
                ) %>% addTiles() %>%
            addMarkers(~longitude, ~latitude, popup = ~as.character(room_type), label = ~as.character(price))
    })
    
    output$table1 <- DT::renderDataTable({datatable(listings[, input$show_vars, drop = FALSE],
                                                    options = list(
                                                        scrollX = TRUE,
                                                        class = 'cell-border stripe'
                                                    )
                                                    )
                                        })
}

shinyApp(ui, server)