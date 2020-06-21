## app.R ##
library(shinydashboard)
library(DT)
library(ggplot2)
library(leaflet)

source("global.R")

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
                        box(width=12, plotOutput("bar1"))
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
                    fluidRow(
                        box(width=12,height = 500, leafletOutput("mymap")),
                        box(width=12, textOutput("text1")
                    ),
                    fluidRow(
                        box(width=6, selectizeInput("selected1",
                                                    "Select Item to Display",
                                                    unique(listings$neighbourhood))
                            ),
                        box(width=6, sliderInput("sliderPrice", "Slider:", min(listings$price),max(listings$price),value =c(100, 500)))
                    ),
                    fluidRow(
                        box(width=6,  selectizeInput("selected2",
                                                     "Select Item to Display",
                                                     unique(listings$room_type))
                            )
                        )
                        
                    )
                )
            ),
            
            # Third tab content
            tabItem(tabName = "data",
                    fluidRow(
                        box(width=3, checkboxGroupInput("show_vars", "AirBnb params:",
                                                        names(listings), selected = names(listings))),
                        box(width=9, dataTableOutput("table1"))    
                    )
                    
            )
            
        )
    )
)

server <- function(input, output) {
    listingHK <- reactive({
        selNeighbourhood = input$selected1
        selRoomType = input$selected2
        startPrice = input$sliderPrice[1]
        endPrice = input$sliderPrice[2]
        listings %>%
            select(neighbourhood,room_type, longitude,latitude,price) %>%
            filter(neighbourhood == selNeighbourhood  
                   & room_type == selRoomType
                   & price >= startPrice
                   & price <= endPrice)
        
    })
    
    mapListingsHk <- reactive({
        num <- nrow(listingHK())
        print(num)
        if (num==0){
            #default map - first 20 records
            listings[1:20,]
        }else {
            listingHK()
        }    
    })
    
    output$plot1 <- renderPlot({
        data <- histdata[seq_len(input$slider)]
        hist(data)
    })
    
    output$text1 <-renderText({
        num <- nrow(listingHK())
        if (num==0){
            "ERROR!!!"
        }else {
            "here we are" 
        }  
    })
    
    output$bar1 <-renderPlot({
        ggplot(listings,aes(neighbourhood)) + geom_bar(aes(fill=room_type))
    })
    
    output$mymap <- renderLeaflet({
        leaflet(data = mapListingsHk()) %>% addTiles() %>%
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