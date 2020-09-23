#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(readxl)
library(data.table)
library(htmltools)
library(choroplethrMaps)
library(choroplethr)


data(country.map, package = "choroplethrMaps")
filtered_data <- read_excel("Copy of filtered.xlsx")
setnames(filtered_data, old = c('Project Name','Primary Sector','Sub-Sector','Specific Reason.1','Updated Date'), new = c('Name','Primary','Sub','SpecificReason','Date'))

# LEAFLET MAPPING
mapping <- #filtered_data %>%
    leaflet(filtered_data) %>%
    addTiles() %>%
    addCircleMarkers(data = filtered_data, 
                     lng = ~Longitude, 
                     lat = ~Latitude, 
                     clusterOptions = markerClusterOptions(),
                     stroke = FALSE, fillOpacity = 0.5,
                     label = ~htmlEscape(Name),
                     popup = ~paste("<b>Project Name:</b>",Name,"<br>",
                                    "<b>Country:</b>",Country,"<br>",
                                    "<b>Region:</b>",Region,"<br>",
                                    "<b>Status:</b>",Status,"<br>",
                                    "<b>Primary Sector:</b>", Primary,"<br>",
                                    "<b>Sub Sector:</b>",Sub,"<br>",
                                    "<b>Problem:</b>",Problem,"<br>",
                                    "<b>Specific Reason:</b>",SpecificReason,"<br>",
                                    "<b>Link:</b>",Link,"<br>",
                                    sep=" "))
# Active, delay, cancel, operation
# 성공
ui <- shinyUI(fluidPage(
    titlePanel(title = "PPI Mapping"),
    sidebarLayout(
        sidebarPanel(selectInput(inputId = "Status",
                                 label = "Choose the type of status you want to see:",
                                 choices = list("Entire" = "entire",
                                                "Delayed" = "delayed", 
                                                "Resumed" = "resume",
                                                "Paused" = "Paused"))),
        mainPanel()),
    leafletOutput("mapping",height = "600")
)
)

server <- shinyServer(
    function(input, output, session) {
        output$mapping <- renderLeaflet({
            
            if(input$Status=="entire"){x <- filtered_data}
            if(input$Status=="delayed"){x <- filter(filtered_data, grepl('delay',Status))}
            if(input$Status=="resume"){x <- filter(filtered_data, grepl('resume',Status))}
            if(input$Status=="Paused"){x <- filter(filtered_data, grepl('Pause',Status))}
            
            leaflet(x) %>%
                addTiles() %>%
                addCircleMarkers(data = x, 
                                 lng = ~Longitude, 
                                 lat = ~Latitude, 
                                 clusterOptions = markerClusterOptions(),
                                 stroke = FALSE, fillOpacity = 0.5,
                                 label = ~htmlEscape(Name),
                                 popup = ~paste("<b>Project Name:</b>",Name,"<br>",
                                                "<b>Country:</b>",Country,"<br>",
                                                "<b>Region:</b>",Region,"<br>",
                                                "<b>Status:</b>",Status,"<br>",
                                                "<b>Primary Sector:</b>", Primary,"<br>",
                                                "<b>Sub Sector:</b>",Sub,"<br>",
                                                "<b>Problem:</b>",Problem,"<br>",
                                                "<b>Specific Reason:</b>",SpecificReason,"<br>",
                                                "<b>Link:</b>",Link,"<br>",
                                                sep=" "))
        })
    }
)
