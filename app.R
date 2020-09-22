#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(leaflet)
library(rgdal)
library(dplyr)
library(DT)
library(readxl)
library(data.table)
library(htmltools)
library(choroplethrMaps)
library(choroplethr)
library(ggplot2)


data(country.map, package = "choroplethrMaps")
filtered_data <- read_excel("Copy of filtered.xlsx")
setnames(filtered_data, old = c('Project Name','Primary Sector','Sub-Sector','Specific Reason.1','Updated Date'), new = c('Name','Primary','Sub','SpecificReason','Date'))

# Trying to replace NA values of long,lat...아직 성공못함
grouping <- country.map %>%
    group_by(region) %>% 
    summarise_at(vars("lat", "long"), mean)
setnames(grouping, old = c('region'), new = c('Country'))
exp <- filtered_data %>%
    mutate(Country = tolower(Country)) %>%
    mutate(Country = recode(Country,
                           "united states"    = "united states of america",
                           "congo, dem. rep." = "democratic republic of the congo",
                           "congo, rep."      = "republic of congo",
                           "korea, dem. rep." = "south korea",
                           "korea. rep."      = "north korea",
                           "tanzania"         = "united republic of tanzania",
                           "serbia"           = "republic of serbia",
                           "slovak republic"  = "slovakia",
                           "yemen, rep."      = "yemen"))

abc <- filtered_data %>%
    group_by(Status)

# CHOROPLETH MAPPING
new <- filtered_data %>% 
    group_by(Country) %>% 
    count()

plotdata <- new %>%
    #filter(year == 2007) %>%
    rename(region = Country,
           value = n) %>%
    mutate(region = tolower(region)) %>%
    mutate(region = recode(region,
                           "united states"    = "united states of america",
                           "congo, dem. rep." = "democratic republic of the congo",
                           "congo, rep."      = "republic of congo",
                           "korea, dem. rep." = "south korea",
                           "korea. rep."      = "north korea",
                           "tanzania"         = "united republic of tanzania",
                           "serbia"           = "republic of serbia",
                           "slovak republic"  = "slovakia",
                           "yemen, rep."      = "yemen"))
country_choropleth(plotdata)

#ggplot2 -- making it visualizing more appealing
country_choropleth(plotdata,
                   num_colors=9) +
    scale_fill_brewer(palette="YlOrRd") +
    labs(title = "Number of PPIs by country",
         subtitle = "Valid PPIs",
         caption = "World Bank",
         fill = "Number",
         )

labeling <- country.map %>%
    group_by(region) %>% 
    summarise_at(vars("economy", "income_grp"), first)

bins <- c(0, 1, 2, 4, 13, 23)
pal <- colorBin("YlOrRd", domain = new$n, bins = bins)

labels <- paste(
    "Country: ", plotdata$region,"<br/>", 
    "Economy: ", labeling$economy, "<br/>", 
    "Income grp: ", labeling$income_grp, 
    sep="") %>%
    lapply(htmltools::HTML)


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

shinyApp(ui = ui, server = server)
