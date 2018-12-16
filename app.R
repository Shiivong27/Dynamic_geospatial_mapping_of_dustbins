library(shiny)
library(leaflet)
library(ggplot2)


ui <- fluidPage(
  titlePanel("Smart Waste Management Application", windowTitle = "Smart Waste Management"),
  
  br(),
  
  sidebarLayout(sidebarPanel(h3("App details"), width = 4,
                             helpText("Smart Waste Management Application has been designed to map all Dustbins geo spatially.
                                      This application uses Google API and an underlying layer of the powerful Javascript library
                                      leaflet, which enables us to add layers on dynamic maps. The leaflet package provides an underlying k-means algorithm to
                                      cluster our points on the map.The application also does basic Data Visualization/
                                      Data Analysis to help the end user understand the data and our observations better"),
                             br(),
                             helpText("This app has been designed by Rishabh Singh,
                                      Graduate from Manipal University")),
                
                mainPanel(h2("Geo Spatial mapping of Dustbins"), align = "center",
                          tabsetPanel(type = "pills",
                                      tabPanel("Dustbin Map",leafletOutput("mymap")),
                                      tabPanel("Dustbin Status", dataTableOutput("data")),
                                      tabPanel("Data Analysis I",plotOutput("ggplot1"),textOutput("summary1")),
                                      tabPanel("Data Analysis II",plotOutput("ggplot2"),textOutput("summary2")),
                                      tabPanel("Data Analysis III",plotOutput("ggplot3"),textOutput("summary3")))
                          
                )
                
                
                
                
                
                
                             )
  
  )

server <- function(input, output, session) {
  
  Dustbin <- read.csv("./Data/Dustbin.csv")
  
  LeafTrashIcon <- icons(
    iconUrl = ifelse(Dustbin$Status == "Empty","http://leafletjs.com/examples/custom-icons/leaf-green.png",
                     ifelse(Dustbin$Status == "Full",
                            "http://leafletjs.com/examples/custom-icons/leaf-red.png",
                            "http://leafletjs.com/examples/custom-icons/leaf-orange.png")),
    iconWidth = 20, iconHeight = 50,
    iconAnchorX = 25, iconAnchorY = 50
  )
  
  pal <- colorFactor(
    palette = c("limegreen","red3","darkorange2"),
    domain = as.factor(Dustbin$Status)
    
  )
  
  
  
  output$mymap <- renderLeaflet({
    
    leaflet() %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addMarkers(lng = Dustbin$Longitude, lat = Dustbin$Latitude, 
                 label  = paste(Dustbin$Area,Dustbin$Time_of_Day, sep = " , Time of day: "),
                 icon = LeafTrashIcon,
                 clusterOptions = markerClusterOptions(FreezeAtZoom = 5)) %>%
      addLegend("bottomright", pal = pal, values = Dustbin$Status,
                title = "Dustbin Status", opacity = 1)
  })
  
  output$data <- renderDataTable({
    
    head(Dustbin, n = 200)
    
  })
  
  output$ggplot1 <- renderPlot({
    
    
    
    val <- c("limegreen","red3","darkorange2")
    lab <- c("Empty","Full","Half")
    ggplot(Dustbin, aes(x = factor(Area), fill = factor(Status))) + geom_bar(position = "stack") +
      scale_x_discrete("Area Name") + scale_y_continuous("Status Proportion") +
      scale_fill_manual("Status", values = val, labels = lab)
    
  })
  
  output$ggplot2 <- renderPlot({
    
    val_time <- c("maroon1","maroon2","maroon3","maroon4")
    lab_time <- c("7-10 am","10am-3pm","3-7 pm","7pm-12am")
    
    ggplot(Dustbin, aes(x = factor(Status), fill = factor(Time_of_Day))) + geom_bar(position = "stack") +
      scale_x_discrete("Dustbin Status") + scale_y_continuous("Dustbin Status count") +
      scale_fill_manual("Time of Day", values = val_time, labels = lab_time)
    
    
  })
  
  output$ggplot3 <- renderPlot({
    
    val <- c("limegreen","red3","darkorange2")
    lab <- c("Empty","Full","Half")
    
    ggplot(Dustbin, aes(x = Population, fill = factor(Status))) + geom_bar(position = "stack") +
      scale_x_discrete("Increasing Area Population") + scale_y_continuous("Status count") +
      scale_fill_manual("Status", values = val, labels = lab)
  })
  
  output$summary1 <- renderText({
    
    "This Analysis shows the Area wise split of Dustbin status. One standout observation
    in our analysis is that of Yeshwanthpur. The Dustbins in this area have never been empty
    in our 6-day observation, making it the most polluted area in our study. An interesting
    thing to note is that Yeshwanthpur is an Industrial area, this seems to support our
    observations."
  })
  
  output$summary2 <- renderText({
    
    "This Analysis shows us that most Dustbins are reach the 'Full' status more often in the
    morning than in any other time of the day. The very fact that people dispose off garbage early in the
    morning (as the garbage collector usually conmes in the morning) seems to support this observation"
  })
  
  output$summary3 <- renderText({
    
    "We know that as the number of people living in an area increases, the waste produced by them
    also tends to increase, mostly linearly and in rare cases, exponentially. Our vizualization here
    confirms that."
    
  })
  
  }

shinyApp(ui, server)