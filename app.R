library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(leaflet)
library(googleway)
library(ggmap)
library(plyr)
df_acc <- read.csv("c:/users/harsh/documents/R/comb_df.csv")
idx <-sample(nrow(df_acc),1000, replace = FALSE)
sample_df<- df_acc[idx,]
model <- readRDS("C:/Users/harsh/Documents/R/Project1/rf_model_cutoff_0.6_0.4.rds")
df_acc$Day_of_Week <- mapvalues(df_acc$Day_of_Week, c("1","2","3","4","5","6","7"),c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"), warn_missing = FALSE)
df_acc$Sex_of_Driver <- mapvalues(df_acc$Sex_of_Driver, c("1","2","3"), c("Male","Female","Unknown"))
df_acc$Road_Type <- as.factor(df_acc$Road_Type)
df_acc$Road_Type <- mapvalues(df_acc$Road_Type, c("1","2","3","6","7","9"),c("Roundabout","One way street","Dual carriageway",
                                                                             "Single carriageway","Slip road","Unknown"))

ui <- dashboardPage(
  dashboardHeader(title = "Accidents Analysis and Predictive Modeling"),
  dashboardSidebar(
    sidebarMenu( 
      menuItem(text ="Analysis", tabName = "tab_analysis", icon = icon("clipboard")),
      menuItem(text ="Modeling", tabName = "tab_modeling", icon = icon("code")),
      menuItem(text ="Prediction", tabName = "tab_prediction", icon = icon("database"))
    )),
  
  dashboardBody(
    #create the dashboard body with 3 tabs
    tabItems(
      tabItem(tabName = "tab_analysis", 
                leafletOutput("my_map"),
                selectInput("select_var", "Select Variables", choices = c("Road_Type","X1st_Road_Class", "Day_of_Week", "Time_Hour","Month","Sex_of_Driver" )),
                plotOutput("coolplot"),br(),br(),br()),
      tabItem(tabName = "tab_modeling", h1("Modeling Menu Activated"), textOutput("model_summary")),
      tabItem(tabName = "tab_prediction", 
              textInput("txtPcodeSrc", "Enter Source Address", value = "HA9 8SR"),
              textInput("txtPcodeDest", "Enter Destination Address", value = "NW1 2DB"),
              actionButton("Btn_show_route","Show Route"),
              google_mapOutput("map_route"))
    )
  )
)

server <- function(input, output) {
  register_google( my_key)
  set_key(my_key)
  
  
  #observe teh selction of variables and draw plots
  observe(print(input$select_var))
  output$coolplot <- renderPlot({
    col1 <- sym(input$select_var)
    ggplot(df_acc)+geom_bar(aes(x=!! col1, fill = as.factor(Accident_Severity)))+theme_bw()  +labs(y = "Number of accidents",title= "Accident Severity vs Day of week")
  })
  
  output$model_summary <- renderPrint({
      summary(model)
  })
  #Plot accident data plots on UK map
  output$my_map = renderLeaflet({
    m <- leaflet(data = df_acc) %>% addTiles() %>%
      addMarkers(clusterOptions = markerClusterOptions())
                 #itude,lat = ~Latitude, radius = 0.001, color = "red" , fillColor = df_acc$Accident_Severity)
  })
  
  #Plot the route between starting and the end address
  output$map_route <- renderGoogle_map({google_map(location = geocode('HA9 9SR'),
                                                   zoom = 12)})
  #When clicking the button "Show Route"
  observeEvent(input$Btn_show_route, {
    start_add <- input$txtPcodeSrc 
    
    #Geocoding of start and end addresses
    address <- geocode(start_add, output = "more")
    lon <- as.numeric(address[,1])
    lat <- as.numeric(address[,2])
    lonlat1 <- c(lat,lon)
    end_add <- input$txtPcodeDest 
    address2 <- geocode(end_add, output = "more")
    lon1 <- as.numeric(address2[,1])
    lat1 <- as.numeric(address2[,2])
    lonlat2 <- c(lat1,lon1)
    df1 <- cbind(lonlat1,lonlat2)
    
    #Calculate route between source and destination
    res <- google_directions(origin = lonlat1, destination = lonlat2,mode ="driving")
    df_route <- data.frame(route = res$routes$overview_polyline$points)
    
    #location for markers
    df_way <- cbind(
      res$routes$legs[[1]]$end_location,
      data.frame(address = res$routes$legs[[1]]$end_address)
    )
    df_way1 <- cbind(
      res$routes$legs[[1]]$start_location,
      data.frame(address = res$routes$legs[[1]]$start_address)
    )
    
    #update the map based on text input provided
    google_map_update(map_id = "map_route") %>% 
      clear_traffic() %>%
      clear_polylines() %>%
      clear_markers()%>%
      add_traffic() %>% 
      add_polylines(data = df_route, polyline = "route", stroke_colour = "#FF33D6",stroke_weight = 7,
                    stroke_opacity = 0.7,
                    info_window = "New route",
                    load_interval = 100) %>%
      add_markers(data = df_way, info_window = "end_address") %>%
      add_markers(data = df_way1, info_window = "start_address")
  }) 
  
  
}

shinyApp(ui, server)