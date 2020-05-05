library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(leaflet)
library(googleway)
library(ggmap)
library(plyr)
library(randomForest)
library(caret)
library(geosphere)
library(stringi)
library(stringr)

#Read the datasets for using during analysis
df_acc <- read.csv("c:/users/harsh/documents/R/comb_df.csv")
idx <-sample(nrow(df_acc),1000, replace = FALSE)
sample_df<- df_acc[idx,]
model <- readRDS("C:/Users/harsh/Documents/R/Project1/rf_model_cutoff_0.6_0.4.rds")
df_acc$Day_of_Week <- mapvalues(df_acc$Day_of_Week, c("1","2","3","4","5","6","7"),c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"), warn_missing = FALSE)
df_acc$Sex_of_Driver <- mapvalues(df_acc$Sex_of_Driver, c("1","2","3"), c("Male","Female","Unknown"))
df_acc$Road_Type <- as.factor(df_acc$Road_Type)
df_acc$Road_Type <- mapvalues(df_acc$Road_Type, c("1","2","3","6","7","9"),c("Roundabout","One way street","Dual carriageway",
                                                                             "Single carriageway","Slip road","Unknown"))
#register google key

register_google(my_key)
set_key(my_key)

#Load the model to be used
model <- readRDS("C:/Users/harsh/Documents/R/Project1/rf_model_cutoff_0.6_0.4.rds")

#Load the presaved road details from the analysis part
saved_road_details <- readRDS("C:/Users/harsh/Documents/R/Project1/road_details.rds")
combined_df <- readRDS("C:/Users/harsh/Documents/R/Project1/combined_df.rds")

#######################################################################################################

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
      
      tabItem(tabName = "tab_modeling", h1("Comparison plot for model performances"),
              radioButtons("rdo_image","Model comparison plot for ",choices = c("Plain models","Sampled models","2 classes")),
              imageOutput("model_result_image1")),
      
      tabItem(tabName = "tab_prediction", 
              fluidPage(
                fluidRow(title= "Driver and Vehicle details",
                         column(4,numericInput("txt_age","Age of Driver", value = 30)),
                         column(4, selectInput("slct_jrney_prps","Journey Purpose of Driver", 
                                               c("Part of work","Commuting to/from work","School runs by parents","Pupil riding to/from school","Other","Unknown")),
                                selected = "unknown"),
                         column(4,radioButtons("rdo_Sex","Sex of Driver",c("Male","Female","Unknown"))))),
              fluidRow(column(4,numericInput("txt_age_veh","Age of Vehicle", value= 5)),
                       column(4,textInput("txt_veh_cap","Vehicle Capacity", value = 1461))),
              fluidRow(
                column(4,textInput("txtPcodeSrc", "Enter Source Address", value = "HA9 8SR")),
                column(4,textInput("txtPcodeDest", "Enter Destination Address", value = "NW1 2DB")),
                column(4, actionButton("Btn_show_route","Show Route")),
                google_mapOutput("map_route")),
              fluidRow(column(4,actionButton("btn_pred","Predict accident Probability")),
                       column(8,textOutput("acc_prob"))))
    )
  )
)

##########################################################################################################
server <- function(input, output) {
  register_google( my_key)
  set_key(my_key)
  
  
  #observe teh selction of variables and draw plots
  observe(input$select_var)
  output$coolplot <- renderPlot({
    sel<-input$select_var
    col1 <- sym(input$select_var)
    ggplot(df_acc)+geom_bar(aes(x=!! col1, fill = as.factor(Accident_Severity)))+theme_bw()  +labs(y = "Number of accidents",title= paste("Accident Severity vs ",sel))
  })
  
  #observe the radio button for plotting image and display on screen
  observe(input$rdo_image)
  
  output$model_result_image1 <-  renderImage({
    if(input$rdo_image=="Plain models")  #plain models selected
      return(list(
        src = "images/plain.png",
        filetype = "image/png",
        alt = "This is a model plot"
      ))
    if(input$rdo_image == "Sampled models")  #sampled models selected
    return(list(
      src = "images/sampling.png",
      filetype = "image/png",
      alt = "This is a model plot"
    ))
    if(input$rdo_image == "2 classes")    #2 classes models selected
      return(list(
        src = "images/2_class.png",
        filetype = "image/png",
        alt = "This is a model plot"
      ))
  },deleteFile = FALSE)
      
  #Plot accident data plots on UK map
  output$my_map = renderLeaflet({
    m <- leaflet(data = df_acc) %>% addTiles() %>%
      addMarkers(clusterOptions = markerClusterOptions())
                 #itude,lat = ~Latitude, radius = 0.001, color = "red" , fillColor = df_acc$Accident_Severity)
  })
  
  #Plot the route between starting and the end address
  output$map_route <- renderGoogle_map({google_map(location = geocode('HA9 9SR'),
                                                   zoom = 12)})
  
  #When clicked on the button "Show Route"
  observeEvent(input$Btn_show_route, {
    #get addresses from textbox
    start_add <- input$txtPcodeSrc 
    end_add <- input$txtPcodeDest
    
    #Calculate route between source and destination
    res <- google_directions(origin = start_add, destination = end_add,mode ="driving")
    df_route <- data.frame(route = res$routes$overview_polyline$points)
    
    #source and destination locations for markers
    end= res$routes$legs[[1]]$end_location
    start = res$routes$legs[[1]]$start_location
    
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
      add_markers(data = end, info_window = "end_address") %>%
      add_markers(data = start, info_window = "start_address")
    }) 
  
  #When predict accident button pressed
  observeEvent(input$btn_pred,{
    output$acc_prob <- renderText(" ")
    
    #Get the source and destination addresses from text fields and calculate the route
    start_add <- input$txtPcodeSrc 
    end_add <- input$txtPcodeDest
    res <- google_directions(origin = start_add, destination = end_add,mode ="driving")
    
    #Starting and ending locations for the markers and clear previously drawn markers
    end= res$routes$legs[[1]]$end_location
    start = res$routes$legs[[1]]$start_location
    google_map_update(map_id = "map_route") %>% clear_markers() %>% add_markers(data = start)%>% add_markers(data = end)
    
    steps <- access_result(res,"steps")
    steps$end_location[1,]
    df_points <- steps$start_location
    
    #find roads on the locations falling on the route
    nearRoads <- google_nearestRoads(df_points)
    roadDetails <- lapply(nearRoads$snappedPoints$placeId, function(x){
      google_place_details(place_id = x)
    })
    
    ## get the road names travelled on route and store in a data frame
    Road_Names <-lapply(roadDetails, function(x){
      x[['result']][['name']]
    })
    df_for_prediction <- as.data.frame(cbind(Latitude = nearRoads$snappedPoints$location$latitude, Longitude =nearRoads$snappedPoints$location$longitude))
    df_for_prediction$address <- as.data.frame(unlist(Road_Names))
    
    
    #Find major road names like A303, M1 etc and store in a column road_name
    df_for_prediction$road_name<-str_extract(Road_Names, "[A-Z][0-9]+")
    df_for_prediction$road_name[is.na(df_for_prediction$road_name)] <- "U0"
    
    #Rename the columns for convenience
    names(saved_road_details)[names(saved_road_details)=="Latitude"] <- "Lat"
    names(saved_road_details)[names(saved_road_details)=="Longitude"] <- "Long"
    
    
    ###########################################################################
    #Get details from saved road details based on min haversine distance between the location points 
    temp <- merge(df_for_prediction,saved_road_details, by = "road_name", all.x = TRUE)
    temp$dist <- distHaversine(temp[,c("Longitude","Latitude")], temp[,c("Long","Lat")])
    
    temp$key <- paste(temp$road_name,temp$Latitude,temp$Longitude)
    temp <- temp %>% arrange(dist) %>% distinct(key, .keep_all = TRUE)
    
    keep <- c("road_name", "Longitude","Latitude","X1st_Road_Class","Urban_or_Rural_Area","Road_Type", "all_motor_vehicles", "mean_encode")
    temp <- temp[keep]
    
    #Merge the result with data points for prediction
    temp1 <- left_join(df_for_prediction, temp , by=c("Latitude","Longitude","road_name"))
    temp1 <- temp1%>% select(-one_of("address","road_name"))
    
    ##################################################################
    
    #create an additional data frame to preserve categories of variables
    addition_df <- combined_df[1:20,]
    
    #Create an empty data frame for prediction points
    prediction_df <- combined_df[0,]
    prediction_df[nrow(temp1),]<-NA
    for(k in names(temp1))
       {prediction_df[[k]] <- temp1[[k]]}
    
    
    #Extract the current time for predictions
    tim <- Sys.time()
    tim
    
    #exrtract the month and create columns Month_X, Months_Y and save them in prediction df
    Month <- as.factor(strftime(x= as.Date(tim),format = "%B"))
    Month <- as.numeric(mapvalues(Month, c("January","February","March","April","May","June","July","August","September","October","November","December"),c(1,2,3,4,5,6,7,8,9,10,11,12), warn_missing = FALSE))
    prediction_df$Month_X<- sin(2*pi*Month/12)
    prediction_df$Month_Y <- cos(2*pi*Month/12)
    
    #extract the time hour, create time_X,Time_Y and save values in prediction df
    Time_Hour <- as.numeric(format(strptime(x= as.character(tim), format = "%H"),"%H"))
    prediction_df$Time_X <- sin(2*pi*Time_Hour/24)
    prediction_df$Time_Y <- cos(2*pi*Time_Hour/24)
    
    day <-as.factor(weekdays(as.POSIXct(tim)))
    day <- mapvalues(day, c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"),c("1","2","3","4","5","6","7"), warn_missing = FALSE)
    
    #Insert values to all the remaining variables of the prediction data frame
    prediction_df$Day_of_Week <- day
    prediction_df$Number_of_Vehicles <- 2
    prediction_df$Number_of_Casualties <- 1
    prediction_df$Junction_Detail <- 0
    prediction_df$Pedestrian_Crossing.Physical_Facilities <- 0
    
    #extract the values from text boxes and save in respective columns of prediction df
    prps <- input$slct_jrney_prps
    prediction_df$Journey_Purpose_of_Driver <- case_when(prps == "Part of work" ~1,
                                                         prps == "Commuting to/from work" ~2,
                                                         prps == "School runs by parents" ~3,
                                                         prps == "Pupil riding to/from school" ~4,
                                                         prps == "Other" ~5,
                                                         prps == "Unknown"~6)
    prediction_df$Sex_of_Driver <- input$rdo_sex
    prediction_df$Vehicle_Manoeuvre <- 18
    prediction_df$Age_of_Driver <- input$txt_age
    sex <-  input$rdo_Sex
    prediction_df$Sex_of_Driver <- case_when(sex=="Male"~1,
                                             sex=="Female"~2,
                                             TRUE ~3)
    age<-input$txt_age
    prediction_df$Age_Band_of_Driver <- case_when(age >=0 & age<=5 ~1,
                                                  age>=6 & age<=10 ~2,
                                                  age>=11 & age<=15 ~3,
                                                  age>=16 & age<=20 ~4,
                                                  age>=21 & age <=25 ~5,
                                                  age>=26 & age <=35 ~6,
                                                  age>=36 & age <=45~7,
                                                  age>=46 & age <=55 ~8,
                                                  age>=56 & age <=65 ~9,
                                                  age >=66 &age<=75 ~10,
                                                  age>=76 ~11,
                                                  TRUE~-1
    )
    prediction_df$Engine_Capacity_.CC. <- input$txt_veh_cap
    prediction_df$Age_of_Vehicle <- input$txt_age_veh
    ###################################################################################################
    
    ##Add the prediction df with additional data frame and then get rid of it
    #This is done to avoid different levels in column categories
    try <- rbind(addition_df,prediction_df)
    prediction_df <- try[21:nrow(try),]
    
    #predict the accident severity using the saved model
    pred <- predict(model, prediction_df, type = "prob")
    minimum_prob <- min(pred[,1])
    print("min",minimum_prob)
    
    #find high risk data points based on accident probability
    Acc_Severity <- ifelse(pred[,1]>(minimum_prob+0.05), "0", "1")
    prediction_df <- cbind(prediction_df,severity = Acc_Severity)
    high_risk_area <- prediction_df%>% filter(severity=="1")
   
    #Calculate probability percentage of accidents
    perc_prob <- max(pred[,2])*100
    print(perc_prob)
    risk_data <- high_risk_area[,c("Longitude", "Latitude")]
    
    
    #display proability in text area
    output$acc_prob <- renderText({paste("Your average accident probability is ",perc_prob,"%. Watch out for probable accident locations.")})
    
    #show a dialog box containing the information
    showModal(modalDialog(title = "Information", paste("Your accident probability is ",perc_prob,"%. Watch out for High accident probability locations."), easyClose = TRUE))
    
    #update the map with markers at high risk points 
    google_map_update(map_id = "map_route") %>% clear_markers()%>% add_markers(data = end) %>% add_markers(data = start)%>%add_markers(data = risk_data)
   
  })
}

###########################################################################################################
shinyApp(ui, server)