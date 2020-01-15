library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(leaflet)
df_acc <- read.csv("c:/users/harsh/documents/project resources/datasets for accidents/accidents/accidents_2015.csv")
sample_df <- df_acc[c(1:1000),]

ui <- dashboardPage(
  dashboardHeader(title = "Accidents Analysis and Predictive Modeling"),
  dashboardSidebar(
                  sidebarMenu( 
                               menuItem(text ="Analysis", tabName = "tab_analysis", icon = icon("clipboard")),
                               menuItem(text ="Modeling", tabName = "tab_modeling", icon = icon("code")),
                               menuItem(text ="Prediction", tabName = "tab_prediction", icon = icon("database"))
                               )),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "tab_analysis", 
              selectInput("select_var", "Select Variables", choices = c("Road_Type","X1st_Road_Class", "Light_Conditions" )),
              plotOutput("coolplot"),
              br(),br(),
              checkboxGroupInput("years","Select the year of crashes", choices = c("2015","2016","2017")),
              leafletOutput("my_map")),
      tabItem(tabName = "tab_modeling", h1("Modeling Menu Activated")),
      tabItem(tabName = "tab_prediction", 
              textInput("txtPcodeSrc", "Enter Source Postcode", value = ""),
              textInput("txtPcodeDest", "Enter Destination Postcode", value = ""),
              actionButton("Btn_show_route","Show Route"))
      )
  )
)

server <- function(input, output) {
 observe(print(input$select_var))
  output$coolplot <- renderPlot({
    col1 <- sym(input$select_var)
    ggplot(df_acc)+geom_bar(aes(x=!! col1, fill = as.factor(Accident_Severity)))+theme_bw()  +labs(y = "Number of accidents",title= "Accident Severity vs Day of week")
  })
  output$my_map = renderLeaflet({
    m <- leaflet(data = df_acc) %>% addTiles() %>%
    addCircleMarkers(lng = ~Longitude,lat = ~Latitude, radius = 0.01, color = "red" , fillColor = df_acc$Accident_Severity)
    m
    #ggmap(map1) +geom_point(aes(x= Longitude, y = Latitude), data = df_acc, color = "red", size = 0.5)
    
  })
  observeEvent(input$Btn_show_route, {
    showModal(modalDialog(title = "Oh no!!",paste0('Thank you for clicking')))
  })
  
}

shinyApp(ui, server)