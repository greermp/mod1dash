## app.R ##
library(shiny)
library(shinydashboard)
library(tidyr)
library(ggplot2)
library(dplyr)
library(readxl)
library(ggthemes)
library(rsconnect)
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody()
)
my_data <- read_excel("OculusData_v5_dash.xlsx", sheet = "df")

# ggplot(my_data, aes(x=Year, y = Value, group=Metric)) + 
#   geom_line(aes(color=Metric))+ labs(title = "Oculus Revenue Decision Analysis") 
# continuous_scale('Year', breaks=1)


#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Facebook Decision Analysis")  
#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Geographic Survey Results", icon = icon("send",lib='glyphicon'), 
             href = "https://rpubs.com/greermp/Regard_by_location")
  )
)


frow1 <- fluidRow(
  valueBoxOutput("value1")
  ,valueBoxOutput("value2")
  ,valueBoxOutput("value3")
)
frow2 <- fluidRow( 
  box(
    title = "Oculus Return on Investment with and without Advertising Revenue"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("two_L", height = "300px")
  )
  ,box(
    title = "Units Sold"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("two_R", height = "300px")
  ) 
)
frow3 <- fluidRow( 
  box(
    title = "Revenues - Units and Software Store"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("three_L", height = "300px")
  )
  ,box(
    title = "Investment in R&D, Acquistions, and P&E"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("three_R", height = "300px")
  ) 
)
# combine the two fluid rows to make the body
body <- dashboardBody(frow1, frow2, frow3)

#completing the ui part with dashboardPage
ui <- dashboardPage(title = 'This is my Page title', header, sidebar, body, skin='blue')