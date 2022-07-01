library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(odbc)
library(dbplyr)
source("database_con_module.R")
source("captures_tab_module.R")

ui <- dashboardPage(
    dashboardHeader(title = "Pinniped Dashboard"),
      dashboardSidebar(sidebarMenu(
                        menuItem("Connect to Database", tabName = "database", icon = icon("dashboard")),
                        menuItem("Captures", tabName = "captures", icon = icon("th"))
                      )
      ),
      dashboardBody(
          tabItems(
            tabItem(tabName = "database",
                    database_ui("database_tab")),
            tabItem( tabName = "captures",
              captures_tab_ui("captures_tab"))
          )
      )
)


server <- function(input,output,session) {
  con <- database_server("database_tab", "ODBC Driver 18 for SQL Server")
  captures_tab_server("captures_tab", con)
}

shinyApp(ui, server)
