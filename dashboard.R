library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(odbc)
library(dbplyr)

# Find the download table widget and make the tables downloadable.
# Use a conditional panel to make the raw data option look nicer
# Read the dplyr documentation for SQl servers
# Try to get the sql application working
# Read the tamatoa readmes
# To push changes use commit and then push to actually get it onto github
# use shiny:: onStop to define commands to execute when the app closes



#Global variables
summary <- c("Summary 1: Captures by season and species", "Summary 2: Captures by species in a given season", "raw data")
season_from_date <- function(date) {
  formatted_date <- as_date(date)
  year <- year(formatted_date)
  month <- month(formatted_date)
  return(ifelse(month < 7, paste0(year - 1, "/", substring(as.character(year), 3,4)), paste0(year, "/", substring(as.character(year + 1), 3,4))))
}
# ui sets up graphics
ui <- dashboardPage(
  dashboardHeader(title = "Pinniped Dashboard"),
  dashboardSidebar(sidebarMenu(
    menuItem("Data and Season Info", tabName = "season", icon = icon("dashboard")),
    menuItem("Captures", tabName = "captures", icon = icon("th"))
  )
  ),
  dashboardBody(tabItems( 
    # First tab content
    tabItem(
      tabName = "season",
      #Sets up all the widgets to load csv files
      # fluidRow(
      #   column(4,fileInput("load_captures", label = h3("Load Captures csv File"), accept = ".csv")),
      #   column(4,fileInput("load_season_info", label = h3("Load Season Info csv File"), accept = ".csv")),
      #   column(4,fileInput("Load_beaches", label = h3("Load Beaches csv File"), accept = ".csv")),
      #   column(3,fileInput("load_tags", label = h3("Load Tags csv File"), accept = ".csv")),
      #   column(4,fileInput("load_pinnipeds", label = h3("Load Pinnipeds csv File"), accept = ".csv"))
      #   
      # ),
      #data table of season info
      DTOutput('datatbl')
    ),
    
    # Second tab content
    tabItem(
      tabName = "captures",
      fluidRow(
        column(5,radioButtons("summary", "Choose Summary Option", summary)),
        column(3,uiOutput("season_list"))
      ),
      conditionalPanel(condition = "input.summary != 'raw data'",
                       plotOutput("plot", width = "700px")),
      DTOutput("summarydatatbl"),
      downloadButton("downloadData", "Download")
      
    )
  )
  )
)

# server controls backend, manages outputs
server <- function(input, output, session) {
  con <- reactive({
    db_con <- try(dbConnect(odbc(), Driver = "ODBC Driver 18 for SQL Server", 
                     Server = "localhost", 
                     Database = "AMLR_PINNIPEDS",
                     uid = "sa", 
                     pwd = "Pinnipedsq1LocalAccess", 
                     port = 1433, 
                     Encrypt = "Optional"), silent = TRUE)
    validate(need((db_con), "Failed to connect to Database"))
    validate(need(dbIsValid(db_con), "Database Connection is invalid"))
    db_con
  })
  captures <- reactive({
    collect(tbl(con(), "captures"))
    #read.csv(req(input$load_captures$datapath))
  })
  season_info <- reactive({
    #browser()
    tbl(con(), "season_info") %>%
      select(-ts) %>%
      collect()
  })
  pinnipeds <- reactive({
    tbl(con(), "pinnipeds") %>%
      select(-ts) %>%
      collect()
  })
  beaches <- reactive({
    collect(tbl(con(), "beaches"))
  })
  tags <- reactive({
    collect(tbl(con(), "tags"))
  })
  output$datatbl <- renderDT({
    season_info()
    }, options = list(lengthChange = FALSE)
  )
  
  #joins the captures and pinnipeds tables together
  join_captures_pinnipeds <- reactive({
    req()
    captures <- mutate(captures(), capture_season = season_from_date(capture_date))
    pinnipeds <- pinnipeds() %>%
      rename(pinniped_id = ID)
    pinnipeds_simple <- pinnipeds %>%
      select(pinniped_id, species)
    captures <- captures %>%
      left_join(pinnipeds_simple)
    captures
  })
  
  #summary table 1
  DTsummary1 <- reactive({
    req()
    captures <- join_captures_pinnipeds()
    captures_by_season <- captures %>%
      group_by(capture_season, species) %>%
      summarize(number_of_captures = n())
    return(captures_by_season)
  })
  
  #reactive for creating summary 2 table
  DTsummary2 <- reactive({
    req(input$summary2_season)
    captures <- join_captures_pinnipeds()
    one_season <- captures %>%
      filter(capture_season == input$summary2_season) %>%
      group_by(capture_date, species) %>%
      summarize(number_of_captures = n())
    return(one_season)
  })
  
  #creates raw data table
  DTrawdata <- reactive({
    req()
    pinnipeds <- pinnipeds() %>%
      rename(pinniped_id = ID)
    pinnipeds_simple <- pinnipeds %>%
      select(pinniped_id, species, sex)
    tags_simple <- tags() %>%
      filter(primary_tag == "TRUE") %>%
      select(tag, tag_type, pinniped_id, tagging_date)
    captures <- captures() %>%
      left_join(pinnipeds_simple) %>%
      left_join(tags_simple)
    return(captures)
  })
  
  output$season_list <- renderUI({
    req(input$summary)
    if (input$summary == "Summary 2: Captures by species in a given season") {
      selectInput("summary2_season", "Select Season", season_info()$season_name)
    }
  })
  
  #DT tables above render with the appropriate data
  summary_table_reactive <- reactive({
    if(input$summary == "Summary 1: Captures by season and species") {
      return(DTsummary1())
    }
    if(input$summary == "Summary 2: Captures by species in a given season") {
      return(DTsummary2())
    }
    if (input$summary == "raw data") {
      return(DTrawdata())
    }
  })
  
  output$summarydatatbl <- renderDT({
    summary_table_reactive()}, options = list(lengthChange = FALSE)
  )
  
  #Creates the plots of captures by season and date
  summary_plot_reactive <- reactive({
    if(input$summary == "Summary 1: Captures by season and species") {
      return(ggplot(DTsummary1(), aes(x = capture_season, y = number_of_captures, color = species, group = species)) +
               geom_point(position = "identity", stat = "identity") +
               geom_line(position = "identity", stat = "identity") +
               theme(axis.text.x = element_text(angle = 90)))
    }
    if(input$summary == "Summary 2: Captures by species in a given season") {
      return(ggplot(DTsummary2(), aes(x = capture_date, y = number_of_captures, color = species, group = species)) +
               geom_point(position = "identity", stat = "identity") +
               geom_line(position = "identity", stat = "identity") +
               theme(axis.text.x = element_text(angle = 90)))
    }
    
  })
  
  output$plot <- renderPlot({
    validate(need(input$summary != "raw data", "no plot for raw data"))
    summary_plot_reactive()
  }, res = 96)
  
  #controls widget to download the tables
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$summarydatatbl, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(summary_table_reactive(), file, row.names = FALSE)
    }
  )
}


shinyApp(ui, server)
#dbDisconnect(con)