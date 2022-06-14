library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(lubridate)
library(ggplot2)

#For the tags table, there's a column called primary tag, only use the ones where it is true because otherwise there could be duplicates.
#Check out browser function for debugging
# curly braces for any multi line reactive
# Try a workflow with the three table options where there is one output table in the ui, and then the output is switched between the three tables
#     with a reactive function
# lookup the argument in the file widgets that controls size

#Global variables
summary <- c("Summary 1: Captures by season and species", "Summary 2: Captures by species in a given season", "raw data")
season_from_date <- function(date) {
  formatted_date <- as_date(date)
  year <- year(formatted_date)
  month <- month(formatted_date)
  if(month < 7) {
    return(paste0(year - 1, "/", substring(as.character(year), 3,4)))
  }
  else {
    return(paste0(year, "/", substring(as.character(year + 1), 3,4)))
  }
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
      fluidRow(
        #These aren't in a row?
        column(4,fileInput("load_captures", label = h3("Load Captures csv File"), accept = ".csv")),
        column(4,fileInput("load_season_info", label = h3("Load Season Info csv File"), accept = ".csv")),
        column(4,fileInput("Load_beaches", label = h3("Load Beaches csv File"), accept = ".csv")),
        column(3,fileInput("load_tags", label = h3("Load Tags csv File"), accept = ".csv")),
        column(4,fileInput("load_pinnipeds", label = h3("Load Pinnipeds csv File"), accept = ".csv"))
        
      ),
      #data table
      DTOutput('datatbl')
    ),
    
    # Second tab content
    tabItem(tabName = "captures",
            h2("Captures tab content"),
            radioButtons("summary", "Choose Summary Option", summary),
            DTOutput("summarydatatbl"),
            plotOutput("plot", width = "500px")
    )
  )
  )
)

# server controls backend, manages outputs
server <- function(input, output, session) {
  captures <- reactive({
    read.csv(req(input$load_captures$datapath))
  })
  season_info <- reactive({
    read.csv(req(input$load_season_info$datapath))
  })
  pinnipeds <- reactive({
    read.csv(req(input$load_pinnipeds$datapath))
  })
  beaches <- reactive({
    read.csv(req(input$load_beaches$datapath))
  })
  tags <- reactive({
    read.csv(req(input$load_tags$datatpath))
  })
  output$datatbl <- renderDT(
    season_info(), options = list(lengthChange = FALSE)
  )
  
  # #work in progress summary table 1
  # DTsummary1 <- reactive({
  #   req(input$load_captures, input$load_season_info, input$load_pinnipeds)
  #   captures <- captures()
  #   captures <- mutate(captures, capture_season = season_from_date(capture_date))
  #   pinnipeds <- pinnipeds() %>%
  #     rename(pinniped_id = ID)
  #   pinnipeds_simple <- pinnipeds %>%
  #     select(pinniped_id, species)
  #   captures <- captures %>%
  #     left_join(pinnipeds_simple)
  #   captures_by_season <- captures %>%
  #     group_by(capture_season, species) %>%
  #     summarize(number_of_captures = n())
  #   return(captures_by_season)
  # })
  # 
  # #reactive for rendering summary 1 table
  # # !! problem, no idea what it is though.
  # renderDTsummary1 <- renderDT(
  #   DTsummary1(), options = list(lengthChange = FALSE)
  # )
  # 
  # #reactive for creating summary 2 table, untested because the app won't run right now
  # DTsummary2 <- reactive({
  #   req(input$load_captures, input$load_season_info, input$load_pinnipeds, input$summary2_season)
  #   captures <- mutate(captures, capture_season = season_from_date(capture_date))
  #   pinnipeds <- pinnipeds() %>%
  #     rename(pinniped_id = ID)
  #   pinnipeds_simple <- pinnipeds %>%
  #     select(pinniped_id, species)
  #   captures <- captures %>%
  #     left_join(pinnipeds_simple)
  #   one_season <- captures %>%
  #     filter(capture_season == input$summary2_season) %>%
  #     group_by(capture_date, species) %>%
  #     summarize(number_of_captures = n())
  #   return(one_season)
  # })
  # 
  # #reactive for rendering the summary 2 table
  # renderDTsummary2 <- renderDT(
  #   DTsummary2(), options = list(lengthChange = FALSE)
  # )
  # 
  # #work in progress creates raw data table, doesn't do anything right now
  # DTrawdata <- reactive({
  #   req(input$load_captures, input$load_season_info, input$load_pinnipeds)
  #   return(captures())
  # })
  # 
  # #reactive function for rendering the raw data table
  # renderDTrawdata <- renderDT(
  #   DTrawdata(), options = list(lengthChange = FALSE)
  # )
  # 
  # #work in progress, trying to get the DT tbles above to render with the appropriate data
  # observeEvent(input$summary, {
  #   if(input$summary == "Summary 1: Captures by season and species") {
  #     #weird argument name missing error happens here. Tried putting the breakpoint inside renderDTsummary1 and it didn't work.
  #     output$summarydatatbl <- renderDTsummary1()
  #     output$plot <- renderPlot({
  #       ggplot(DTsummary1(), aes(x = capture_season, y = number_of_captures, color = species)) +
  #         geom_bar()
  #       }, res = 96)
  #   }
  #   if(input$summary == "Summary 2: Captures by species in a given season") {
  #     selectInput("summary2_season", "Select Season", season_info()$season_name)
  #     output$summarydatatbl <- renderDTsummary2()
  #     output$plot <- renderPlot({plot(DTsummary2())}, res = 96)
  #   }
  #   if (input$summary == "raw data") {
  #     output$summarydatatbl <- renderDTrawdata()
  #     output$plot <- renderPlot({plot(DTrawdata())}, res = 96)
  #   }
  # })
  
}


shinyApp(ui, server)