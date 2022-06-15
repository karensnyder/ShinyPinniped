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
# To push changes use commit and then push to actually get it onto github

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
            uiOutput("season_list"),
            DTOutput("summarydatatbl"),
            plotOutput("plot", width = "700px")
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
  
  join_captures_pinnipeds <- reactive({
    req(input$load_captures, input$load_season_info, input$load_pinnipeds)
    captures <- mutate(captures(), capture_season = season_from_date(capture_date))
    pinnipeds <- pinnipeds() %>%
      rename(pinniped_id = ID)
    pinnipeds_simple <- pinnipeds %>%
      select(pinniped_id, species)
    captures <- captures %>%
      left_join(pinnipeds_simple)
    captures
  })
  
  #work in progress summary table 1
  DTsummary1 <- reactive({
    req(input$load_captures, input$load_season_info, input$load_pinnipeds)
    #browser()
    captures <- join_captures_pinnipeds()
    captures_by_season <- captures %>%
      group_by(capture_season, species) %>%
      summarize(number_of_captures = n())
    return(captures_by_season)
  })
  
  #reactive for creating summary 2 table, untested because the app won't run right now
  # Can make the first few lines a reactive function to avoid code duplication.
  DTsummary2 <- reactive({
    req(input$load_captures, input$load_season_info, input$load_pinnipeds, input$summary2_season)
    #browser()
    captures <- join_captures_pinnipeds()
    one_season <- captures %>%
      filter(capture_season == input$summary2_season) %>%
      group_by(capture_date, species) %>%
      summarize(number_of_captures = n())
    return(one_season)
  })

  #work in progress creates raw data table, doesn't do anything right now
  DTrawdata <- reactive({
    req(input$load_captures, input$load_season_info, input$load_pinnipeds, input$load_tags)
    #browser()
    pinnipeds <- pinnipeds() %>%
      rename(pinniped_id = ID)
    pinnipeds_simple <- pinnipeds %>%
      select(pinniped_id, species, sex)
    #also need tag unique, not sure what that is
    tags_simple <- tags() %>%
      filter(primary_tag == "TRUE") %>%
      select(tag, tag_type, pinniped_id, tagging_date)
    captures <- captures() %>%
      left_join(pinnipeds_simple) %>%
      left_join(tags_simple)
    return(captures)
  })

  output$season_list <- renderUI({
    req(input$summary, input$load_season_info)
    if (input$summary == "Summary 2: Captures by species in a given season") {
      selectInput("summary2_season", "Select Season", season_info()$season_name)
    }
  })
  
  #work in progress, trying to get the DT tables above to render with the appropriate data
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
  
  summary_plot_reactive <- reactive({
    if(input$summary == "Summary 1: Captures by season and species") {
        return(ggplot(DTsummary1(), aes(x = capture_season, y = number_of_captures, fill = species)) +
          geom_bar(position = "stack", stat = "identity")) +
          theme(axis.text.x = element_text(angle = 90)))
    }
    #make this a point and line plot
    if(input$summary == "Summary 2: Captures by species in a given season") {
        return(ggplot(DTsummary2(), aes(x = capture_date, y = number_of_captures, fill = species)) +
          geom_bar(position = "stack", stat = "identity") +
          theme(axis.text.x = element_text(angle = 90)))
    }
    
  })
  
  output$plot <- renderPlot({
    validate(need(input$summary != "raw data", "no plot for raw data"))
    summary_plot_reactive()
  }, res = 96, width = 700)
}


shinyApp(ui, server)