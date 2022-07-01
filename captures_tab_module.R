library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(odbc)
library(dbplyr)

summary <- c("Summary 1: Captures by season and species", "Summary 2: Captures by species in a given season", "raw data")

season_from_date <- function(date) {
  formatted_date <- as_date(date)
  year <- year(formatted_date)
  month <- month(formatted_date)
  return(ifelse(month < 7, paste0(year - 1, "/", substring(as.character(year), 3,4)), paste0(year, "/", substring(as.character(year + 1), 3,4))))
}

captures_tab_ui <- function(id) {
  ns = NS(id)
  tagList(
      fluidRow(
        column(5,radioButtons(ns("summary"), "Choose Summary Option", summary)),
        column(3,uiOutput(ns("season_list")))
      ),
      conditionalPanel(condition = "input.summary != 'raw data'",
                       plotOutput(ns("plot"), width = "700px"), ns = ns),
      DTOutput(ns("summarydatatbl")),
      downloadButton(ns("downloadData"), "Download")
  )
}

captures_tab_server <- function(id, con) {
  moduleServer(
    id, 
    function(input, output, session) {
      captures <- reactive({
        collect(tbl(con(), "captures"))
      })
      season_info <- reactive({
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
          selectInput(session$ns("summary2_season"), "Select Season", season_info()$season_name)
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
  )
}
