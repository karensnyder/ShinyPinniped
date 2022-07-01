library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(odbc)
library(dbplyr)

database_ui <- function(id, col.width = 5) {
  ns <- NS(id)
  local.server <- "localhost"
  tagList(
    box(
      title = "Database connection information", status = "warning",
      solidHeader = FALSE, width = col.width, collapsible = TRUE,
      tableOutput(ns("db_conn")),
      tags$br()
    ),
    box(
      width = 12,
      textInput(ns("db_other_server"), tags$h5("SQL Server Name"),
                value = local.server),
      textInput(ns("db_other_database"), tags$h5("SQL Server Database"), value = "AMLR_PINNIPEDS"),
      checkboxInput(ns("db_other_port_check"), "Specify port number", value = TRUE),
      conditionalPanel(
        condition = "input.db_other_port_check == true", ns = ns,
        numericInput(ns("db_other_port"), tags$h5("Port number"), value = 1443)
        ),
      textInput(ns("db_other_uid"), tags$h5("User"), value = "sa"),
      textInput(ns("db_other_pwd"), tags$h5("Password")),
      actionButton(ns("db_other_action"), "Connect to other database")
    )
  )
}

database_server <- function(id, db.driver) {
  moduleServer(
    id,
    function(input,output,session) {
      vals.db <- reactiveValues(
        con = NULL
      )
      
      #encrypt.driver <- "ODBC Driver 18 for SQL Server"
      
      
      #----------------------------------------------------------------------------
      # Connect to database
      
      db_other_close <- function() {
        if (isTruthy(vals.db$con)) {
          if (dbIsValid(vals.db$con)) 
            dbDisconnect(vals.db$con)
        }
      }
    ### Other database, on button click
    observeEvent(input$db_other_action, {
      db.args.list <- c(odbc(), list(
          Driver = db.driver,
          Server = input$db_other_server,
          Database = input$db_other_database,
          uid = input$db_other_uid,
          pwd = input$db_other_pwd,
          port = if (input$db_other_port_check) input$db_other_port else NULL,
          Encrypt =  "Optional"
        ))
      db_other_close()
      vals.db$con <- try(do.call(dbConnect, purrr::compact(db.args.list)), silent = TRUE)
      # validate(need((vals.db$con), "Failed to connect to Database"))
      # validate(need(dbIsValid(vals.db$con), "Database Connection is invalid"))
    })
    
    ### Get and print info about db connection
    output$db_conn <- renderTable({
      validate(
        need(inherits(vals.db$con, "Microsoft SQL Server"),
             paste("The Shiny app was unable to connect to the specified database.",
                   "Are you connected to VPN, and/or have you",
                   "specified the correct connection arguments?"))
      )
      
      db.query <- dbGetQuery(
        req(vals.db$con), "SELECT @@servername, DB_NAME(), SYSTEM_USER"
      )
      
      data.frame(
        Label = c("Driver", "Server", "Database", "User"),
        Value = unlist(
          c(db.driver, db.query[[1]], db.query[[2]], db.query[[3]])
        )
      )
    })
    ### Return values
    return(reactive(vals.db$con))
    } 
  )
}
