#' Database module for AMLR shiny apps
#'
#' Database module for AMLR shiny apps
#'
#' @name mod_database
#'
#' @param id character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#' @param db.name.prod character; name of production database on SWFSC server
#' @param db.name.test character; name of test database on SWFSC server
#' @param remote.valid logical; indicates if the remote (TRUE) or local (FALSE)
#'   database connection should be the default
#' @param col.width integer; column width of column of UI widgets
#' @param db.remote.default character; default remote database to connect to.
#'   To allow developer to specify test database as initial db
#'
#' @export
mod_database_ui <- function(id, db.name.prod, db.name.test, remote.valid,
                            col.width = 5, db.remote.default = "remote_prod") {
  ns <- NS(id)
  
  choices.list <- list("remote_prod", "remote_test", "other")
  stopifnot(db.remote.default %in% choices.list)
  names(choices.list) <- c(
    paste(db.name.prod, "- estrella"),
    paste(db.name.test, "- estrella"),
    "Other"
  )
  local.server <- paste0(Sys.info()[["nodename"]], "\\SQLEXPRESS")
  
  # assemble UI elements
  tagList(
    box(
      title = "Database connection information", status = "warning",
      solidHeader = FALSE, width = col.width, collapsible = TRUE,
      tableOutput(ns("pool_db_conn")),
      tags$br(),
      radioButtons(ns("db_conn"), tags$h5("Select database connection"),
                   choices = choices.list,
                   selected = if_else(remote.valid, db.remote.default, "other")),
      conditionalPanel(
        condition = "input.db_conn == 'other'", ns = ns,
        box(
          width = 12,
          textInput(ns("db_other_server"), tags$h5("SQL Server Name"),
                    value = local.server),
          textInput(ns("db_other_database"), tags$h5("SQL Server Database"),
                    value = db.name.prod),
          checkboxInput(ns("db_other_port_check"), "Specify port number", value = FALSE),
          conditionalPanel(
            condition = "input.db_other_port_check == true", ns = ns,
            numericInput(ns("db_other_port"), tags$h5("Port number"), value = 1443)
          ),
          radioButtons(ns("db_other_conn"), tags$h5("Connection type"),
                       choices = list("Trusted connection" = "trusted",
                                      "User login" = "login"),
                       selected = "trusted"),
          conditionalPanel(
            condition = "input.db_other_conn == 'login'", ns = ns,
            textInput(ns("db_other_uid"), tags$h5("User"), value = "sa"),
            textInput(ns("db_other_pwd"), tags$h5("Password"))
          ),
          actionButton(ns("db_other_action"), "Connect to other database")
        )
      )
    )
  )
}

#' @name mod_database
#'
#' @param pool.remote.prod output of a \code{\link[pool]{dbPool}} call.
#'   A DBI database connection pool connected to the remote PRODUCTION database,
#'   e.g. 'AMLR_PINNIPEDS'
#' @param pool.remote.test output of a \code{\link[pool]{dbPool}} call.
#'   A DBI database connection pool connected to the remote TEST database,
#'   e.g. 'AMLR_PINNIPEDS_Test'
#' @param db.driver character; name of driver used to connect to the databases
#'
#' @details
#' This module allows users to connect to the database of their choice:
#' the provided production database, the provided dev (test) database,
#' or a database specified by the user (e.g., a local database).
#'
#' If the user specifies their own database,
#' then the pool object is created and returned by this module.
#'
#' @returns
#' Returns a reactive of the pool connection specified by the user
#'
#' @export
mod_database_server <- function(id, pool.remote.prod, pool.remote.test, db.driver) {
  moduleServer(
    id,
    function(input, output, session) {
      vals.db <- reactiveValues(
        pool = NULL,
        other = FALSE
      )
      
      encrypt.driver <- "ODBC Driver 18 for SQL Server"
      trusted.connection <- if_else(db.driver == encrypt.driver, "Yes", "TRUE")
      trusted.connection.no <- if_else(db.driver == encrypt.driver, "No", "FALSE")
      
      
      #----------------------------------------------------------------------------
      # Connect to database
      
      db_other_close <- function() {
        if (isTruthy(vals.db$pool) & vals.db$other) {
          if (pool::dbIsValid(vals.db$pool)) pool::poolClose(vals.db$pool)
        }
      }
      
      ### Default databases
      observeEvent(input$db_conn, {
        req(input$db_conn != "other")
        db_other_close()
        
        vals.db$other <- FALSE
        vals.db$pool <- if (input$db_conn == "remote_prod") {
          pool.remote.prod
        } else if (input$db_conn == "remote_test") {
          pool.remote.test
        } else {
          NULL
        }
      })
      
      ### Other database, on button click
      observeEvent(input$db_other_action, {
        db_other_close()
        
        db.args.toadd <- if (input$db_other_conn == "trusted") {
          list(Trusted_Connection = trusted.connection)
        } else if (input$db_other_conn == "login") {
          list(uid = input$db_other_uid, pwd = input$db_other_pwd,
               Trusted_Connection = trusted.connection.no)
        } else {
          stop("invalid input$db_other_conn value")
        }
        
        db.args.list <- c(
          list(
            Driver = db.driver,
            Server = input$db_other_server,
            Database = input$db_other_database,
            port = if (input$db_other_port_check) input$db_other_port else NULL,
            Encrypt = if (db.driver == encrypt.driver) "Optional" else NULL
          ),
          db.args.toadd
        )
        
        vals.db$other <- TRUE
        vals.db$pool <- do.call(amlr_dbPool, purrr::compact(db.args.list))
      })
      
      
      #----------------------------------------------------------------------------
      # Outputs
      
      ### Get and print info about db connection
      output$pool_db_conn <- renderTable({
        validate(
          need(inherits(vals.db$pool, "Pool"),
               paste("The Shiny app was unable to connect to the specified database.",
                     "Are you connected to VPN, and/or have you",
                     "specified the correct connection arguments?"))
        )
        
        db.query <- pool::dbGetQuery(
          req(vals.db$pool), "SELECT @@servername, DB_NAME(), SYSTEM_USER"
        )
        
        data.frame(
          Label = c("Driver", "Server", "Database", "User"),
          Value = unlist(
            c(db.driver, db.query[[1]], db.query[[2]], db.query[[3]])
          )
        )
      })
      
      ### Return values
      return(reactive(vals.db$pool))
    }
  )
}