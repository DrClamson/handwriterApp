## Load Libraries
library(shiny)
library(shinyjs)
library(shinyBS)
library(ggplot2)
library(bslib)
library(bsicons)
library(shinycssloaders)
library(randomForest)
library(dplyr)
library(DT)

## Load handwriter
library(handwriter)

## Config
options(shiny.maxRequestSize = 30*1024^2)
addResourcePath("images", "images")


# SERVER ------------------------------------------------------------------
server <- function(input, output, session) {
  
  # NEXT BUTTONS ----
  # disable next buttons at start
  shinyjs::disable("setup_next_button")
  shinyjs::disable("known_next_button")
  shinyjs::disable("qd_next_button")
  
  # enable next buttons
  observe({
    # main_dir needs to be defined
    req(global$main_dir)
    shinyjs::enable("setup_next_button")
  })
  observe({
    # model needs to be loaded
    req(global$model)
    shinyjs::enable("known_next_button")
  })
  observe({
    # analysis needs to be loaded
    req(global$analysis)
    shinyjs::enable("qd_next_button")
  })
  
  # change selected tab in main panel
  observeEvent(input$begin_button, {updateTabsetPanel(session, "prevreport", selected = "Setup")})
  observeEvent(input$setup_next_button, {updateTabsetPanel(session, "prevreport", selected = "Known Writing")})
  observeEvent(input$known_next_button, {updateTabsetPanel(session, "prevreport", selected = "Questioned Document")})
  observeEvent(input$qd_next_button, {updateTabsetPanel(session, "prevreport", selected = "Report")})
  
  # STORAGE ----
  global <- reactiveValues(
    analysis = NULL,
    known_docs = NULL,
    main_dir = NULL,
    model = NULL,
    qd_image = NULL,
    qd_name = NULL,
    qd_path = NULL,
  )
  
  # MAIN DIRECTORY ----
  maindirServer('maindir1', global)

  # KNOWN WRITING ----
  knownServer('known1', global)
  
  # QUESTIONED DOCUMENT ----
  qdServer('qd1', global)
  
  # REPORT ----
  reportServer('report1', global)
  
}