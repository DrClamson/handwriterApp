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
  
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = function() {
      paste('report', 
            sep = '.', 
            switch(input$format, 
                   PDF = 'pdf', 
                   HTML = 'html', 
                   Word = 'docx'))
    },
    content = function(file) {
      rmd_name <- switch(input$format,
                         PDF = 'report_pdf.Rmd', 
                         HTML = 'report_html.Rmd', 
                         Word = 'report_word.Rmd')
      src <- normalizePath(rmd_name)
      
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), rmd_name)
      file.copy(src, tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(
        main_dir = global$main_dir,
        analysis = global$analysis,
        known_docs = global$known_docs,
        model = global$model,
        qd_doc = global$doc
      )
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, 
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
}