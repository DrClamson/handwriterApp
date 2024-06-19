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
  knownSidebarServer('known1', global)
  
  # QUESTIONED DOCUMENT ----
  
  # load QD image for display and copy to temp dir > data > questioned_docs
  observeEvent(input$qd_upload, {
    global$qd_path <- input$qd_upload$datapath
    global$qd_name <- input$qd_upload$name
    
    global$qd_image <- NULL
    global$qd_image <- load_qd(global$qd_path)
  })
  
  output$qd_image <- renderImage({
    tmp <- global$qd_image %>%
      image_write(tempfile(fileext='png'), format = 'png')
    
    # return a list
    list(src = tmp, contentType = "image/png")
  }, deleteFile = FALSE)
  
  output$qd_nodes <- renderPlot({
    handwriter::plotNodes(global$doc, nodeSize = 2)
  })
  
  output$qd_profile <- renderPlot({
    handwriter::plot_cluster_fill_counts(global$analysis, facet=FALSE)
  })
  
  output$qd_analysis <- renderTable({
    make_posteriors_df(global$analysis)
  })
  
  # UI to display QD and plots
  output$qd_display <- renderUI({
    if(!is.null(global$analysis)) {
      # load processed question doc for report
      global$doc <- load_processed_qd(global$main_dir)
      
      # display QD image, graphs plot, and clusters plot
      bsCollapse(id = "qd_display",
                 bsCollapsePanel("Preview", 
                                 card(
                                   card_header(class = "bg-dark", ""),
                                   # max_height = 300,
                                   full_screen = TRUE,
                                   imageOutput("qd_image"))
                 ),
                 bsCollapsePanel("Processed", 
                                 p(class = "text-muted", "The handwriting in the questioned document is split into 
                                          component shapes called graphs."),
                                 plotOutput("qd_nodes")),
                 bsCollapsePanel("Writer profile", plotOutput("qd_profile")),
                 bsCollapsePanel("Writership analysis", 
                                 p(class = "text-muted", "The posterior probability that each writer in the closed-set of writers wrote the questioned document."),
                                 tableOutput("qd_analysis")))
    } else if (!is.null(input$qd_upload)){
      # copy qd to main directory
      create_dir(file.path(global$main_dir, "data", "questioned_docs"))
      copy_qd_to_project(main_dir = global$main_dir, qd_path = global$qd_path, qd_name = global$qd_name)
      
      # analyze
      global$analysis <- analyze_questioned_documents(main_dir = global$main_dir,
                                                      questioned_docs = file.path(global$main_dir, "data", "questioned_docs"),
                                                      model = global$model,
                                                      num_cores = 1,
                                                      writer_indices = c(input$qd_writer_start_char, input$qd_writer_end_char),
                                                      doc_indices = c(input$qd_doc_start_char, input$qd_doc_end_char))
      
      # load processed question doc for report
      global$doc <- load_processed_qd(global$main_dir)
      
      # display QD image, graphs plot, and clusters plot
      bsCollapse(id = "qd_display",
                 bsCollapsePanel("Preview", 
                                 card(
                                   card_header(class = "bg-dark", ""),
                                   # max_height = 300,
                                   full_screen = TRUE,
                                   imageOutput("qd_image"))
                 ),
                 bsCollapsePanel("Processed", 
                                 p(class = "text-muted", "The handwriting in the questioned document is split into 
                                          component shapes called graphs."),
                                 plotOutput("qd_nodes")),
                 bsCollapsePanel("Writer profile", plotOutput("qd_profile")),
                 bsCollapsePanel("Writership analysis", 
                                 p(class = "text-muted", "The posterior probability that each writer in the closed-set of writers wrote the questioned document."),
                                 tableOutput("qd_analysis")))
    } else {
      return(NULL)
    }
  })
  
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