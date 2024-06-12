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
  
  
  # FOLDERS ----
  shinyDirChoose(
    input,
    'main_dir',
    roots = c(home = '~'),
    filetypes = c('', 'txt', 'bigWig', "tsv", "csv", "bw")
  )
  
  dir <- reactive(input$main_dir)
  
  # display folder path below button
  output$dir <- renderText({
    global$main_dir
  })
  
  # update main directory to the selected directory
  observeEvent(ignoreInit = TRUE,
               eventExpr = {
                 input$main_dir
               },
               handlerExpr = {
                 # update main directory
                 if (!"path" %in% names(dir())) {
                   return()
                 }
                 home <- normalizePath("~")
                 global$main_dir <- file.path(home, paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep))
                 
                 # setup main directory or load previous analysis
                 if (length(list.files(global$main_dir)) == 0){
                   # setup directory for new analysis
                   setup_main_dir(global$main_dir)
                 } else {
                   # load files if they exist to continue previously started analysis
                   global$known_docs <- list_model_docs(global$main_dir, output_dataframe = TRUE)
                   global$model <- load_model(global$main_dir)
                   global$qd_path <- list_qd(global$main_dir)
                   global$qd_image <- load_qd(global$qd_path)
                   global$doc <- load_processed_qd(global$main_dir)
                   global$analysis <- load_analysis(global$main_dir)
                 }
               })
  
  
  # KNOWN WRITING ----
  # load known images and save to temp directory > data > model_docs
  observeEvent(input$known_upload, {
    global$known_paths <- input$known_upload$datapath
    global$known_names <- input$known_upload$name
  })
  
  output$known_docs <- renderTable({global$known_docs})
  
  output$known_profiles <- renderPlot({handwriter::plot_credible_intervals(model = global$model, facet = TRUE)})
  
  # UI to display known handwriting samples and plots
  output$known_display <- renderUI({
    # skip processing if model already exists in main dir
    if(!is.null(global$model)) {
      # display
      bsCollapse(id = "known_display",
                 bsCollapsePanel("Known writing samples", tableOutput("known_docs")),
                 bsCollapsePanel("Writer profiles", plotOutput("known_profiles"))
      ) 
    } else if (!is.null(input$known_upload)) {
      # fit model when user selects known writing samples
      
      # copy known docs to temp directory > data > model_docs
      create_dir(file.path(global$main_dir, "data", "model_docs"))
      copy_known_files_to_project(global$main_dir, global$known_paths, global$known_names)
      
      # list known docs
      global$known_docs <- list_model_docs(global$main_dir, output_dataframe = TRUE)
      
      global$model <- handwriter::fit_model(main_dir = global$main_dir,
                                            model_docs = file.path(global$main_dir, "data", "model_docs"),
                                            num_iters = 4000,
                                            num_chains = 1,
                                            num_cores = 1,
                                            writer_indices = c(input$known_writer_start_char, input$known_writer_end_char),
                                            doc_indices = c(input$known_doc_start_char, input$known_doc_end_char))
      # display
      bsCollapse(id = "known_display",
                 bsCollapsePanel("Known writing samples", tableOutput("known_docs")),
                 bsCollapsePanel("Writer profiles", plotOutput("known_profiles"))
      ) 
    } else {
      return(NULL)
    }
  })
  
  
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