knownSidebarUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(5, set_indices(id = ns("known_writer_start_char"), label = "Start location")),
             column(5, set_indices(id = ns("known_writer_end_char"), label = "End location"))),
    helpText("Where are the document numbers located in the file names?"),
    fluidRow(column(5, set_indices(id = ns("known_doc_start_char"), label = "Start location")),
             column(5, set_indices(id = ns("known_doc_end_char"), label = "End location"))),
    helpText("Select three known writing samples from each person of interest."),
    fileInput(ns("known_upload"), "", accept = ".png", multiple=TRUE)
  )
}

knownBodyUI <- function(id){
  ns <- NS(id)
  tagList(
    bsCollapse(id = "known_display",
               bsCollapsePanel(ns("Known writing samples"), tableOutput(ns("known_docs"))),
               bsCollapsePanel(ns("Writer profiles"), plotOutput(ns("known_profiles"))))
  )
}

knownSidebarServer <- function(id, global) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(input$known_upload, {
        known_paths <- input$known_upload$datapath
        known_names <- input$known_upload$name
        
        # copy known docs to temp directory > data > model_docs
        create_dir(file.path(global$main_dir, "data", "model_docs"))
        copy_known_files_to_project(global$main_dir, known_paths, known_names)
        
        # list known docs
        global$known_docs <- list_model_docs(global$main_dir, output_dataframe = TRUE)
        
        # fit model
        global$model <- handwriter::fit_model(main_dir = global$main_dir,
                                              model_docs = file.path(global$main_dir, "data", "model_docs"),
                                              num_iters = 4000,
                                              num_chains = 1,
                                              num_cores = 1,
                                              writer_indices = c(input$known_writer_start_char, 
                                                                 input$known_writer_end_char),
                                              doc_indices = c(input$known_doc_start_char, 
                                                              input$known_doc_end_char))
      })
      
      output$known_docs <- renderTable({
        req(global$known_docs)
        global$known_docs
      })
      
      output$known_profiles <- renderPlot({
        req(global$model)
        handwriter::plot_credible_intervals(model = global$model, facet = TRUE)
      })
    }
  )
}