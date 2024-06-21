knownSidebarUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(shiny::column(5, set_indices(id = ns("known_writer_start_char"), label = "Start location")),
             shiny::column(5, set_indices(id = ns("known_writer_end_char"), label = "End location"))),
    shiny::helpText("Where are the document numbers located in the file names?"),
    shiny::fluidRow(shiny::column(5, set_indices(id = ns("known_doc_start_char"), label = "Start location")),
             shiny::column(5, set_indices(id = ns("known_doc_end_char"), label = "End location"))),
    shiny::helpText("Select three known writing samples from each person of interest."),
    shiny::fileInput(ns("known_upload"), "", accept = ".png", multiple=TRUE)
  )
}

knownBodyUI <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    shinycssloaders::withSpinner(uiOutput(ns("known_tabs")))
  )
}

knownServer <- function(id, global) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      shiny::observeEvent(input$known_upload, {
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
      
      output$known_docs <- shiny::renderTable({
        req(global$known_docs)
        global$known_docs
      })
      
      output$known_profiles <- shiny::renderPlot({
        req(global$model)
        handwriter::plot_credible_intervals(model = global$model, facet = TRUE)
      })
      
      # NOTE: this is UI that lives inside server so that tabs are hidden if known_docs
      # doesn't exist
      output$known_tabs <- shiny::renderUI({
        ns <- session$ns
        req(global$known_docs)
        tagList(
          tabsetPanel(
            tabPanel("Known Writing Samples",
                     tableOutput(ns("known_docs"))
            ),
            tabPanel("Known Writer Profiles",
                     plotOutput(ns("known_profiles"))
            )
          )
        )
      })
    }
  )
}