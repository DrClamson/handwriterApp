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
    currentImageUI(ns("known"))
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
        copy_docs_to_project(main_dir = global$main_dir, 
                             paths = known_paths, 
                             names = known_names,
                             type = "model")
        
        # list known filepaths
        global$known_paths <- list_docs(global$main_dir, type = "model", filepaths = TRUE)
        global$known_names <- list_names_in_named_vector(global$known_paths)
        
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
      
      currentImageServer("known", global, "model")
    }
  )
}