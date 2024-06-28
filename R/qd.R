qdSidebarUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(shiny::column(5, set_indices(id = ns("qd_writer_start_char"), label = "Start location")),
                    shiny::column(5, set_indices(id = ns("qd_writer_end_char"), label = "End location"))),
    shiny::helpText(id="qd_docID_help", "Where are the document numbers located in the file names?"),
    shiny::fluidRow(shiny::column(5, set_indices(id = ns("qd_doc_start_char"), label = "Start location")),
                    shiny::column(5, set_indices(id = ns("qd_doc_end_char"), label = "End location"))),
    shiny::helpText("Select the questioned document."),
    shiny::fileInput(ns("qd_upload"), "", accept = ".png", multiple=TRUE)
  )
}

qdBodyUI <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    shinycssloaders::withSpinner(shiny::uiOutput(ns("qd_results"))),
    currentImageUI(ns("qd"))
  )
}

qdServer <- function(id, global) {
  moduleServer(
    id,
    function(input, output, session) { 
      observeEvent(input$qd_upload, {
        global$qd_paths <- input$qd_upload$datapath  # filepaths for temp docs not filepaths on disk
        global$qd_names <- input$qd_upload$name
        
        # copy qd to main directory
        create_dir(file.path(global$main_dir, "data", "questioned_docs"))
        copy_docs_to_project(main_dir = global$main_dir, paths = global$qd_paths, names = global$qd_names, type = "questioned")
        
        # get filepaths and names from main_dir > data > questioned_docs folder
        global$qd_paths <- list_docs(global$main_dir, type = "questioned", filepaths = TRUE)
        # get a named vector where the names are the filenames and the values
        # are the full filepaths to use with the selectInput so the user sees only
        # sees the filenames in the drop-down menu but behind the scenes the app
        # gets the filepaths
        global$qd_names <- list_names_in_named_vector(global$qd_paths)
        
        # analyze
        global$analysis <- handwriter::analyze_questioned_documents(main_dir = global$main_dir,
                                                                    questioned_docs = file.path(global$main_dir, "data", "questioned_docs"),
                                                                    model = global$model,
                                                                    num_cores = 1,
                                                                    writer_indices = c(input$qd_writer_start_char, input$qd_writer_end_char),
                                                                    doc_indices = c(input$qd_doc_start_char, input$qd_doc_end_char))
        
      })
      
      # Display analysis ----
      # NOTE: this is UI that lives inside server so that the heading is hidden
      # if analysis doesn't exist
      output$qd_analysis <- renderTable({
        req(global$analysis)
        make_posteriors_df(global$analysis)
      })
      output$qd_results <- renderUI({
        ns <- session$ns
        req(global$analysis)
        shiny::tagList(
          shiny::h3("Evaluation Results"),
          shiny::tableOutput(ns("qd_analysis")),
          shiny::br()
        )
      })
      
      currentImageServer("qd", global, "questioned")
    }
  )
}