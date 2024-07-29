demoQDSidebarUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(shiny::column(width=12, align = "center", actionButton(ns("demo_qd_analyze"), "Analyze Questioned Docs"))),
    shiny::br()
  )
}

demoQDBodyUI <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    shinycssloaders::withSpinner(shiny::uiOutput(ns("qd_results"))),
    currentImageUI(ns("qd"))
  )
}

demoQDServer <- function(id, global) {
  shiny::moduleServer(
    id,
    function(input, output, session) { 
      shiny::observeEvent(input$demo_qd_analyze, {
        # known writing samples in tests folder
        qd_paths <- list.files(testthat::test_path("fixtures", "template", "data", "questioned_docs"), full.names = TRUE)
        qd_names <- basename(qd_paths)
        
        # copy qd to main directory
        copy_docs_to_project(main_dir = global$main_dir, 
                             paths = qd_paths, 
                             names = qd_names, 
                             type = "questioned")
        
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
                                                                    writer_indices = c(2, 5),
                                                                    doc_indices = c(7, 18))
        
      })
      
      # Display analysis ----
      # NOTE: this is UI that lives inside server so that the heading is hidden
      # if analysis doesn't exist
      output$qd_analysis <- shiny::renderTable({
        shiny::req(global$analysis)
        make_posteriors_df(global$analysis)
      })
      output$qd_results <- shiny::renderUI({
        ns <- session$ns
        shiny::req(global$analysis)
        shiny::tagList(
          shiny::h3("Evaluation Results"),
          shiny::HTML("<p>The table shows the posterior probability of writership for each questioned document and each known writer. Each
                      column corresponds to a questioned document and each row corresponds to a known writer. The posterior probability of 
                      writership in each column sums to 100%.</p>"),
          shiny::tableOutput(ns("qd_analysis")),
          shiny::br()
        )
      })
      
      currentImageServer("qd", global, "questioned")
    }
  )
}