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
    shiny::uiOutput(ns("qd_select")),
    shiny::uiOutput(ns("qd_tabs"))
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
        copy_qd_to_project(main_dir = global$main_dir, qd_paths = global$qd_paths, qd_names = global$qd_names)
        
        # get filepaths and names from main_dir > data > questioned_docs folder
        global$qd_paths <- list_qd_paths(global$main_dir)
        global$qd_names <- list_qd_names(global$qd_paths)
        
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
      
      # Select QD from drop-down ----
      # NOTE: this is UI that lives inside server so that the heading is hidden
      # if analysis doesn't exist
      output$qd_select <- renderUI({
        ns <- session$ns
        req(global$qd_paths)
        shiny::tagList(
          shiny::h3("Supporting Materials"),
          shiny::selectInput(ns("qd_select"), label = "Choose a Questioned Document", choices = global$qd_names),
        )
      })
      
      observeEvent(input$qd_select, {
        global$qd_image <- load_qd(input$qd_select)
        global$qd_name <- get_qd_name(input$qd_select)
        global$qd_processed <- load_processed_qd(main_dir = global$main_dir, qd_name = global$qd_name)
      })
      
      # display qd ----
      output$qd_image <- renderImage({
        req(global$qd_image)

        tmp <- global$qd_image %>%
          magick::image_write(tempfile(fileext='png'), format = 'png')

        # return a list
        list(src = tmp, contentType = "image/png")
      }, deleteFile = FALSE
      )
      
      # display processed qd
      output$qd_nodes <- renderPlot({
        req(global$qd_processed)
        handwriter::plotNodes(global$qd_processed, nodeSize = 2)
      })

      # display writer profile for qd
      output$qd_profile <- renderPlot({
        req(global$analysis)
        counts <- global$analysis
        counts$cluster_fill_counts <- counts$cluster_fill_counts %>% dplyr::filter(docname == global$qd_name)
        handwriter::plot_cluster_fill_counts(counts, facet=FALSE)
      })

      # NOTE: this is UI that lives inside server so that tabs are hidden
      # if qd_image doesn't exist
      output$qd_tabs <- renderUI({
        ns <- session$ns
        req(global$qd_image)
        shiny::tagList(
          tabsetPanel(
            tabPanel("Document",
                     imageOutput(ns("qd_image"))),
            tabPanel("Processed Document",
                     plotOutput(ns("qd_nodes"))),
            tabPanel("Writer Profile",
                     plotOutput(ns("qd_profile")))
          )
        )
      })
    }
  )
}