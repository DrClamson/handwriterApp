qdSidebarUI <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    fluidRow(shiny::column(5, set_indices(id = ns("qd_writer_start_char"), label = "Start location")),
             shiny::column(5, set_indices(id = ns("qd_writer_end_char"), label = "End location"))),
    helpText(id="qd_docID_help", "Where are the document numbers located in the file names?"),
    fluidRow(shiny::column(5, set_indices(id = ns("qd_doc_start_char"), label = "Start location")),
             shiny::column(5, set_indices(id = ns("qd_doc_end_char"), label = "End location"))),
    helpText("Select the questioned document."),
    fileInput(ns("qd_upload"), "", accept = ".png", multiple=FALSE)
  )
}

qdBodyUI <- function(id){
  ns <- shiny::NS(id)
  tagList(
    shinycssloaders::withSpinner(uiOutput(ns("qd_results"))),
    shinycssloaders::withSpinner(uiOutput(ns("qd_tabs")))
  )
}

qdServer <- function(id, global) {
  moduleServer(
    id,
    function(input, output, session) { 
      observeEvent(input$qd_upload, {
        global$qd_path <- input$qd_upload$datapath
        global$qd_name <- input$qd_upload$name
        
        global$qd_image <- NULL
        global$qd_image <- load_qd(global$qd_path)
        
        # copy qd to main directory
        create_dir(file.path(global$main_dir, "data", "questioned_docs"))
        copy_qd_to_project(main_dir = global$main_dir, qd_path = global$qd_path, qd_name = global$qd_name)
        
        # analyze
        global$analysis <- handwriter::analyze_questioned_documents(main_dir = global$main_dir,
                                                        questioned_docs = file.path(global$main_dir, "data", "questioned_docs"),
                                                        model = global$model,
                                                        num_cores = 1,
                                                        writer_indices = c(input$qd_writer_start_char, input$qd_writer_end_char),
                                                        doc_indices = c(input$qd_doc_start_char, input$qd_doc_end_char))
        
        # load processed question doc for report
        global$doc <- load_processed_qd(global$main_dir)
      })
      
      output$qd_image <- renderImage({
        req(global$qd_image)
        tmp <- global$qd_image %>%
          magick::image_write(tempfile(fileext='png'), format = 'png')
        
        # return a list
        list(src = tmp, contentType = "image/png")
      }, deleteFile = FALSE
      )
      
      output$qd_nodes <- renderPlot({
        req(global$doc)
        handwriter::plotNodes(global$doc, nodeSize = 2)
      })
      
      output$qd_profile <- renderPlot({
        req(global$analysis)
        handwriter::plot_cluster_fill_counts(global$analysis, facet=FALSE)
      })
      
      output$qd_analysis <- renderTable({
        req(global$analysis)
        make_posteriors_df(global$analysis)
      })
      
      # NOTE: this is UI that lives inside server so that heading is hidden
      # if analysis doesn't exist
      output$qd_results <- renderUI({
        ns <- session$ns
        req(global$analysis)
        tagList(
          shiny::h3("Evaluation Results"),
          shiny::tableOutput(ns("qd_analysis")),
        )
      })
      
      # NOTE: this is UI that lives inside server so that tabs are hidden
      # if qd_image doesn't exist
      output$qd_tabs <- renderUI({
        ns <- session$ns
        req(global$qd_image)
        tagList(
          shiny::h3("Supporting Materials"),
          tabsetPanel(
            tabPanel("Questioned Document",
                     imageOutput(ns("qd_image"))),
            tabPanel("Processed Questioned Document",
                     plotOutput(ns("qd_nodes"))),
            tabPanel("Questioned Document Writer Profile",
                     plotOutput(ns("qd_profile")))
          )
        )
      })
    }
  )
}