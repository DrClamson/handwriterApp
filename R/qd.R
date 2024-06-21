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
    shiny::h3("Supporting Materials"),
    shiny::fluidRow(
      shiny::column(width = 4, shinycssloaders::withSpinner(uiOutput(ns("qd_image_window")))),
      shiny::column(width = 4, shinycssloaders::withSpinner(uiOutput(ns("qd_nodes_window")))),
      shiny::column(width = 4, shinycssloaders::withSpinner(uiOutput(ns("qd_profile_window"))))
    ),
    shiny::br(),
    shiny::h3("Evaluation Results"),
    shiny::tableOutput(ns("qd_analysis"))
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
      
      # NOTE: this is UI that lives inside server so that button is hidden
      # if qd_image doesn't exist
      output$qd_image_window <- renderUI({
        ns <- session$ns
        req(global$qd_image)
        tagList(
          # QD displayed in pop-up window
          shinyBS::bsModal("qd_image_modal", 
                  title = "Questioned Document", 
                  trigger = ns("qd_image_button"),  # NS needed for UI
                  size = "large",
                  imageOutput(ns("qd_image"))),  
          actionButton(ns("qd_image_button"), "View Document", style = 'width:100%')
        )
      })
      
      # NOTE: this is UI that lives inside server so that button is hidden if
      # doc doesn't exist
      output$qd_nodes_window <- renderUI({
        ns <- session$ns
        req(global$doc)
        # Processed QD displayed in pop-up window
        tagList(
          # QD displayed in pop-up window
          shinyBS::bsModal("qd_nodes_modal", 
                  title = "Questioned Document Decomposed into Graphs", 
                  trigger = ns("qd_nodes_button"),  # NS needed for UI
                  size = "large",
                  plotOutput(ns("qd_nodes"))),  
          actionButton(ns("qd_nodes_button"), "View Document Graphs", style = 'width:100%')
        )
      })
      
      # NOTE: this is UI that lives inside server so that button is hidden if
      # analysis doesn't exist
      output$qd_profile_window <- renderUI({
        ns <- session$ns
        req(global$analysis)
        # QD writer profile displayed in pop-up window
        tagList(
          # QD displayed in pop-up window
          shinyBS::bsModal("qd_profile_modal", 
                  title = "Writer Profile from Questioned Document", 
                  trigger = ns("qd_profile_button"),  # NS needed for UI
                  size = "large",
                  plotOutput(ns("qd_profile"))),  
          actionButton(ns("qd_profile_button"), "View Writer Profile", style = 'width:100%')
        )
      })
      
      # NOTE: this is UI that lives inside server so that box is hidden if
      # analysis doesn't exist
      output$qd_analysis_box <- renderUI({
        ns <- session$ns
        req(global$analysis)
        tableOutput(ns("qd_analysis"))  # NS needed for UI
      })
    }
  )
}