qdSidebarUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(column(5, set_indices(id = ns("qd_writer_start_char"), label = "Start location")),
             column(5, set_indices(id = ns("qd_writer_end_char"), label = "End location"))),
    helpText(id="qd_docID_help", "Where are the document numbers located in the file names?"),
    fluidRow(column(5, set_indices(id = ns("qd_doc_start_char"), label = "Start location")),
             column(5, set_indices(id = ns("qd_doc_end_char"), label = "End location"))),
    helpText("Select the questioned document."),
    fileInput(ns("qd_upload"), "", accept = ".png", multiple=FALSE)
  )
}

qdBodyUI <- function(id){
  ns <- NS(id)
  tagList(
    # display QD image, graphs plot, and clusters plot
    bsCollapse(id = "qd_display",
               bsCollapsePanel("Preview", 
                               imageOutput(ns("qd_image"))
               ),
               bsCollapsePanel("Processed", 
                               p(class = "text-muted", "The handwriting in the questioned document is split into 
                                          component shapes called graphs."),
                               plotOutput(ns("qd_nodes"))),
               bsCollapsePanel("Writer profile", 
                               plotOutput(ns("qd_profile"))),
               bsCollapsePanel("Writership analysis", 
                               p(class = "text-muted", "The posterior probability that each writer in the closed-set of writers wrote the questioned document."),
                               tableOutput(ns("qd_analysis"))))
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
        global$analysis <- analyze_questioned_documents(main_dir = global$main_dir,
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
          image_write(tempfile(fileext='png'), format = 'png')
        
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
    }
  )
}