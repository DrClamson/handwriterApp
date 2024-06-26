currentImageUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("qd_select")),
    shiny::uiOutput(ns("qd_tabs"))
  )
}

currentImageServer <- function(id, global) {
  shiny::moduleServer(
    id,
    function(input, output, session) { 
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