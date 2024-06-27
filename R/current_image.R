currentImageUI <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::uiOutput(ns("current_select")),
    shiny::uiOutput(ns("current_tabs"))
  )
}

currentImageServer <- function(id, global, type) {
  moduleServer(
    id,
    function(input, output, session) { 
      # Select doc from drop-down ----
      # NOTE: this is UI that lives inside server so that the heading is hidden
      # if object doesn't exist
      output$current_select <- renderUI({
        ns <- session$ns
        global$current_paths <- switch(type, "model" = global$known_paths, "questioned" = global$qd_paths)
        global$current_names <- list_names_in_named_vector(global$current_paths)
        shiny::tagList(
          shiny::h3("Supporting Materials"),
          shiny::selectInput(ns("current_select"), 
                             label = switch(type, "model" = "Chose a Known Writing Sample", "questioned" = "Choose a Questioned Document"), 
                             choices = global$current_names),
        )
      })
      
      observeEvent(input$current_select, {
        global$current_image <- load_image(input$current_select)
        global$current_name <- basename(input$current_select)
        global$current_processed <- load_processed_doc(main_dir = global$main_dir, name = global$current_name, type = type)
      })
      
      # display current doc
      output$current_image <- renderImage({
        req(global$current_image)
        
        tmp <- global$current_image %>%
          magick::image_write(tempfile(fileext='png'), format = 'png')
        
        # return a list
        list(src = tmp, contentType = "image/png")
      }, deleteFile = FALSE
      )
      
      # display processed current doc
      output$current_nodes <- renderPlot({
        req(global$current_processed)
        handwriter::plotNodes(global$current_processed, nodeSize = 2)
      })
      
      # display writer profile for current doc
      output$current_profile <- renderPlot({
        shiny::req(global$current_name)
        switch(type, 
               "model" = {
                 shiny::req(global$model)
                 counts <- global$model
               },
               "questioned" = {
                 shiny::req(global$analysis)
                 counts <- global$analysis
               })
        counts$cluster_fill_counts <- counts$cluster_fill_counts %>% 
          dplyr::filter(docname == stringr::str_replace(global$current_name, ".png", ""))
        handwriter::plot_cluster_fill_counts(counts, facet=FALSE)
      })
      
      # NOTE: this is UI that lives inside server so that tabs are hidden
      # if qd_image doesn't exist
      output$current_tabs <- renderUI({
        ns <- session$ns
        req(global$current_image)
        shiny::tagList(
          tabsetPanel(
            tabPanel("Document",
                     imageOutput(ns("current_image"))
            ),
            tabPanel("Processed Document",
                     plotOutput(ns("current_nodes"))
            ),
            tabPanel("Writer Profile",
                     plotOutput(ns("current_profile"))
            )
          )
        )
      })
    })
}