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
      # store current paths, names, image, processed, and profile locally not
      # globally so docs don't carry over when user switched between Known Writing
      # and QD screens
      local <- reactiveValues(current_paths = NULL,
                              current_names = NULL,
                              current_image = NULL,
                              current_processed = NULL,
                              current_profile = NULL)
      
      # Select doc from drop-down ----
      # NOTE: this is UI that lives inside server so that the heading is hidden
      # if object doesn't exist
      output$current_select <- renderUI({
        ns <- session$ns
        local$current_paths <- switch(type, "model" = global$known_paths, "questioned" = global$qd_paths)
        req(local$current_paths)
        local$current_names <- list_names_in_named_vector(local$current_paths)
        shiny::tagList(
          shiny::h3("Supporting Materials"),
          shiny::selectInput(ns("current_select"), 
                             label = switch(type, "model" = "Chose a Known Writing Sample", "questioned" = "Choose a Questioned Document"), 
                             choices = local$current_names),
        )
      })
      
      observeEvent(input$current_select, {
        local$current_image <- load_image(input$current_select)
        local$current_name <- basename(input$current_select)
        local$current_processed <- load_processed_doc(main_dir = global$main_dir, name = local$current_name, type = type)
      })
      
      # display current doc
      output$current_image <- renderImage({
        req(local$current_image)
        
        tmp <- local$current_image %>%
          magick::image_write(tempfile(fileext='png'), format = 'png')
        
        # return a list
        list(src = tmp, contentType = "image/png")
      }, deleteFile = FALSE
      )
      
      # display processed current doc
      output$current_nodes <- renderPlot({
        req(local$current_processed)
        handwriter::plotNodes(local$current_processed, nodeSize = 2)
      })
      
      # display writer profile for current doc
      output$current_profile <- renderPlot({
        shiny::req(local$current_name)
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
          dplyr::filter(docname == stringr::str_replace(local$current_name, ".png", ""))
        handwriter::plot_cluster_fill_counts(counts, facet=FALSE)
      })
      
      # NOTE: this is UI that lives inside server so that tabs are hidden
      # if qd_image doesn't exist
      output$current_tabs <- renderUI({
        ns <- session$ns
        req(local$current_image)
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