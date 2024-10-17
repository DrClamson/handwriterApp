writerProfileBodyUI <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::card(class = "single-image",
                max_width = 300,
                max_height = 250,
                full_screen = FALSE,
                bslib::card_header(class = "bg-dark", shiny::textOutput(ns("path"))),
                shiny::hr(),
                shiny::plotOutput(ns("writer_profile"))
    ),
    shiny::br()
  )
}

writerProfileServer <- function(id, sample) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      output$path <- shiny::renderText({
        req(sample()$datapath)
        basename(sample()$datapath)
      })
      
      output$writer_profile <- shiny::renderPlot({
        # Don't display plot until slr_df is calculated, even though
        # the plot doesn't use slr_df
        req(file.exists(file.path(tempdir(), "comparison1", "clusters", stringr::str_replace(basename(sample()$datapath), ".png", ".rds"))))
        
        # load the cluster assignments for document
        clusters <- readRDS(file.path(tempdir(), "comparison1", "clusters", stringr::str_replace(basename(sample()$datapath), ".png", ".rds")))
        
        counts <- handwriter::get_cluster_fill_counts(clusters)
        
        plot_writer_profile(counts)
      })
    }
  )
}