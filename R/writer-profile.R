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

writerProfileServer <- function(id, image_path, sample_num, image_name = NULL) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      output$path <- shiny::renderText({
        req(image_path)
        
        if (is.null(image_name)) {
          basename(image_path)
        } else {
          image_name
        }
      })
      
      output$writer_profile <- shiny::renderPlot({
        # Don't display plot until slr_df is calculated, even though
        # the plot doesn't use slr_df
        req(image_path)
        
        # load the cluster assignments for document
        clusters <- list.files(file.path(tempdir(), "comparison1", "clusters"), full.names = TRUE)
        clusters <- clusters[which(basename(clusters) != "all_clusters.rds")]
        clusters <- readRDS(clusters[sample_num])
        
        counts <- handwriter::get_cluster_fill_counts(clusters)
        
        plot_writer_profile(counts)
      })
    }
  )
}