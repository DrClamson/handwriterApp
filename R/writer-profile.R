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

writerProfileServer <- function(id, sample, clusters) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      output$path <- shiny::renderText({
        req(sample()$datapath)
        basename(sample()$datapath)
      })
      
      output$writer_profile <- shiny::renderPlot({
        counts <- handwriter::get_cluster_fill_counts(clusters())
        
        plot_writer_profile(counts)
      })
    }
  )
}