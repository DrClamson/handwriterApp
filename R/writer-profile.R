# display cluster fill counts for a single document inside a card. The card
# header is the file name of the PNG document.
writerProfileBodyUI <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::card(class = "single-image",
                max_width = 600,
                max_height = 400,
                full_screen = FALSE,
                bslib::card_header(class = "bg-dark", shiny::textOutput(ns("path"))),
                shiny::hr(),
                shiny::plotOutput(ns("writer_profiles"))
    ),
    shiny::br()
  )
}

writerProfileServer <- function(id, sample1, sample2, clusters) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      output$path <- shiny::renderText({
        req(sample1()$datapath, sample2()$datapath)
        paste(basename(sample1()$datapath), "and", basename(sample2()$datapath))
      })
      
      output$writer_profiles <- shiny::renderPlot({
        df <- rbind(clusters()$sample1, clusters()$sample2)
        
        counts <- handwriter::get_cluster_fill_counts(df)
        rates <- handwriterRF::get_cluster_fill_rates(counts)
        
        plot_writer_profiles(rates)
      })
    }
  )
}