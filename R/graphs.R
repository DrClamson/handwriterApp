singleImageBodyUI <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::card(class = "single-image",
                max_width = 300,
                max_height = 250,
                full_screen = TRUE,
                bslib::card_header(class = "bg-dark", 
                                   shiny::textOutput(ns("path")),
                                   shiny::hr()),
                bslib::card_body(shiny::imageOutput(ns("image")))
    ),
    shiny::br()
  )
}

graphsBodyUI <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::card(class = "single-image",
                max_width = 300,
                max_height = 250,
                full_screen = TRUE,
                bslib::card_header(class = "bg-dark", 
                                   shiny::textOutput(ns("path")),
                                   shiny::hr()),
                bslib::card_body(
                  min_width = 600,
                  min_height = 1000,
                  shiny::plotOutput(ns("graphs"))
                )
    ),
    shiny::br()
  )
}

graphsServer <- function(id, sample, graphs) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      output$path <- shiny::renderText({
        shiny::req(sample()$datapath)
        basename(sample()$datapath)
      })
      
      output$graphs <- shiny::renderPlot({
        handwriter::plot_graphs(graphs(), ncol=5)
      },
      width = 600)
    }
  )
}