openSampleSidebarUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::helpText("Select two scanned handwriting samples saved as PNG images."),
    shiny::fileInput(ns("open_upload1"), "Sample 1", accept = ".png", multiple=FALSE),
    shiny::fileInput(ns("open_upload2"), "Sample 2", accept = ".png", multiple=FALSE)
  )
}

openSampleBodyUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinycssloaders::withSpinner(shiny::uiOutput(ns("slr_results")))
  )
}

openSampleServer <- function(id, open_global) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      shiny::observeEvent(input$open_upload1, {
        path <- input$open_upload1$datapath
        name <- input$open_upload1$name
        open_global$sample1_path <- path
        open_global$sample1_name <- name
      })
      
      shiny::observeEvent(input$open_upload2, {
        # reset slr
        open_global$slr <- NULL
        
        path <- input$open_upload2$datapath
        name <- input$open_upload2$name
        open_global$sample2_path <- path
        open_global$sample2_name <- name
        
        open_global$slr <- handwriterRF::calculate_slr(
          sample1_path = open_global$sample1_path,
          sample2_path = open_global$sample2_path)
      })
      
      output$slr <- shiny::renderText({
        open_global$slr
      })
      
      output$slr_results <- shiny::renderUI({
        req(open_global$slr)
        ns <- session$ns
        shiny::tagList(
          shiny::h3("Comparison Results"),
          shiny::fluidRow(shiny::column(width=6, singleImageBodyUI(ns("sample1"))),
                          shiny::column(width=6, singleImageBodyUI(ns("sample2")))),
          shiny::HTML("Score-based Likelihood Ratio"),
          shiny::textOutput(ns("slr"))
        )
      })
      
      singleImageServer("sample1", open_global$sample1_path, open_global$sample1_name)
      singleImageServer("sample2", open_global$sample2_path, open_global$sample2_name)
    }
  )
}
