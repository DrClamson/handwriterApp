openSampleSidebarUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fileInput(ns("open_upload1"), "Document 1", accept = ".png", multiple=FALSE),
    shiny::fileInput(ns("open_upload2"), "Document 2", accept = ".png", multiple=FALSE)
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
      
      # load sample 1 and get path and name
      shiny::observeEvent(input$open_upload1, {
        path <- input$open_upload1$datapath
        name <- input$open_upload1$name
        open_global$sample1_path <- path
        open_global$sample1_name <- name
      })
      
      # load sample 1, get path and name, and calculate slr
      shiny::observeEvent(input$open_upload2, {
        # reset slr
        open_global$slr_df <- NULL
        
        path <- input$open_upload2$datapath
        name <- input$open_upload2$name
        open_global$sample2_path <- path
        open_global$sample2_name <- name
        
        open_global$slr_df <- handwriterRF::calculate_slr(
          sample1_path = open_global$sample1_path,
          sample2_path = open_global$sample2_path)
      })
      
      # display slr data frame
      output$slr <- shiny::renderText({
        req(open_global$slr_df)
        
        slr <- open_global$slr_df$slr
        
        if (slr >= 1) {
          # add commas to large numbers
          format(round(slr, 1), big.mark = ",")
        } else if (slr > 0 && slr < 1){
          # round numbers greater than 0 and less than 1 to 3 decimal places
          format(round(slr, 3), nsmall = 2)
        } else {
          slr
        }
      })
      
      # display slr interpretation
      output$slr_interpretation <- shiny::renderText({
        req(open_global$slr_df)
        
        handwriterRF::interpret_slr(open_global$slr_df)
      })
      
      output$slr_results <- shiny::renderUI({
        req(open_global$slr_df)
        ns <- session$ns
        shiny::tagList(
          shiny::h3("COMPARISON RESULTS"),
          shiny::br(),
          shiny::h4("Handwriting Samples"),
          shiny::fluidRow(shiny::column(width=6, singleImageBodyUI(ns("sample1"))),
                          shiny::column(width=6, singleImageBodyUI(ns("sample2")))),
          shiny::h4("Score-based Likelihood Ratio"),
          shiny::textOutput(ns("slr")),
          shiny::br(),
          shiny::h4("Verbal Interpretation of the Score-based Likelihood Ratio"),
          shiny::textOutput(ns("slr_interpretation")),
          shiny::br(),
          shiny::br()
        )
      })
      
      singleImageServer("sample1", open_global$sample1_path, open_global$sample1_name)
      singleImageServer("sample2", open_global$sample2_path, open_global$sample2_name)
    }
  )
}
