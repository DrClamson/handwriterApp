demoPreviewBodyUI <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h3("Known Writing Examples"),
    shiny::fluidRow(shiny::column(width=4, demoImageBodyUI(ns("demo1"))),
                    shiny::column(width=4, demoImageBodyUI(ns("demo2"))),
                    shiny::column(width=4, demoImageBodyUI(ns("demo3")))),
    shiny::fluidRow(shiny::column(width=4, demoImageBodyUI(ns("demo4"))),
                    shiny::column(width=4, demoImageBodyUI(ns("demo5"))),
                    shiny::column(width=4, demoImageBodyUI(ns("demo6")))),
    shiny::fluidRow(shiny::column(width=4, demoImageBodyUI(ns("demo7"))),
                    shiny::column(width=4, demoImageBodyUI(ns("demo8"))),
                    shiny::column(width=4, demoImageBodyUI(ns("demo9")))),
    shiny::fluidRow(shiny::column(width=4, demoImageBodyUI(ns("demo10"))),
                    shiny::column(width=4, demoImageBodyUI(ns("demo11"))),
                    shiny::column(width=4, demoImageBodyUI(ns("demo12")))),
    shiny::fluidRow(shiny::column(width=4, demoImageBodyUI(ns("demo13"))),
                    shiny::column(width=4, demoImageBodyUI(ns("demo14"))),
                    shiny::column(width=4, demoImageBodyUI(ns("demo15")))),
    shiny::br(),
    shiny::h3("Questioned Writing Examples"),
    shiny::fluidRow(shiny::column(width=4, demoImageBodyUI(ns("demoQ1"))),
                    shiny::column(width=4, demoImageBodyUI(ns("demoQ2"))),
                    shiny::column(width=4, demoImageBodyUI(ns("demoQ3")))),
  )
}

demoPreviewServer <- function(id, global) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      demoImageServer("demo1", global, testthat::test_path("fixtures", "template", "data", "model_docs", "w0009_s01_pWOZ_r01.png"))
      demoImageServer("demo2", global, testthat::test_path("fixtures", "template", "data", "model_docs", "w0009_s01_pWOZ_r02.png"))
      demoImageServer("demo3", global, testthat::test_path("fixtures", "template", "data", "model_docs", "w0009_s01_pWOZ_r03.png"))
      demoImageServer("demo4", global, testthat::test_path("fixtures", "template", "data", "model_docs", "w0030_s01_pWOZ_r01.png"))
      demoImageServer("demo5", global, testthat::test_path("fixtures", "template", "data", "model_docs", "w0030_s01_pWOZ_r02.png"))
      demoImageServer("demo6", global, testthat::test_path("fixtures", "template", "data", "model_docs", "w0030_s01_pWOZ_r03.png"))
      demoImageServer("demo7", global, testthat::test_path("fixtures", "template", "data", "model_docs", "w0203_s01_pWOZ_r01.png"))
      demoImageServer("demo8", global, testthat::test_path("fixtures", "template", "data", "model_docs", "w0203_s01_pWOZ_r02.png"))
      demoImageServer("demo9", global, testthat::test_path("fixtures", "template", "data", "model_docs", "w0203_s01_pWOZ_r03.png"))
      demoImageServer("demo10", global, testthat::test_path("fixtures", "template", "data", "model_docs", "w0238_s01_pWOZ_r01.png"))
      demoImageServer("demo11", global, testthat::test_path("fixtures", "template", "data", "model_docs", "w0238_s01_pWOZ_r02.png"))
      demoImageServer("demo12", global, testthat::test_path("fixtures", "template", "data", "model_docs", "w0238_s01_pWOZ_r03.png"))
      demoImageServer("demo13", global, testthat::test_path("fixtures", "template", "data", "model_docs", "w0400_s01_pWOZ_r01.png"))
      demoImageServer("demo14", global, testthat::test_path("fixtures", "template", "data", "model_docs", "w0400_s01_pWOZ_r02.png"))
      demoImageServer("demo15", global, testthat::test_path("fixtures", "template", "data", "model_docs", "w0400_s01_pWOZ_r03.png"))
      
      demoImageServer("demoQ1", global, testthat::test_path("fixtures", "template", "data", "questioned_docs", "w0009_s03_pLND_r02.png"))
      demoImageServer("demoQ2", global, testthat::test_path("fixtures", "template", "data", "questioned_docs", "w0030_s02_pLND_r01.png"))
      demoImageServer("demoQ3", global, testthat::test_path("fixtures", "template", "data", "questioned_docs", "w0238_s01_pLND_r01.png"))
    }
  )
}

demoImageBodyUI <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    # allows users to scroll vertically and horizontally
    bslib::card(
      bslib::card_header(class = "bg-dark", shiny::textOutput(ns("path"))),
      max_width = 300,
      max_height = 250,
      full_screen = FALSE,
      shiny::imageOutput(ns("image"))
    ),
    shiny::br()
  )
}

demoImageServer <- function(id, global, image_path) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      output$path <- shiny::renderText({basename(image_path)})
      
      output$image <- shiny::renderImage({
        path <- image_path
        
        image <- magick::image_read(path)
        tmp <- image %>%
          magick::image_write(tempfile(fileext='png'), format = 'png')
        
        # return a list
        list(src = tmp, 
             contentType = "image/png",
             width = "100%")
      }, deleteFile = FALSE
      )
    }
  )
}