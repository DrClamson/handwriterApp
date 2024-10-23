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

singleImageServer <- function(id, sample) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      output$path <- shiny::renderText({
        req(sample()$datapath)
        basename(sample()$datapath)
      })
      
      output$image <- shiny::renderImage({
        req(sample()$datapath)
        
        image <- magick::image_read(sample()$datapath)
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