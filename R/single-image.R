singleImageBodyUI <- function(id){
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

singleImageServer <- function(id, image_path) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      output$path <- shiny::renderText({
        req(image_path)
        basename(image_path)
      })
      
      output$image <- shiny::renderImage({
        req(image_path)
        
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