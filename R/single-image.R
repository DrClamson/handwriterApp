singleImageBodyUI <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    # allows users to scroll vertically and horizontally
    bslib::card(class = "single-image",
                max_width = 300,
                max_height = 250,
                full_screen = FALSE,
                bslib::card_header(class = "bg-dark", shiny::textOutput(ns("path"))),
                shiny::hr(),
                shiny::imageOutput(ns("image"))
    ),
    shiny::br()
  )
}

singleImageServer <- function(id, image_path, image_name = NULL) {
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