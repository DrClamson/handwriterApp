# The 'handwriterApp' R package performs writership analysis of handwritten
# documents. Copyright (C) 2024 Iowa State University of Science and Technology
# on behalf of its Center for Statistics and Applications in Forensic Evidence
#
# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# this program.  If not, see <https://www.gnu.org/licenses/>.


#' Single Image Module UI
#'
#' Displays a single image inside a card. The image's filename is the card
#' header. The user can click to see a full-screen version of the card. 
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'   server function
#' @param max_height The maximum height of the card.
#'
#' @return A card displaying an image
#' 
#' @noRd
singleImageUI <- function(id, max_height = 250){
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::card(class = "single-image",
                max_width = 300,
                max_height = max_height,
                full_screen = TRUE,
                bslib::card_header(class = "bg-dark", 
                                   shiny::textOutput(ns("title")),
                                   shiny::hr()),
                bslib::card_body(shiny::imageOutput(ns("image")))
    ),
    shiny::br()
  )
}

#' Single Image Module Server
#'
#' Displays a single image inside a card. The user can click to see a
#' full-screen version of the card. By default, the image's filename is the
#' card's header, but an optional title may be supplied.
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'   UI function
#' @param sample A reactive containing the image's file path under 'datapath'.
#'   basename(sample()$datapath) will be used to access the image's filename.
#' @param title Optional. A title to display in the card's header instead of the
#'   image's filename.
#'
#' @return A card displaying an image
#'
#' @noRd
singleImageServer <- function(id, sample, title = NULL) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      output$title <- shiny::renderText({
        if (!is.null(title)){
          title
        } else {
          shiny::req(sample()$datapath)
          basename(sample()$datapath)
        }
      })
      
      output$image <- shiny::renderImage({
        shiny::req(sample()$datapath)
        
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
