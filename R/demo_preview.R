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
      demoImageServer("demo1", global, system.file(file.path("extdata", "template", "data", "model_docs", "w0009_s01_pWOZ_r01.png"), package = "handwriterApp"))
      demoImageServer("demo2", global, system.file(file.path("extdata", "template", "data", "model_docs", "w0009_s01_pWOZ_r02.png"), package = "handwriterApp"))
      demoImageServer("demo3", global, system.file(file.path("extdata", "template", "data", "model_docs", "w0009_s01_pWOZ_r03.png"), package = "handwriterApp"))
      demoImageServer("demo4", global, system.file(file.path("extdata", "template", "data", "model_docs", "w0030_s01_pWOZ_r01.png"), package = "handwriterApp"))
      demoImageServer("demo5", global, system.file(file.path("extdata", "template", "data", "model_docs", "w0030_s01_pWOZ_r02.png"), package = "handwriterApp"))
      demoImageServer("demo6", global, system.file(file.path("extdata", "template", "data", "model_docs", "w0030_s01_pWOZ_r03.png"), package = "handwriterApp"))
      demoImageServer("demo7", global, system.file(file.path("extdata", "template", "data", "model_docs", "w0203_s01_pWOZ_r01.png"), package = "handwriterApp"))
      demoImageServer("demo8", global, system.file(file.path("extdata", "template", "data", "model_docs", "w0203_s01_pWOZ_r02.png"), package = "handwriterApp"))
      demoImageServer("demo9", global, system.file(file.path("extdata", "template", "data", "model_docs", "w0203_s01_pWOZ_r03.png"), package = "handwriterApp"))
      demoImageServer("demo10", global, system.file(file.path("extdata", "template", "data", "model_docs", "w0238_s01_pWOZ_r01.png"), package = "handwriterApp"))
      demoImageServer("demo11", global, system.file(file.path("extdata", "template", "data", "model_docs", "w0238_s01_pWOZ_r02.png"), package = "handwriterApp"))
      demoImageServer("demo12", global, system.file(file.path("extdata", "template", "data", "model_docs", "w0238_s01_pWOZ_r03.png"), package = "handwriterApp"))
      demoImageServer("demo13", global, system.file(file.path("extdata", "template", "data", "model_docs", "w0400_s01_pWOZ_r01.png"), package = "handwriterApp"))
      demoImageServer("demo14", global, system.file(file.path("extdata", "template", "data", "model_docs", "w0400_s01_pWOZ_r02.png"), package = "handwriterApp"))
      demoImageServer("demo15", global, system.file(file.path("extdata", "template", "data", "model_docs", "w0400_s01_pWOZ_r03.png"), package = "handwriterApp"))
      
      demoImageServer("demoQ1", global, system.file(file.path("extdata", "template", "data", "questioned_docs", "w0009_s03_pLND_r02.png"), package = "handwriterApp"))
      demoImageServer("demoQ2", global, system.file(file.path("extdata", "template", "data", "questioned_docs", "w0030_s02_pLND_r01.png"), package = "handwriterApp"))
      demoImageServer("demoQ3", global, system.file(file.path("extdata", "template", "data", "questioned_docs", "w0238_s01_pLND_r01.png"), package = "handwriterApp"))
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