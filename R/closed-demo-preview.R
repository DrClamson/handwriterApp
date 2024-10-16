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
    shiny::h3("KNOWN WRITING EXAMPLES"),
    shiny::fluidRow(shiny::column(width=4, singleImageBodyUI(ns("demo1"))),
                    shiny::column(width=4, singleImageBodyUI(ns("demo2"))),
                    shiny::column(width=4, singleImageBodyUI(ns("demo3")))),
    shiny::fluidRow(shiny::column(width=4, singleImageBodyUI(ns("demo4"))),
                    shiny::column(width=4, singleImageBodyUI(ns("demo5"))),
                    shiny::column(width=4, singleImageBodyUI(ns("demo6")))),
    shiny::fluidRow(shiny::column(width=4, singleImageBodyUI(ns("demo7"))),
                    shiny::column(width=4, singleImageBodyUI(ns("demo8"))),
                    shiny::column(width=4, singleImageBodyUI(ns("demo9")))),
    shiny::fluidRow(shiny::column(width=4, singleImageBodyUI(ns("demo10"))),
                    shiny::column(width=4, singleImageBodyUI(ns("demo11"))),
                    shiny::column(width=4, singleImageBodyUI(ns("demo12")))),
    shiny::fluidRow(shiny::column(width=4, singleImageBodyUI(ns("demo13"))),
                    shiny::column(width=4, singleImageBodyUI(ns("demo14"))),
                    shiny::column(width=4, singleImageBodyUI(ns("demo15")))),
    shiny::br(),
    shiny::h3("QUESTIONED WRITING EXAMPLES"),
    shiny::fluidRow(shiny::column(width=4, singleImageBodyUI(ns("demoQ1"))),
                    shiny::column(width=4, singleImageBodyUI(ns("demoQ2"))),
                    shiny::column(width=4, singleImageBodyUI(ns("demoQ3")))),
  )
}

demoPreviewServer <- function(id, global) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      singleImageServer("demo1", system.file(file.path("extdata", "template", "data", "model_docs", "w0009_s01_pWOZ_r01.png"), package = "handwriterApp"))
      singleImageServer("demo2", system.file(file.path("extdata", "template", "data", "model_docs", "w0009_s01_pWOZ_r02.png"), package = "handwriterApp"))
      singleImageServer("demo3", system.file(file.path("extdata", "template", "data", "model_docs", "w0009_s01_pWOZ_r03.png"), package = "handwriterApp"))
      singleImageServer("demo4", system.file(file.path("extdata", "template", "data", "model_docs", "w0030_s01_pWOZ_r01.png"), package = "handwriterApp"))
      singleImageServer("demo5", system.file(file.path("extdata", "template", "data", "model_docs", "w0030_s01_pWOZ_r02.png"), package = "handwriterApp"))
      singleImageServer("demo6", system.file(file.path("extdata", "template", "data", "model_docs", "w0030_s01_pWOZ_r03.png"), package = "handwriterApp"))
      singleImageServer("demo7", system.file(file.path("extdata", "template", "data", "model_docs", "w0203_s01_pWOZ_r01.png"), package = "handwriterApp"))
      singleImageServer("demo8", system.file(file.path("extdata", "template", "data", "model_docs", "w0203_s01_pWOZ_r02.png"), package = "handwriterApp"))
      singleImageServer("demo9", system.file(file.path("extdata", "template", "data", "model_docs", "w0203_s01_pWOZ_r03.png"), package = "handwriterApp"))
      singleImageServer("demo10", system.file(file.path("extdata", "template", "data", "model_docs", "w0238_s01_pWOZ_r01.png"), package = "handwriterApp"))
      singleImageServer("demo11", system.file(file.path("extdata", "template", "data", "model_docs", "w0238_s01_pWOZ_r02.png"), package = "handwriterApp"))
      singleImageServer("demo12", system.file(file.path("extdata", "template", "data", "model_docs", "w0238_s01_pWOZ_r03.png"), package = "handwriterApp"))
      singleImageServer("demo13", system.file(file.path("extdata", "template", "data", "model_docs", "w0400_s01_pWOZ_r01.png"), package = "handwriterApp"))
      singleImageServer("demo14", system.file(file.path("extdata", "template", "data", "model_docs", "w0400_s01_pWOZ_r02.png"), package = "handwriterApp"))
      singleImageServer("demo15", system.file(file.path("extdata", "template", "data", "model_docs", "w0400_s01_pWOZ_r03.png"), package = "handwriterApp"))
      
      singleImageServer("demoQ1", system.file(file.path("extdata", "template", "data", "questioned_docs", "w0009_s03_pLND_r02.png"), package = "handwriterApp"))
      singleImageServer("demoQ2", system.file(file.path("extdata", "template", "data", "questioned_docs", "w0030_s02_pLND_r01.png"), package = "handwriterApp"))
      singleImageServer("demoQ3", system.file(file.path("extdata", "template", "data", "questioned_docs", "w0238_s01_pLND_r01.png"), package = "handwriterApp"))
    }
  )
}
