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

demoPreviewUI <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h1("KNOWN WRITING EXAMPLES"),
    shiny::fluidRow(shiny::column(width=4, singleImageUI(ns("demo1"))),
                    shiny::column(width=4, singleImageUI(ns("demo2"))),
                    shiny::column(width=4, singleImageUI(ns("demo3")))),
    shiny::fluidRow(shiny::column(width=4, singleImageUI(ns("demo4"))),
                    shiny::column(width=4, singleImageUI(ns("demo5"))),
                    shiny::column(width=4, singleImageUI(ns("demo6")))),
    shiny::fluidRow(shiny::column(width=4, singleImageUI(ns("demo7"))),
                    shiny::column(width=4, singleImageUI(ns("demo8"))),
                    shiny::column(width=4, singleImageUI(ns("demo9")))),
    shiny::fluidRow(shiny::column(width=4, singleImageUI(ns("demo10"))),
                    shiny::column(width=4, singleImageUI(ns("demo11"))),
                    shiny::column(width=4, singleImageUI(ns("demo12")))),
    shiny::fluidRow(shiny::column(width=4, singleImageUI(ns("demo13"))),
                    shiny::column(width=4, singleImageUI(ns("demo14"))),
                    shiny::column(width=4, singleImageUI(ns("demo15")))),
    shiny::br(),
    shiny::h1("QUESTIONED WRITING EXAMPLES"),
    shiny::fluidRow(shiny::column(width=4, singleImageUI(ns("demoQ1"))),
                    shiny::column(width=4, singleImageUI(ns("demoQ2"))),
                    shiny::column(width=4, singleImageUI(ns("demoQ3")))),
  )
}

demoPreviewServer <- function(id, global) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      model_docs <- list.files(system.file(file.path("extdata", "template", "data", "model_docs"), package = "handwriterApp"), full.names = TRUE)
      questioned_docs <- list.files(system.file(file.path("extdata", "template", "data", "questioned_docs"), package = "handwriterApp"), full.names = TRUE)
      
      # create a reactive for each model doc
      model_reactives <- lapply(model_docs, function(doc) {
        shiny::reactive({
          list(datapath = doc)
        })
      })
      
      # create a reactive for each questioned doc
      questioned_reactives <- lapply(questioned_docs, function(doc) {
        shiny::reactive({
          list(datapath = doc)
        })
      })
      
      singleImageServer("demo1", model_reactives[[1]])
      singleImageServer("demo2", model_reactives[[2]])
      singleImageServer("demo3", model_reactives[[3]])
      singleImageServer("demo4", model_reactives[[4]])
      singleImageServer("demo5", model_reactives[[5]])
      singleImageServer("demo6", model_reactives[[6]])
      singleImageServer("demo7", model_reactives[[7]])
      singleImageServer("demo8", model_reactives[[8]])
      singleImageServer("demo9", model_reactives[[9]])
      singleImageServer("demo10", model_reactives[[10]])
      singleImageServer("demo11", model_reactives[[11]])
      singleImageServer("demo12", model_reactives[[12]])
      singleImageServer("demo13", model_reactives[[13]])
      singleImageServer("demo14", model_reactives[[14]])
      singleImageServer("demo15", model_reactives[[15]])
      
      singleImageServer("demoQ1", questioned_reactives[[1]])
      singleImageServer("demoQ2", questioned_reactives[[2]])
      singleImageServer("demoQ3", questioned_reactives[[3]])
    }
  )
}
