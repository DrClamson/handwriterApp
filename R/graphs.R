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


#' Graphs Plot Module UI
#' 
#' Plots the graphs from a document with handwriter::plot_graphs().
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'   server function
#'
#' @return A plot
#' 
#' @noRd
graphsUI <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::card(class = "single-image",
                max_width = 300,
                max_height = 250,
                full_screen = TRUE,
                bslib::card_header(class = "bg-dark", 
                                   shiny::textOutput(ns("path")),
                                   shiny::hr()),
                bslib::card_body(
                  shiny::plotOutput(ns("graphs"))
                )
    ),
    shiny::br()
  )
}

#' Graphs Plot Module Server
#' 
#' Plots the graphs from a document with handwriter::plot_graphs().
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'   server function
#'
#' @return A plot
#' 
#' @noRd
graphsServer <- function(id, sample, graphs) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      output$path <- shiny::renderText({
        shiny::req(sample()$datapath)
        basename(sample()$datapath)
      })
      
      output$graphs <- shiny::renderPlot({
        handwriter::plotNodes(graphs(), nodeSize = 1)
      })
    }
  )
}
