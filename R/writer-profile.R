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


#' Writer Profiles Module UI
#'
#' Displays a plot of two writer profiles inside a card. plot_writer_profiles()
#' creates the plot. The user can click to see a full-screen version of the
#' plot.
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'   server function
#'
#' @return A plot in a card
#'
#' @noRd
writerProfileUI <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::card(class = "single-image",
                max_width = 600,
                max_height = 400,
                full_screen = FALSE,
                bslib::card_header(class = "bg-dark", shiny::textOutput(ns("path"))),
                shiny::hr(),
                shiny::plotOutput(ns("writer_profiles"))
    ),
    shiny::br()
  )
}

#' Writer Profiles Module Server
#'
#' Displays a plot of two writer profiles inside a card. plot_writer_profiles()
#' creates the plot. The user can click to see a full-screen version of the
#' plot. The header of the card is the filenames of the original images from
#' which the writer profiles were estimated.
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'   UI function
#' @param sample1 A reactive containing an uploaded image. Used to get the
#'   filename of the image for the card header.
#' @param sample2 A reactive containing an uploaded image. Used to get the
#'   filename of the image for the card header.
#' @param clusters The cluster assignments from the two images created with
#'   handwriter::get_clusters_batch(). clusters is a reactiveValues where
#'   clusters$sample1 and clusters$sample2 contain the cluster assignments from
#'   sample1 and sample2, respectively.
#'
#' @return A plot in a card
#'
#' @noRd
writerProfileServer <- function(id, sample1, sample2, clusters) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      output$path <- shiny::renderText({
        shiny::req(sample1()$datapath, sample2()$datapath)
        paste(basename(sample1()$datapath), "and", basename(sample2()$datapath))
      })
      
      output$writer_profiles <- shiny::renderPlot({
        df <- rbind(clusters()$sample1, clusters()$sample2)
        
        counts <- handwriter::get_cluster_fill_counts(df)
        rates <- handwriterRF::get_cluster_fill_rates(counts)
        
        plot_writer_profiles(rates)
      })
    }
  )
}
