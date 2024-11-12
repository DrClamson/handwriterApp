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

#' Select Image Module UI
#' 
#' Creates a drop-down list of known or questioned images. The user selects an image
#' from the drop-down list and a preview of the image, a plot with the image's nodes, 
#' and a plot of the writer's profile are displayed in three separate tabs.
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'   server function
#'
#' @return A drop-down list and tabs
#' 
#' @noRd
selectImageUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("current_select")),
    shiny::uiOutput(ns("current_tabs"))
  )
}

#' Select Image Module Server
#' 
#' Creates a drop-down list of known or questioned images. The user selects an image
#' from the drop-down list and a preview of the image, a plot with the image's nodes, 
#' and a plot of the writer's profile are displayed in three separate tabs.
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'   server function
#' @param global Reactive values
#' @param type Either "model" or "questioned"
#'
#' @return A drop-down list and tabs
#' 
#' @noRd
selectImageServer <- function(id, global, type) {
  shiny::moduleServer(
    id,
    function(input, output, session) { 
      # store current paths, names, image, processed, and profile locally not
      # globally so docs don't carry over when user switched between Known Writing
      # and QD screens
      
      # prevent note: "no visible binding for global variable"
      docname <- NULL
      
      local <- shiny::reactiveValues(current_paths = NULL,
                                     current_names = NULL,
                                     current_image = NULL,
                                     current_processed = NULL,
                                     current_profile = NULL)
      
      # Select doc from drop-down ----
      # NOTE: this is UI that lives inside server so that the heading is hidden
      # if object doesn't exist
      output$current_select <- shiny::renderUI({
        ns <- session$ns
        local$current_paths <- switch(type, "model" = global$known_paths, "questioned" = global$qd_paths)
        shiny::req(local$current_paths)
        local$current_names <- list_names_in_named_vector(local$current_paths)
        shiny::tagList(
          shiny::h1("SUPPORTING MATERIALS"),
          shiny::selectInput(ns("current_select"), 
                             label = switch(type, "model" = "Choose a Known Writing Sample", "questioned" = "Choose a Questioned Document"), 
                             choices = local$current_names),
        )
      })
      
      shiny::observeEvent(input$current_select, {
        local$current_image <- load_image(input$current_select)
        local$current_name <- basename(input$current_select)
        local$current_processed <- load_processed_doc(main_dir = global$main_dir, name = local$current_name, type = type)
      })
      
      # display current doc
      output$current_image <- shiny::renderImage({
        shiny::req(local$current_image)
        
        tmp <- local$current_image %>%
          magick::image_write(tempfile(fileext='png'), format = 'png')
        
        # return a list
        list(src = tmp, contentType = "image/png")
      }, deleteFile = FALSE
      )
      
      # display processed current doc
      output$current_nodes <- shiny::renderPlot({
        shiny::req(local$current_processed)
        handwriter::plotNodes(local$current_processed, nodeSize = 2)
      })
      
      # display writer profile for current doc
      output$current_profile <- shiny::renderPlot({
        shiny::req(local$current_name)
        switch(type, 
               "model" = {
                 shiny::req(global$model)
                 counts <- global$model
               },
               "questioned" = {
                 shiny::req(global$analysis)
                 counts <- global$analysis
               })
        counts$cluster_fill_counts <- counts$cluster_fill_counts %>% 
          dplyr::filter(docname == stringr::str_replace(local$current_name, ".png", ""))
        handwriter::plot_cluster_fill_counts(counts, facet=FALSE)
      })
      
      # NOTE: this is UI that lives inside server so that tabs are hidden
      # if qd_image doesn't exist
      output$current_tabs <- shiny::renderUI({
        ns <- session$ns
        shiny::req(local$current_image)
        shiny::tagList(
          shiny::tabsetPanel(
            shiny::tabPanel("Document",
                            shiny::imageOutput(ns("current_image"))
            ),
            shiny::tabPanel("Processed Document",
                            shiny::HTML("<p>Handwriter processes handwriting by placing <i>nodes</i>, displayed as red dots, at interesections and the ends of lines. Then handwriter 
                                     uses the nodes and a set of rules to break the handwriting into component shapes called <i>graphs</i>. Graphs capture shapes, not necessarily individual letters. 
                                     Graphs might be a part of a letter or contain parts of multiple letters.</p>"),
                            shiny::plotOutput(ns("current_nodes"))
            ),
            shiny::tabPanel("Writer Profile",
                            shiny::HTML("<p>Handwriter groups graphs with similar shapes into <i>clusters</i> and counts the number of graphs from a document 
                      that fall into each cluster. The rate at which a writer produces 
                      graphs in each cluster serves as an estimate of a <i>writer profile</i>.</p>"),
                            shiny::plotOutput(ns("current_profile"))
            )
          )
        )
      })
    })
}