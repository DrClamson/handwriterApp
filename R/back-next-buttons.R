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


#' Back and Next Buttons Module UI
#' 
#' A pair of side-by-side "back" and "next" buttons.
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'   server function
#' @param label_back The text label for the "back" button
#' @param label_next The text label for the "next" button
#'
#' @return Action buttons
#'
#' @noRd
backNextUI <- function(id, label_back, label_next){
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(shiny::column(6, shiny::actionButton(class = "btn-sidebar", 
                                                         inputId = ns("back"), 
                                                         label = label_back)),
                    shiny::column(6, shiny::actionButton(class = "btn-sidebar", 
                                                         inputId = ns("forward"), 
                                                         label = label_next))
    )
  )
}


#' Back and Next Buttons Module Server
#' 
#' A pair of side-by-side "back" and "next" buttons.
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'   UI function
#' @param parent_session The parent session of backNextServer() is required for
#'   backNextServer() to change the selected tab in the parent session.
#' @param tabs_id The ID of the tabsetPanel in the parent session
#' @param select_back The ID of the tabPanel the "back" button will navigate to
#' @param select_next The ID of the tabPanel the "next" button will navigate to
#'
#' @return Action buttons
#'
#' @noRd
backNextServer <- function(id, parent_session, tabs_id, select_back, select_next) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      shiny::observeEvent(input$back, {
        shiny::updateTabsetPanel(session = parent_session, inputId = tabs_id, selected = select_back)
      })
      
      shiny::observeEvent(input$forward, {
        shiny::updateTabsetPanel(session = parent_session, inputId = tabs_id, selected = select_next)
      })
    }
  )
}
