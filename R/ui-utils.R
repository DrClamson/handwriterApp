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


#' Set Indices
#' 
#' This helper function creates a numericInput with default value 1, minimum value 1,
#' and max value 100. This function is used in the app to get the start and end characters
#' for the writer and document indices for the known writing samples and the questioned document.
#'
#' @param label The label to use for the numericInput
#'
#' @return numericInput
#'
#' @noRd
set_indices <- function(id, label){
  shiny::numericInput(id, label=label, value=1, min=1, max=100)
}

#' Format Sidebar
#' 
#' This helper function creates is used to set a standard 
#' format for the sidebar in the app. 
#' 
#' @param title The title to appear in the sidebar
#' @param help_text The help text to appear in the sidebar
#' @param module Optional. A moduleUI to include in the sidebar.
#'
#' @return numericInput
#'
#' @noRd
format_sidebar <- function(title, help_text, module = NULL, break_after_module = TRUE){
  output <- shiny::tagList(
    shiny::tags$h1(class = "responsive-text", title),
    shiny::br(),
    shiny::helpText(help_text),
    module,
    if (break_after_module){
      shiny::br()
    }
  )
  return(output)
}