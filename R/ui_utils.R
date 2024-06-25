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
  numericInput(id, label=label, value=1, min=1, max=100)
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
format_sidebar <- function(title, help_text, module = NULL){
  output <- shiny::tagList(
    shiny::tags$h1(class = "responsive-text", title),
    shiny::br(),
    shiny::helpText(help_text),
    module,
    shiny::br(),
  )
  return(output)
}