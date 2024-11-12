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


#' Report Module UI
#'
#' Creates a button that generates and downloads a report as a PDF file.
#' Parameters from the app are plugged into a report template.
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'   server function
#'
#' @return A PDF file
#' 
#' @noRd
reportUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::downloadButton(class = "btn-sidebar", ns("report"), "Generate report")
  )
}

#' Report Module Server
#'
#' Creates a button that generates and downloads a report as a PDF file.
#' Parameters from the app are plugged into a report template.
#'
#' @param id An ID string that corresponds with the ID used to call the module's
#'   UI function
#' @param params A reactive list of parameters
#' @param report_template Filename of the report template saved in extdata >
#'   report_templates
#'
#' @return A PDF file
#'
#' @noRd
reportServer <- function(id, params, report_template) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      output$report <- shiny::downloadHandler(
        filename = function() {
          'report.pdf'
        },
        content = function(file) {
          rmd_name <- system.file(file.path("extdata", "report_templates"), 
                                  report_template, 
                                  package = "handwriterApp")
          src <- normalizePath(rmd_name)
          
          # Copy the report file to a temporary directory before processing it, in
          # case we don't have write permissions to the current working dir (which
          # can happen when deployed).
          tempReport <- file.path(tempdir(), basename(rmd_name))
          file.copy(src, tempReport, overwrite = TRUE)

          # Knit the document, passing in the 'params' list, and eval it in a
          # child of the global environment (this isolates the code in the document
          # from the code in this app).
          rmarkdown::render(tempReport, 
                            output_file = file,
                            params = params(),
                            envir = new.env(parent = globalenv())
          )
        }
      )
    }
  )
}
