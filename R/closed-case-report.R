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

caseReportSidebarUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(shiny::column(12, shiny::downloadButton(ns("report"), "Generate report"), align="center"))
  )
}

caseReportServer <- function(id, global) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      output$report <- shiny::downloadHandler(
        filename = function() {
          'report.pdf'
        },
        content = function(file) {
          rmd_name <- system.file(file.path("extdata", "report_templates"), 
                                  "report_pdf.Rmd", 
                                  package = "handwriterApp")
          src <- normalizePath(rmd_name)
          
          # Copy the report file to a temporary directory before processing it, in
          # case we don't have write permissions to the current working dir (which
          # can happen when deployed).
          tempReport <- file.path(tempdir(), basename(rmd_name))
          file.copy(src, tempReport, overwrite = TRUE)
          
          # Set up parameters to pass to Rmd document
          params <- list(
            main_dir = global$main_dir,
            analysis = global$analysis,
            model = global$model
          )
          
          # Knit the document, passing in the 'params' list, and eval it in a
          # child of the global environment (this isolates the code in the document
          # from the code in this app).
          rmarkdown::render(tempReport, 
                            output_file = file,
                            params = params,
                            envir = new.env(parent = globalenv())
          )
        }
      )
    }
  )
}