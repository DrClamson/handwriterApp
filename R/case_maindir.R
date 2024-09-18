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

caseMaindirUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinyFiles::shinyDirButton(ns("main_dir"), "Main folder", "Select a folder"),
    shiny::verbatimTextOutput(ns("dir"), placeholder = TRUE)
  )
}

caseMaindirServer <- function(id, global) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      shinyFiles::shinyDirChoose(
        input,
        'main_dir',
        roots = c(home = '~'),
        filetypes = c('', 'txt', 'bigWig', "tsv", "csv", "bw")
      )
      
      dir <- shiny::reactive(input$main_dir)
      
      # display folder path below button
      output$dir <- shiny::renderText({
        global$main_dir
      })
      
      # update main directory to the selected directory
      shiny::observeEvent(ignoreInit = TRUE,
                   eventExpr = {
                     input$main_dir
                   },
                   handlerExpr = {
                     # update main directory
                     if (!"path" %in% names(dir())) {
                       return()
                     }
                     home <- normalizePath("~")
                     global$main_dir <- file.path(home, paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep))
                     
                     # setup main directory or load previous analysis
                     if (length(list.files(global$main_dir)) == 0){
                       # setup directory for new analysis
                       setup_main_dir(global$main_dir)
                     } else {
                       # load files if they exist to continue previously started analysis
                       global$known_paths <- list_docs(global$main_dir, type = "model", filepaths = TRUE)
                       global$known_names <- list_docs(global$main_dir, type = "model", filepaths = FALSE)
                       global$model <- load_model(global$main_dir)
                       global$qd_paths <- list_docs(global$main_dir, type = "questioned", filepaths = TRUE)
                       global$qd_names <- list_names_in_named_vector(global$qd_paths)
                       global$analysis <- load_analysis(global$main_dir)
                     }
                   })
    }
  )
}