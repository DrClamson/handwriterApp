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

caseQDSidebarUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(shiny::column(5, set_indices(id = ns("qd_writer_start_char"), label = "Start location")),
                    shiny::column(5, set_indices(id = ns("qd_writer_end_char"), label = "End location"))),
    shiny::helpText(id="qd_docID_help", "Where are the document numbers located in the file names?"),
    shiny::fluidRow(shiny::column(5, set_indices(id = ns("qd_doc_start_char"), label = "Start location")),
                    shiny::column(5, set_indices(id = ns("qd_doc_end_char"), label = "End location"))),
    shiny::helpText("Select the questioned document."),
    shiny::fileInput(ns("qd_upload"), "", accept = ".png", multiple=TRUE)
  )
}

caseQDBodyUI <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    shinycssloaders::withSpinner(shiny::uiOutput(ns("qd_results"))),
    currentImageUI(ns("qd"))
  )
}

caseQDServer <- function(id, global) {
  shiny::moduleServer(
    id,
    function(input, output, session) { 
      shiny::observeEvent(input$qd_upload, {
        global$qd_paths <- input$qd_upload$datapath  # filepaths for temp docs not filepaths on disk
        global$qd_names <- input$qd_upload$name
        
        # copy qd to main directory
        create_dir(file.path(global$main_dir, "data", "questioned_docs"))
        copy_docs_to_project(main_dir = global$main_dir, paths = global$qd_paths, names = global$qd_names, type = "questioned")
        
        # get filepaths and names from main_dir > data > questioned_docs folder
        global$qd_paths <- list_docs(global$main_dir, type = "questioned", filepaths = TRUE)
        # get a named vector where the names are the filenames and the values
        # are the full filepaths to use with the selectInput so the user sees only
        # sees the filenames in the drop-down menu but behind the scenes the app
        # gets the filepaths
        global$qd_names <- list_names_in_named_vector(global$qd_paths)
        
        # analyze
        global$analysis <- handwriter::analyze_questioned_documents(main_dir = global$main_dir,
                                                                    questioned_docs = file.path(global$main_dir, "data", "questioned_docs"),
                                                                    model = global$model,
                                                                    num_cores = 1,
                                                                    writer_indices = c(input$qd_writer_start_char, input$qd_writer_end_char),
                                                                    doc_indices = c(input$qd_doc_start_char, input$qd_doc_end_char))
        
      })
      
      # Display analysis ----
      # NOTE: this is UI that lives inside server so that the heading is hidden
      # if analysis doesn't exist
      output$qd_analysis <- shiny::renderTable({
        shiny::req(global$analysis)
        make_posteriors_df(global$analysis)
      })
      output$qd_results <- shiny::renderUI({
        ns <- session$ns
        shiny::req(global$analysis)
        shiny::tagList(
          shiny::h3("Evaluation Results"),
          shiny::HTML("<p>The table shows the posterior probability of writership for each questioned document and each known writer. Each
                      column corresponds to a questioned document and each row corresponds to a known writer. The posterior probability of 
                      writership in each column sums to 100%.</p>"),
          shiny::tableOutput(ns("qd_analysis")),
          shiny::br()
        )
      })
      
      currentImageServer("qd", global, "questioned")
    }
  )
}