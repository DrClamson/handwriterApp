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

openUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::sidebarLayout(shiny::tags$div(id=ns("my-sidebar"),
                                         shiny::sidebarPanel(width=3,
                                                             shiny::fluidPage(
                                                               
                                                               # Welcome UI ----
                                                               shiny::conditionalPanel(condition="input.screen == 'Welcome'",
                                                                                       ns = shiny::NS(id),
                                                                                       shiny::div(id = "autonomous",
                                                                                                  format_sidebar(title = "COMPARE TWO DOCUMENTS",
                                                                                                                 help_text = "Select two handwritten documents to compare. The files must be PNG images.",
                                                                                                                 module = openSampleSidebarUI(ns('samples')),
                                                                                                                 break_after_module = FALSE),
                                                                                                  shiny::hr(),
                                                                                                  shiny::actionButton(ns("clear_open"), "Start New Comparison")
                                                                                       ),
                                                               ),
                                                               
                                                               
                                                             )
                                         )
    ),
    shiny::mainPanel(
      shiny::tabsetPanel(id=ns("screen"),
                         type = "hidden",
                         
                         # Welcome Display ----
                         shiny::tabPanel(id = ns("Welcome"),
                                         title = "Welcome",
                                         shinycssloaders::withSpinner(openSampleBodyUI(ns('samples')))
                         ),
      )
    )
    )
  )
}


openServer <- function(id){
  shiny::moduleServer(
    id,
    function(input, output, session){
      
      # STORAGE ----
      open_global <- shiny::reactiveValues(
        sample1_path = NULL,
        sample2_path = NULL,
        sample1_name = NULL,
        sample2_name = NULL,
        slr_df = NULL
      )
      
      shiny::observeEvent(input$clear_open, {
        # delete comparison1 folder and contents in tempdir()
        # unlink(file.path(tempdir(), "comparison1"), recursive = TRUE)
        
        open_global <- shiny::reactiveValues(
          sample1_path = NULL,
          sample2_path = NULL,
          sample1_name = NULL,
          sample2_name = NULL,
          slr_df = NULL
        )
        
        # reset module
        shinyjs::reset('samples-open_upload1')
        shinyjs::reset('samples-open_upload2')
      })
      
      if (!open_global$hide) {
        openSampleServer('samples', open_global)
      } 
    }
  )
}