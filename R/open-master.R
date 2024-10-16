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
                                                                                                  output <- shiny::tagList(
                                                                                                    shiny::tags$h1(class = "responsive-text", "COMPARE TWO DOCUMENTS"),
                                                                                                    shiny::br(),
                                                                                                    shiny::helpText("Select two handwritten documents to compare. The files must be PNG images."),
                                                                                                    shiny::fileInput(ns("open_upload1"), "Document 1", accept = ".png", multiple=FALSE),
                                                                                                    shiny::fileInput(ns("open_upload2"), "Document 2", accept = ".png", multiple=FALSE)
                                                                                                  ),
                                                                                                  shiny::hr(),
                                                                                                  shiny::actionButton(ns("clear_open"), "Start New Comparison", width = "100%")
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
                                         shinycssloaders::withSpinner(shiny::uiOutput(ns("slr_results")))
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
      
      # RESET ----
      shiny::observeEvent(input$clear_open, {
        # handwriterRF::calculate_slr() deletes the contents of tempdir() >
        # comparison before the function terminates, but the app needs the
        # cluster assignments to plot the writer profiles, so use tempdir() >
        # comparison1 as the project directory and delete this folder and its
        # contents when the clear_open button is clicked.
        unlink(file.path(tempdir(), "comparison1"), recursive = TRUE)
        
        # reset global values
        open_global$sample1_path <- NULL
        open_global$sample2_path <- NULL
        open_global$sample1_name <- NULL
        open_global$sample2_name <- NULL
        open_global$slr_df <- NULL  # hides the display on right side of screen
        
        # reset module
        shinyjs::reset('open_upload1')
        shinyjs::reset('open_upload2')
      })
      
      # LOAD ----
      # load sample 1 and get path and name
      shiny::observeEvent(input$open_upload1, {
        # change temp file name assigned by shiny to original file name
        fixed <- fix_upload_name(input$open_upload1)
        
        open_global$sample1_path <- fixed$datapath
        open_global$sample1_name <- fixed$name
      })
      
      # load sample 2, get path and name, and calculate slr
      shiny::observeEvent(input$open_upload2, {
        # reset slr
        open_global$slr_df <- NULL
        
        # change temp file name assigned by shiny to original file name
        fixed <- fix_upload_name(input$open_upload2)
        
        open_global$sample2_path <- fixed$datapath
        open_global$sample2_name <- fixed$name
        
        open_global$slr_df <- handwriterRF::calculate_slr(
          sample1_path = open_global$sample1_path,
          sample2_path = open_global$sample2_path,
          project_dir = file.path(tempdir(), "comparison1"))
      })
      
      # RENDER ----
      # display similarity score
      output$score <- shiny::renderText({
        req(open_global$slr_df)
        
        open_global$slr_df$score
      })
      
      # display slr
      output$slr <- shiny::renderText({
        req(open_global$slr_df)
        
        slr <- open_global$slr_df$slr
        
        if (slr >= 1) {
          # add commas to large numbers
          format(round(slr, 1), big.mark = ",")
        } else if (slr > 0 && slr < 1){
          # round numbers greater than 0 and less than 1 to 3 decimal places
          format(round(slr, 3), nsmall = 2)
        } else {
          slr
        }
      })
      
      # display slr interpretation
      output$slr_interpretation <- shiny::renderText({
        req(open_global$slr_df)
        
        handwriterRF::interpret_slr(open_global$slr_df)
      })
      
      output$slr_results <- shiny::renderUI({
        req(open_global$slr_df)
        ns <- session$ns
        
        shiny::tagList(
          shiny::h3("COMPARISON RESULTS"),
          shiny::br(),
          shiny::h4("Handwriting Samples"),
          shiny::fluidRow(shiny::column(width=6, singleImageBodyUI(ns("sample1"))),
                          shiny::column(width=6, singleImageBodyUI(ns("sample2")))),
          shiny::h4("Writer Profiles"),
          shiny::fluidRow(shiny::column(width=6, writerProfileBodyUI(ns("writer1_profile"))),
                          shiny::column(width=6, writerProfileBodyUI(ns("writer2_profile")))),
          shiny::h4("Similarity Score"),
          shiny::textOutput(ns("score")),
          shiny::br(),
          shiny::h4("Score-based Likelihood Ratio"),
          shiny::textOutput(ns("slr")),
          shiny::br(),
          shiny::h4("Verbal Interpretation of the Score-based Likelihood Ratio"),
          shiny::textOutput(ns("slr_interpretation")),
          shiny::br(),
          shiny::br()
        )
      })
      
      singleImageServer("sample1", open_global$sample1_path, open_global$sample1_name)
      singleImageServer("sample2", open_global$sample2_path, open_global$sample2_name)
      
      writerProfileServer("writer1_profile", open_global$sample1_path, open_global$sample1_name, sample_num = 1)
      writerProfileServer("writer2_profile", open_global$sample2_path, open_global$sample2_name, sample_num = 2)
    }
  )
}