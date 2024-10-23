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
                                                                                                  shiny::actionButton(ns("compare"), "Compare Documents", width = "100%")
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
                                         shinycssloaders::withSpinner(shiny::uiOutput(ns("samples_display"))),
                                         shinycssloaders::withSpinner(shiny::uiOutput(ns("graphs_display"))),
                                         shinycssloaders::withSpinner(shiny::uiOutput(ns("profiles_display"))),
                                         shinycssloaders::withSpinner(shiny::uiOutput(ns("slr_display")))
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
      
      # ON / OFF BUTTON FOR SLR DISPLAY ----
      display <- reactiveValues(show = FALSE)
      
      # graphs
      graphs <- shiny::reactiveValues(sample1 = NULL,
                                      sample2 = NULL)
      
      # clusters
      clusters <- shiny::reactiveValues(sample1 = NULL,
                                        sample2 = NULL)
      
      # LOAD ----
      sample1 <- reactive({
        cat(file=stderr(), "sample1 reactive \n")
        
        # turn off slr display
        display$show = FALSE
        
        # reset clusters
        clusters$sample1 <- NULL
        
        fix_upload_name(input$open_upload1)
      }) %>% 
        shiny::bindEvent(input$open_upload1)
      
      sample2 <- reactive({
        cat(file=stderr(), "sample2 reactive \n")
        
        # turn off slr display
        display$show = FALSE
        
        # reset clusters
        clusters$sample1 <- NULL
        
        fix_upload_name(input$open_upload2)
      }) %>% 
        shiny::bindEvent(input$open_upload2)
      
      # calculate slr
      slr_df <- reactive({
        req(sample1(), sample2())
        
        cat(file=stderr(), "slr_df reactive \n")
        
        # turn on slr display
        display$show = TRUE
        
        # handwriterRF::calculate_slr() deletes the contents of tempdir() >
        # comparison before the function terminates, but the app needs the
        # cluster assignments to plot the writer profiles, so use tempdir() >
        # comparison1 as the project directory and delete this folder and its
        # contents when the clear_open button is clicked.
        
        unlink(file.path(tempdir(), "comparison1"), recursive = TRUE)
        
        slr <- handwriterRF::calculate_slr(
          sample1_path = sample1()$datapath,
          sample2_path = sample2()$datapath,
          project_dir = file.path(tempdir(), "comparison1"))
        
        # load graphs
        graphs$sample1 <- readRDS(file.path(tempdir(), "comparison1", "graphs", stringr::str_replace(basename(sample1()$datapath), ".png", "_proclist.rds")))
        graphs$sample2 <- readRDS(file.path(tempdir(), "comparison1", "graphs", stringr::str_replace(basename(sample2()$datapath), ".png", "_proclist.rds")))
        
        # load clusters
        clusters$sample1 <- readRDS(file.path(tempdir(), "comparison1", "clusters", stringr::str_replace(basename(sample1()$datapath), ".png", ".rds")))
        clusters$sample2 <- readRDS(file.path(tempdir(), "comparison1", "clusters", stringr::str_replace(basename(sample2()$datapath), ".png", ".rds")))
        
        return(slr)
      }) %>% 
        shiny::bindEvent(input$compare)
      
      
      # RENDER ----
      # display similarity score
      output$score <- shiny::renderText({
        req(slr_df(), display$show)
        
        cat(file=stderr(), "render score \n")
        slr_df()$score
      })
      
      # display slr
      output$slr <- shiny::renderText({
        req(slr_df(), display$show)
        
        cat(file=stderr(), "render slr \n")
        
        slr <- slr_df()$slr
        
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
        req(slr_df(), display$show)
        cat(file=stderr(), "render interpretation \n")
        handwriterRF::interpret_slr(slr_df())
      })
      
      # display handwriting samples
      output$samples_display <- shiny::renderUI({
        req(sample1(), sample2())
        ns <- session$ns
        
        cat(file=stderr(), "render samples UI \n")
        
        shiny::tagList(
          shiny::h1("HANDWRITING SAMPLES"),
          shiny::br(),
          shiny::fluidRow(shiny::column(width=6, singleImageBodyUI(ns("sample1"))),
                          shiny::column(width=6, singleImageBodyUI(ns("sample2")))),
        )
      })
      
      # display graphs
      output$graphs_display <- shiny::renderUI({
        req(graphs$sample1, graphs$sample2)
        ns <- session$ns
        
        cat(file=stderr(), "render graphs UI \n")
        
        shiny::tagList(
          shiny::h1("COMPARISON RESULTS"),
          shiny::h2("Graphs"),
          shiny::HTML("<p>Handwriter processes handwriting by placing <i>nodes</i> at interesections and the ends of lines. Then handwriter 
                      uses the nodes and a set of rules to break the handwriting into component shapes called <i>graphs</i>. Graphs capture shapes, 
                      not necessarily individual letters. Graphs might be a part of a letter or contain parts of multiple letters.</p>"),
          shiny::br(),
          shiny::fluidRow(shiny::column(width=6, graphsBodyUI(ns("sample1_graphs"))),
                          shiny::column(width=6, graphsBodyUI(ns("sample2_graphs")))),
        )
      })
      
      # display writer profiles
      output$profiles_display <- shiny::renderUI({
        req(clusters$sample1, clusters$sample2)
        ns <- session$ns
        
        cat(file=stderr(), "render writer profiles UI \n")
        
        shiny::tagList(
          shiny::h2("Writer Profiles"),
          shiny::HTML("<p>Handwriter groups graphs with similar shapes into <i>clusters</i> and counts the number of graphs from a document 
                      that fall into each cluster. The rate at which a writer produces 
                      graphs in each cluster serves as an estimate of a <i>writer profile</i>.</p>"),
          shiny::br(),
          shiny::fluidRow(shiny::column(width=6, writerProfileBodyUI(ns("writer1_profile"))),
                          shiny::column(width=6, writerProfileBodyUI(ns("writer2_profile")))),
        )
      })
      
      # display slr results
      output$slr_display <- shiny::renderUI({
        req(sample1(), sample2(), slr_df(), display$show)
        ns <- session$ns
        
        cat(file=stderr(), "render slr UI \n")
        
        shiny::tagList(
          shiny::h2("Similarity Score"),
          shiny::textOutput(ns("score")),
          shiny::br(),
          shiny::h2("Score-based Likelihood Ratio"),
          shiny::textOutput(ns("slr")),
          shiny::br(),
          shiny::h2("Verbal Interpretation of the Score-based Likelihood Ratio"),
          shiny::textOutput(ns("slr_interpretation")),
          shiny::br(),
          shiny::br()
        )
      })
      
      singleImageServer("sample1", sample1)
      singleImageServer("sample2", sample2)
      
      graphsServer("sample1_graphs", sample1, reactive(graphs$sample1))
      graphsServer("sample2_graphs", sample2, reactive(graphs$sample2))
      
      writerProfileServer("writer1_profile", sample1, reactive(clusters$sample1))
      writerProfileServer("writer2_profile", sample2, reactive(clusters$sample2))
    }
  )
}