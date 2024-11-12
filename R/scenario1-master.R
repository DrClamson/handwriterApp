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

scenario1UI <- function(id) {
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
                                                                                                    shiny::fileInput(ns("upload1"), "Document 1", accept = ".png", multiple=FALSE),
                                                                                                    shiny::fileInput(ns("upload2"), "Document 2", accept = ".png", multiple=FALSE),
                                                                                                    shiny::actionButton(class = "btn-sidebar", ns("compare"), "Compare Documents", width = "100%"),
                                                                                                    reportUI(ns('report'))
                                                                                                  )
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
                                         shinycssloaders::withSpinner(shiny::uiOutput(ns("hypotheses_display"))),
                                         shinycssloaders::withSpinner(shiny::uiOutput(ns("limitations_display"))),
                                         shinycssloaders::withSpinner(shiny::uiOutput(ns("profiles_display"))),
                                         shinycssloaders::withSpinner(shiny::uiOutput(ns("slr_display")))
                         ),
      )
    )
    )
  )
}


scenario1Server <- function(id){
  shiny::moduleServer(
    id,
    function(input, output, session){
      
      # ON / OFF BUTTON FOR RESULTS DISPLAY ----
      display <- shiny::reactiveValues(show = FALSE)
      
      # graphs
      graphs <- shiny::reactiveValues(sample1 = NULL,
                                      sample2 = NULL)
      
      # clusters
      clusters <- shiny::reactiveValues(sample1 = NULL,
                                        sample2 = NULL)
      
      # LOAD ----
      sample1 <- shiny::reactive({
        cat(file=stderr(), "sample1 reactive \n")
        
        # turn off slr display
        display$show = FALSE
        
        # reset clusters
        clusters$sample1 <- NULL
        
        fix_upload_name(input$upload1)
      }) %>% 
        shiny::bindEvent(input$upload1)
      
      sample2 <- shiny::reactive({
        cat(file=stderr(), "sample2 reactive \n")
        
        # turn off slr display
        display$show = FALSE
        
        # reset clusters
        clusters$sample1 <- NULL
        
        fix_upload_name(input$upload2)
      }) %>% 
        shiny::bindEvent(input$upload2)
      
      template_plot <- shiny::reactive({
        x <- list()
        x$datapath <- system.file(file.path("extdata", "images", "template.png"), package = "handwriterApp")
        return(x)
      })
      
      # calculate slr
      slr_df <- shiny::reactive({
        shiny::req(sample1(), sample2())
        
        cat(file=stderr(), "slr_df reactive \n")
        
        # turn on slr display
        display$show = TRUE
        
        # handwriterRF::calculate_slr() deletes the contents of tempdir() >
        # comparison before the function terminates, but the app needs the
        # cluster assignments to plot the writer profiles, so use tempdir() >
        # comparison1 as the project directory.
        
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
      # display handwriting samples
      output$samples_display <- shiny::renderUI({
        shiny::req(sample1(), sample2())
        ns <- session$ns
        
        cat(file=stderr(), "render samples UI \n")
        
        shiny::tagList(
          shiny::h1("HANDWRITING SAMPLES"),
          shiny::br(),
          shiny::fluidRow(shiny::column(width=6, singleImageUI(ns("sample1"))),
                          shiny::column(width=6, singleImageUI(ns("sample2")))),
        )
      })
      
      # display hypotheses
      output$hypotheses_display <- shiny::renderUI({
        shiny::req(sample1(), sample2())
        ns <- session$ns
        
        cat(file=stderr(), "render hypotheses UI \n")
        
        shiny::tagList(
          shiny::h1("PROPOSITIONS"),
          shiny::HTML("<p>Handwriter addresses two propositions:
                      <ul>
                        <li> <strong>P<sub>1</sub>: The two documents were written by the same writer.</strong>
                        </li>
                        <li> <strong>P<sub>2</sub>: The two documents were written by different writers.</strong>
                        </li>
                      </ul>
                      </p>"),
          shiny::br()
        )
      })
      
      # display limitations
      output$limitations_display <- shiny::renderUI({
        shiny::req(sample1(), sample2())
        ns <- session$ns
        
        cat(file=stderr(), "render limiations UI \n")
        
        shiny::tagList(
          shiny::h1("LIMITATIONS"),
          shiny::HTML("<p>Handwriter assumes that the documents were written in the writer's natural handwriting and that 
          the writer did not attempt to disguise their handwriting nor forge someone else's handwriting.
                      </p>
                      <p>Handwriter has been tested on handwriting examples from publicly available handwriting databases, where
                      volunteers were asked to copy a writing prompt in their natural handwriting. Error rates on other types of 
                      handwriting samples are unknown.
                      </p>"),
          shiny::br()
        )
      })
      
      
      # display writer profiles
      output$profiles_display <- shiny::renderUI({
        shiny::req(clusters$sample1, clusters$sample2, display$show)
        ns <- session$ns
        
        cat(file=stderr(), "render writer profiles UI \n")
        
        shiny::tagList(
          shiny::h1("WRITER PROFILES"),
          shiny::h2("Graphs"),
          shiny::HTML("<p>Handwriter processes handwriting by converting the writing to black and white, thinning the writing to 1 pixel in width, and following a set of rules
                      to break the writing into component shapes called <i>graphs</i>. Graphs capture shapes, 
                      not necessarily individual letters. Graphs might be a part of a letter or contain parts of multiple letters.</p>"),
          shiny::br(),
          shiny::fluidRow(shiny::column(width=6, graphsUI(ns("sample1_graphs"))),
                          shiny::column(width=6, graphsUI(ns("sample2_graphs")))),
          shiny::h2("Clusters"),
          shiny::HTML("<p>Handwriter use 40 exemplar shapes called <i>clusters</i>. Again, these shapes are not necessarily individual letters. 
                      They might be part of a letter or contain parts of multiple letters. For more information on how these 40 clusters were created, 
                      see <cite><a href='https://onlinelibrary.wiley.com/doi/abs/10.1002/sam.11488'> A clustering method for graphical handwriting 
                      components and statistical writership analysis.</a></cite></p>"),
          shiny::fluidRow(shiny::column(width=12, singleImageUI(ns("template1"), max_height=NULL))),
          shiny::h2("Writer Profiles"),
          shiny::HTML("<p>For each handwriting sample, handwriter assigns each graph to the cluster with the most similar shape. Then
                      for each document, handwriter calculates the proportion of graphs assigned to each cluster. The rate at which a 
                      writer produces graphs in each cluster serves as an estimate of a <i>writer profile</i>.</p>"),
          shiny::br(),
          writerProfileUI(ns("writer_profiles"))
        )
      })
      
      # display similarity score
      output$score <- shiny::renderText({
        shiny::req(slr_df(), display$show)
        
        cat(file=stderr(), "render score \n")
        slr_df()$score
      })
      
      # display slr
      output$slr <- shiny::renderText({
        shiny::req(slr_df(), display$show)
        
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
        shiny::req(slr_df(), display$show)
        cat(file=stderr(), "render interpretation \n")
        handwriterRF::interpret_slr(slr_df())
      })

      # display slr results
      output$slr_display <- shiny::renderUI({
        shiny::req(sample1(), sample2(), slr_df(), display$show)
        ns <- session$ns
        
        cat(file=stderr(), "render slr UI \n")
        
        shiny::tagList(
          shiny::h1("COMPARISON RESULTS"),
          shiny::HTML("<p>Handwriter measures the similarity between the two writer profiles using a random forest trained 
                      on handwriting samples from the <a href='https://data.csafe.iastate.edu/HandwritingDatabase/'>CSAFE Handwriting Database</a>. 
                      The result is a <i>similarity score</i> between the two writer profiles. Next, handwriter calculates the likelihood of observing the similarity score if the 'same writer' hypothesis is true and the likelihood 
                      of observing the similarity score if the 'different writers' hypothesis is true. The <i>score-based likelihood ratio</i> is the ratio of these
                      two likelihoods. For more information, see <cite><a hrer='https://doi.org/10.1002/sam.11566'>Handwriting identification using random forests 
                      and score-based likelihood ratios.</a></cite></p>
                      "
                      ),
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
      
      # REPORT ----
      
      singleImageServer("sample1", sample1)
      singleImageServer("sample2", sample2)
      
      graphsServer("sample1_graphs", sample1, shiny::reactive(graphs$sample1))
      graphsServer("sample2_graphs", sample2, shiny::reactive(graphs$sample2))
      
      singleImageServer("template1", template_plot, title = "Clusters")
      
      writerProfileServer("writer_profiles", sample1, sample2, shiny::reactive(clusters))
      
      # Set up parameters to pass to Rmd document
      params <- shiny::reactive({
        x <- list(
          project_dir = file.path(tempdir(), "comparison1"),
          graphs1 = graphs$sample1,
          graphs2 = graphs$sample2,
          clusters1 = clusters$sample1,
          clusters2 = clusters$sample2,
          slr_df = slr_df()
        )
        return(x)
      })
      reportServer('report', params = params, report_template = "scenario1_report_pdf.Rmd")
      
    }
  )
}
