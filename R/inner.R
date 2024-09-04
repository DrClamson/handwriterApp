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

innerUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::sidebarLayout(shiny::tags$div(id=ns("my-sidebar"),
                                         shiny::sidebarPanel(width=3,
                                                             shiny::fluidPage(
                                                               
                                                               # Welcome UI ----
                                                               shiny::conditionalPanel(condition="input.screen == 'Welcome'",
                                                                                       ns = shiny::NS(id),
                                                                                       shiny::div(id = "autonomous",
                                                                                                  format_sidebar(title = "GET STARTED",
                                                                                                                 help_text = "Start using handwriter to compare questioned documents to known writing samples. See a demo with 
                                                                                                                 example data or simulate casework and analyze your handwriting samples."),
                                                                                                  shiny::fluidRow(shiny::column(width = 3, shiny::actionButton(ns("demo_button"), "Demo")), 
                                                                                                                  shiny::column(width = 9, align = "right", shiny::actionButton(ns("case_button"), align="right", "Casework Simulation")))
                                                                                       ),
                                                               ),
                                                               
                                                               # Demo UI ----
                                                               shiny::conditionalPanel(condition="input.screen == 'Demo Preview'",
                                                                                       ns = shiny::NS(id),
                                                                                       shiny::div(id = "autonomous",
                                                                                                  shiny::includeHTML(system.file(file.path("extdata", "HTML"), "demo_preview.html", package = "handwriterApp")),
                                                                                                  shiny::fluidRow(shiny::column(width = 3, shiny::actionButton(ns("demo_preview_back_button"), "Back")), 
                                                                                                                  shiny::column(width = 9, align = "right", shiny::actionButton(ns("demo_preview_next_button"), "Next")))
                                                                                       ),
                                                               ),
                                                               
                                                               # Demo Known UI ----
                                                               shiny::conditionalPanel(condition="input.screen == 'Demo Known'",
                                                                                       ns = shiny::NS(id),
                                                                                       shiny::div(id = "autonomous",
                                                                                                  format_sidebar(title = "KNOWN WRITING",
                                                                                                                 help_text = "Estimate writer profiles from the known writing samples and fit a statistical model to the writer profiles.",
                                                                                                                 module = demoKnownSidebarUI(ns("demo_known")),
                                                                                                                 break_after_module = TRUE),
                                                                                                  shiny::fluidRow(shiny::column(width = 3, shiny::actionButton(ns("demo_known_back_button"), "Back")), 
                                                                                                                  shiny::column(width = 9, align = "right", shiny::actionButton(ns("demo_known_next_button"), "Next")))
                                                                                       ),
                                                               ),
                                                               
                                                               # Demo QD UI ----
                                                               shiny::conditionalPanel(condition="input.screen == 'Demo QD'",
                                                                                       ns = shiny::NS(id),
                                                                                       shiny::div(id = "autonomous",
                                                                                                  format_sidebar(title = "QUESTIONED DOCUMENTS",
                                                                                                                 help_text = "Estimate writer profiles from the questioned documents. Use the statistical model to estimate the posterior probabilities that each POI wrote a questioned document.",
                                                                                                                 module = demoQDSidebarUI(ns("demo_qd")),
                                                                                                                 break_after_module = TRUE),
                                                                                                  shiny::fluidRow(shiny::column(width = 3, shiny::actionButton(ns("demo_qd_back_button"), "Back")),
                                                                                                                  shiny::column(width = 9, align = "right", shiny::actionButton(ns("demo_qd_next_button"), "Finish")))
                                                                                       ),
                                                               ),
                                                               
                                                               # Setup Requirements UI ----
                                                               shiny::conditionalPanel(condition="input.screen == 'Case Requirements'",
                                                                                       ns = shiny::NS(id),
                                                                                       shiny::includeHTML(system.file(file.path("extdata", "HTML"), "case_requirements.html", package = "handwriterApp")),
                                                                                       shiny::fluidRow(shiny::column(width = 6, shiny::actionButton(ns("case_requirements_back_button"), "Back")), 
                                                                                                       shiny::column(width = 6, align = "right", shiny::actionButton(ns("case_requirements_next_button"), "Next")))
                                                                                       
                                                               ),
                                                               
                                                               # Setup Files UI ----
                                                               shiny::conditionalPanel(condition="input.screen == 'Case Files'",
                                                                                       ns = shiny::NS(id),
                                                                                       shiny::includeHTML(system.file(file.path("extdata", "HTML"), "case_files.html", package = "handwriterApp")),
                                                                                       shiny::fluidRow(shiny::column(width = 6, shiny::actionButton(ns("case_files_back_button"), "Back")), 
                                                                                                       shiny::column(width = 6, align = "right", shiny::actionButton(ns("case_files_next_button"), "Next")))
                                                                                       
                                                               ),
                                                               
                                                               # Setup Project UI ----
                                                               shiny::conditionalPanel(condition="input.screen == 'Case Project'",
                                                                                       ns = shiny::NS(id),
                                                                                       shiny::div(id = "autonomous",
                                                                                                  format_sidebar(title = "PROJECT FOLDER", 
                                                                                                                 help_text = "Handwriter saves files to a project 
                                                                            folder on your computer as you analyze a questioned document. Choose 
                                                                            an empty folder to start a new analysis. If you want
                                                                            to continue an analysis, select that folder.",
                                                                                                                 module = caseMaindirUI(ns('case_maindir'))),
                                                                                                  shiny::fluidRow(shiny::column(width = 6, shiny::actionButton(ns("case_project_back_button"), "Back")), 
                                                                                                                  shiny::column(width = 6, align = "right", shiny::actionButton(ns("case_project_next_button"), "Next")))
                                                                                       ),
                                                               ),
                                                               
                                                               # Known Writing UI ----
                                                               shiny::conditionalPanel(condition="input.screen == 'Case Known'",
                                                                                       ns = shiny::NS(id),
                                                                                       shiny::div(id = "autonomous",
                                                                                                  format_sidebar(title = "KNOWN WRITING",
                                                                                                                 help_text = "Where are the writer IDs located in the file names?",
                                                                                                                 module = caseKnownSidebarUI(ns('case_known')),
                                                                                                                 break_after_module = FALSE),
                                                                                                  shiny::fluidRow(shiny::column(width = 6, shiny::actionButton(ns("case_known_back_button"), "Back")), 
                                                                                                                  shiny::column(width = 6, align = "right", shiny::actionButton(ns("case_known_next_button"), "Next")))
                                                                                       ),
                                                               ),
                                                               
                                                               # Questioned Document UI ----
                                                               shiny::conditionalPanel(condition="input.screen == 'Case Questioned'",
                                                                                       ns = shiny::NS(id),
                                                                                       shiny::div(id = "autonomous",
                                                                                                  format_sidebar(title = "QUESTIONED DOCUMENT",
                                                                                                                 help_text = "Where are the writer IDs located in the file names?",
                                                                                                                 module = caseQDSidebarUI(ns('case_qd')),
                                                                                                                 break_after_module = FALSE),
                                                                                                  shiny::fluidRow(shiny::column(width = 6, shiny::actionButton(ns("case_qd_back_button"), "Back")), 
                                                                                                                  shiny::column(width = 6, align = "right", shiny::actionButton(ns("case_qd_next_button"), "Next")))
                                                                                       ),
                                                               ),
                                                               
                                                               # Report Button UI ----
                                                               shiny::conditionalPanel(condition="input.screen == 'Case Report'",
                                                                                       ns = shiny::NS(id),
                                                                                       shiny::div(id = "autonomous",
                                                                                                  format_sidebar(title = "REPORT",
                                                                                                                 help_text = "Download the report."),
                                                                                                  shiny::fluidRow(shiny::column(width = 3, shiny::actionButton(ns("case_report_back_button"), "Back")), 
                                                                                                                  shiny::column(width = 9, align = "right", caseReportSidebarUI(ns('case_report'))))
                                                                                       ),
                                                               ),
                                                             ))),
                         shiny::mainPanel(
                           shiny::tabsetPanel(id=ns("screen"),
                                              type = "hidden",
                                              
                                              # Welcome Display ----
                                              shiny::tabPanel(id = ns("Welcome"),
                                                              title = "Welcome",
                                                              shiny::h3("WELCOME TO HANDWRITER!"),
                                                              shiny::p("Unlock the power of handwriting analysis with handwriter. 
                                                This tool is designed to assist forensic examiners by analyzing handwritten 
                                                documents against a closed set of potential writers. It determines the probability 
                                                that each writer wrote the document. Whether you are a forensic document examiner, 
                                                legal professional, academic, or simply curious about how statistics are applied to 
                                                handwriting, handwriter provides an automated way to evaluate handwriting samples."),
                                                              shiny::br(),
                                              ),
                                              
                                              # Demo Display ----
                                              shiny::tabPanel(id = ns("Demo Preview"),
                                                              title = "Demo Preview",
                                                              shinycssloaders::withSpinner(demoPreviewBodyUI(ns('demo_preview')))
                                              ),
                                              
                                              # Demo Known Display ----
                                              shiny::tabPanel(id = ns("Demo Known"),
                                                              title = "Demo Known",
                                                              shinycssloaders::withSpinner(demoKnownBodyUI(ns('demo_known')))
                                              ),
                                              
                                              # Demo QD Display ----
                                              shiny::tabPanel(id = ns("Demo QD"),
                                                              title = "Demo QD",
                                                              shinycssloaders::withSpinner(demoQDBodyUI(ns('demo_qd')))
                                              ),
                                              
                                              # Setup Requirements Display ----
                                              shiny::tabPanel(id = ns("Case Requirements"),
                                                              title = "Case Requirements",
                                              ),
                                              
                                              # Setup Files Display ----
                                              shiny::tabPanel(id = ns("Case Files"),
                                                              title = "Case Files",
                                              ),
                                              
                                              # Setup Project Display ----
                                              shiny::tabPanel(id = ns("Case Project"),
                                                              title = "Case Project",
                                              ),
                                              
                                              # Known Writing Display ----
                                              shiny::tabPanel(id = ns("Case Known"),
                                                              title = "Case Known",
                                                              shinycssloaders::withSpinner(caseKnownBodyUI(ns('case_known')))
                                              ),
                                              
                                              # Questioned Document Display ----
                                              shiny::tabPanel(id = ns("Case Questioned"), 
                                                              title = "Case Questioned",
                                                              shinycssloaders::withSpinner(caseQDBodyUI(ns('case_qd')))
                                              ),
                                              
                                              # Comparison Report Display ----
                                              shiny::tabPanel(id = ns("Case Report"),
                                                              title = "Case Report",
                                              )  
                           )
                         )
    )
  )
}


innerServer <- function(id){
  shiny::moduleServer(
    id,
    function(input, output, session){
      # NEXT BUTTONS ----
      # disable next buttons at start
      shinyjs::disable("demo_known_next_button")
      shinyjs::disable("case_project_next_button")
      shinyjs::disable("case_known_next_button")
      shinyjs::disable("case_qd_next_button")
      
      # enable next buttons
      shiny::observe({
        # main_dir needs to be defined
        shiny::req(global$main_dir)
        shinyjs::enable("case_project_next_button")
      })
      shiny::observe({
        # model needs to be loaded
        shiny::req(global$model)
        shinyjs::enable("case_known_next_button")
        shinyjs::enable("demo_known_next_button")
      })
      shiny::observe({
        # analysis needs to be loaded
        shiny::req(global$analysis)
        shinyjs::enable("case_qd_next_button")
      })
      
      # demo next buttons
      shiny::observeEvent(input$demo_button, {shiny::updateTabsetPanel(session, "screen", selected = "Demo Preview")})
      shiny::observeEvent(input$demo_preview_next_button, {shiny::updateTabsetPanel(session, "screen", selected = "Demo Known")})
      shiny::observeEvent(input$demo_known_next_button, {shiny::updateTabsetPanel(session, "screen", selected = "Demo QD")})
      shiny::observeEvent(input$demo_qd_next_button, {shiny::updateTabsetPanel(session, "screen", selected = "Welcome")})
      
      # casework next buttons
      shiny::observeEvent(input$case_button, {shiny::updateTabsetPanel(session, "screen", selected = "Case Requirements")})
      shiny::observeEvent(input$case_requirements_next_button, {shiny::updateTabsetPanel(session, "screen", selected = "Case Files")})
      shiny::observeEvent(input$case_files_next_button, {shiny::updateTabsetPanel(session, "screen", selected = "Case Project")})
      shiny::observeEvent(input$case_project_next_button, {shiny::updateTabsetPanel(session, "screen", selected = "Case Known")})
      shiny::observeEvent(input$case_known_next_button, {shiny::updateTabsetPanel(session, "screen", selected = "Case Questioned")})
      shiny::observeEvent(input$case_qd_next_button, {shiny::updateTabsetPanel(session, "screen", selected = "Case Report")})
      
      # demo back buttons
      shiny::observeEvent(input$demo_preview_back_button, {shiny::updateTabsetPanel(session, "screen", selected = "Welcome")})
      shiny::observeEvent(input$demo_known_back_button, {shiny::updateTabsetPanel(session, "screen", selected = "Demo Preview")})
      shiny::observeEvent(input$demo_qd_back_button, {shiny::updateTabsetPanel(session, "screen", selected = "Demo Known")})
      
      # casework back buttons
      shiny::observeEvent(input$case_requirements_back_button, {shiny::updateTabsetPanel(session, "screen", selected = "Welcome")})
      shiny::observeEvent(input$case_files_back_button, {shiny::updateTabsetPanel(session, "screen", selected = "Case Requirements")})
      shiny::observeEvent(input$case_project_back_button, {shiny::updateTabsetPanel(session, "screen", selected = "Case Files")})
      shiny::observeEvent(input$case_known_back_button, {shiny::updateTabsetPanel(session, "screen", selected = "Case Project")})
      shiny::observeEvent(input$case_qd_back_button, {shiny::updateTabsetPanel(session, "screen", selected = "Case Known")})
      shiny::observeEvent(input$case_report_back_button, {shiny::updateTabsetPanel(session, "screen", selected = "Case Questioned")})
      
      # STORAGE ----
      global <- shiny::reactiveValues(
        analysis = NULL,
        known_names = NULL,
        known_paths = NULL,
        main_dir = NULL,
        model = NULL,
        qd_names = NULL,
        qd_paths = NULL
      )
      
      # Reset storage
      shiny::observeEvent(input$demo_button, {
        reset_app(global)
        delete_demo_dir()
      })
      
      # Reset storage
      shiny::observeEvent(input$case_button, {
        reset_app(global)
        delete_demo_dir()
      })
      
      # Reset storage and empty temp > demo directory
      shiny::observeEvent(input$demo_qd_next_button, {
        reset_app(global)
        delete_demo_dir()
      })
      

      
      # DEMO PREVIEW ----
      demoPreviewServer('demo_preview', global)
      
      # DEMO KNOWN ----
      demoKnownServer('demo_known', global)
      
      # DEMO QD ----
      demoQDServer('demo_qd', global)
      
      # MAIN DIRECTORY ----
      caseMaindirServer('case_maindir', global)
      
      # KNOWN WRITING ----
      caseKnownServer('case_known', global)
      
      # QUESTIONED DOCUMENT ----
      caseQDServer('case_qd', global)
      
      # REPORT ----
      caseReportServer('case_report', global)
      
    }
  )
}