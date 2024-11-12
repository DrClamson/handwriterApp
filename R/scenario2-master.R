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

scenario2UI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::sidebarLayout(shiny::tags$div(id=ns("my-sidebar"),
                                         shiny::sidebarPanel(width=3,
                                                             shiny::fluidPage(
                                                               
                                                               # Welcome UI ----
                                                               shiny::conditionalPanel(condition="input.screen == 'Welcome'",
                                                                                       ns = shiny::NS(id),
                                                                                       shiny::div(id = "autonomous",
                                                                                                  format_sidebar(title = "COMPARE QUESTIONED DOCUMENTS TO KNOWN WRITING SAMPLES",
                                                                                                                 help_text = "Compare one or more questioned documents to known writing samples from each writer in a group of potential writers. The questioned document(s) MUST have been written by someone in this group.
                                                                                                                 See a demo with example data or use your own handwriting samples."),
                                                                                                  shiny::tagList(shiny::actionButton(class = "btn-sidebar", ns("demo_button"), "Demo"),
                                                                                                                 shiny::actionButton(class = "btn-sidebar", ns("case_button"), align="right", "Use Your Own Samples"))
                                                                                       ),
                                                               ),
                                                               
                                                               # Demo UI ----
                                                               shiny::conditionalPanel(condition="input.screen == 'Demo Preview'",
                                                                                       ns = shiny::NS(id),
                                                                                       shiny::div(id = "autonomous",
                                                                                                  shiny::includeHTML(system.file(file.path("extdata", "HTML"), "demo_preview.html", package = "handwriterApp")),
                                                                                                  backNextUI(ns("demo_preview"), label_back = "Back", label_next = "Next"),
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
                                                                                                  backNextUI(ns("demo_known"), label_back = "Back", label_next = "Next")
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
                                                                                                  shiny::fluidRow(shiny::column(width = 6, shiny::actionButton(class = "btn-sidebar", ns("demo_qd_back_button"), "Back")),
                                                                                                                  shiny::column(width = 6, align = "right", shiny::actionButton(class = "btn-sidebar", ns("demo_finish"), "Finish")))
                                                                                       ),
                                                               ),
                                                               
                                                               # Setup Requirements UI ----
                                                               shiny::conditionalPanel(condition="input.screen == 'Case Requirements'",
                                                                                       ns = shiny::NS(id),
                                                                                       shiny::includeHTML(system.file(file.path("extdata", "HTML"), "case_requirements.html", package = "handwriterApp")),
                                                                                       backNextUI(ns("case_requirements"), label_back = "Back", label_next = "Next")
                                                                                       
                                                               ),
                                                               
                                                               # Setup Files UI ----
                                                               shiny::conditionalPanel(condition="input.screen == 'Case Files'",
                                                                                       ns = shiny::NS(id),
                                                                                       shiny::includeHTML(system.file(file.path("extdata", "HTML"), "case_files.html", package = "handwriterApp")),
                                                                                       backNextUI(ns("case_files"), label_back = "Back", label_next = "Next")
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
                                                                                                  backNextUI(ns("case_project"), label_back = "Back", label_next = "Next")
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
                                                                                                  backNextUI(ns("case_known"), label_back = "Back", label_next = "Next")
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
                                                                                                  backNextUI(ns("case_QD"), label_back = "Back", label_next = "Next")
                                                                                       ),
                                                               ),
                                                               
                                                               # Report Button UI ----
                                                               shiny::conditionalPanel(condition="input.screen == 'Case Report'",
                                                                                       ns = shiny::NS(id),
                                                                                       shiny::div(id = "autonomous",
                                                                                                  format_sidebar(title = "REPORT",
                                                                                                                 help_text = "Download the report."),
                                                                                                  shiny::actionButton(class = "btn-sidebar", ns("case_report_back_button"), "Back"),
                                                                                                  reportUI(ns('case_report'))
                                                                                       ),
                                                               ),
                                                             ))),
                         shiny::mainPanel(
                           shiny::tabsetPanel(id=ns("screen"),
                                              type = "hidden",
                                              
                                              # Welcome Display ----
                                              shiny::tabPanel(id = ns("Welcome"),
                                                              title = "Welcome",
                                              ),
                                              
                                              # Demo Display ----
                                              shiny::tabPanel(id = ns("Demo Preview"),
                                                              title = "Demo Preview",
                                                              shinycssloaders::withSpinner(demoPreviewUI(ns('demo_preview')))
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


scenario2Server <- function(id){
  shiny::moduleServer(
    id,
    function(input, output, session){
      # NEXT BUTTONS ----
      backNextServer("demo_preview", 
                     parent_session = session, 
                     tabs_id = "screen",
                     select_back = "Welcome", 
                     select_next = "Demo Known")
      
      backNextServer("demo_known", 
                     parent_session = session, 
                     tabs_id = "screen",
                     select_back = "Demo Preview", 
                     select_next = "Demo QD")
      
      backNextServer("case_requirements", 
                     parent_session = session, 
                     tabs_id = "screen",
                     select_back = "Welcome", 
                     select_next = "Case Files")
      
      backNextServer("case_files", 
                     parent_session = session, 
                     tabs_id = "screen",
                     select_back = "Case Requirements", 
                     select_next = "Case Project")
      
      backNextServer("case_project", 
                     parent_session = session, 
                     tabs_id = "screen",
                     select_back = "Case Files", 
                     select_next = "Case Known")
      
      backNextServer("case_known", 
                     parent_session = session, 
                     tabs_id = "screen",
                     select_back = "Case Project", 
                     select_next = "Case Questioned")
      
      backNextServer("case_QD", 
                     parent_session = session, 
                     tabs_id = "screen",
                     select_back = "Case Known", 
                     select_next = "Case Report")
      
      # demo next buttons
      shiny::observeEvent(input$demo_button, {shiny::updateTabsetPanel(session, "screen", selected = "Demo Preview")})
      shiny::observeEvent(input$demo_finish, {shiny::updateTabsetPanel(session, "screen", selected = "Welcome")})
      
      # casework next buttons
      shiny::observeEvent(input$case_button, {shiny::updateTabsetPanel(session, "screen", selected = "Case Requirements")})
      
      # demo back buttons
      shiny::observeEvent(input$demo_qd_back_button, {shiny::updateTabsetPanel(session, "screen", selected = "Demo Known")})
      
      # casework back buttons
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
      shiny::observeEvent(input$demo_finish, {
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
      # Set up parameters to pass to Rmd document
      params <- shiny::reactive({
        x <- list(
        main_dir = global$main_dir,
        analysis = global$analysis,
        model = global$model
        )
        return(x)
      })
      reportServer('case_report', params = params, report_template = "report_pdf.Rmd")
      
    }
  )
}
