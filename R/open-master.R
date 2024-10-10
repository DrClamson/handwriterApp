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
                                                                                                  format_sidebar(title = "WELCOME",
                                                                                                                 help_text = "Start using handwriter to compare two handwritten documents."),
                                                                                                  # shiny::fluidRow(shiny::column(width = 3, shiny::actionButton(ns("demo_button"), "Demo")),
                                                                                                  #                 shiny::column(width = 9, align = "right", shiny::actionButton(ns("case_button"), align="right", "Casework Simulation")))
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
                         
      )
    )
    )
  )
}


openServer <- function(id){
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