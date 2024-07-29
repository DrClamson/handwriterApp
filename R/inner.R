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
                                                                                                                  shiny::column(width = 9, align = "right", shiny::actionButton(ns("casework_button"), align="right", "Casework Simulation")))
                                                                                       ),
                                                               ),
                                                               
                                                               # Demo UI ----
                                                               shiny::conditionalPanel(condition="input.screen == 'Demo Preview'",
                                                                                       ns = shiny::NS(id),
                                                                                       shiny::div(id = "autonomous",
                                                                                                  shiny::includeHTML(system.file(file.path("extdata", "HTML"), "demo_preview.html", package = "handwriterApp")),
                                                                                                  shiny::fluidRow(shiny::column(width = 3, shiny::actionButton(ns("demo_preview_back_button"), "Back")), 
                                                                                                                  shiny::column(width = 9, align = "right", shiny::actionButton(ns("demo_preview_next_button"), "Estimate Writer Profiles")))
                                                                                       ),
                                                               ),
                                                               
                                                               # Demo Known UI ----
                                                               shiny::conditionalPanel(condition="input.screen == 'Demo Known'",
                                                                                       ns = shiny::NS(id),
                                                                                       shiny::div(id = "autonomous",
                                                                                                  # shiny::includeHTML(system.file(file.path("extdata", "HTML"), "demo.html", package = "handwriterApp")),
                                                                                                  h3("Demo Known"),
                                                                                                  shiny::fluidRow(shiny::column(width = 3, shiny::actionButton(ns("demo_known_back_button"), "Back")), 
                                                                                                                  shiny::column(width = 9, align = "right", shiny::actionButton(ns("demo_known_next_button"), "Analyze Questioned Docs")))
                                                                                       ),
                                                               ),
                                                               
                                                               # Setup Requirements UI ----
                                                               shiny::conditionalPanel(condition="input.screen == 'Requirements'",
                                                                                       ns = shiny::NS(id),
                                                                                       shiny::includeHTML(system.file(file.path("extdata", "HTML"), "setup_requirements.html", package = "handwriterApp")),
                                                                                       shiny::fluidRow(shiny::column(width = 6, shiny::actionButton(ns("requirements_back_button"), "Back")), 
                                                                                                       shiny::column(width = 6, align = "right", shiny::actionButton(ns("requirements_next_button"), "Next")))
                                                                                       
                                                               ),
                                                               
                                                               # Setup Files UI ----
                                                               shiny::conditionalPanel(condition="input.screen == 'Files'",
                                                                                       ns = shiny::NS(id),
                                                                                       shiny::includeHTML(system.file(file.path("extdata", "HTML"), "setup_files.html", package = "handwriterApp")),
                                                                                       shiny::fluidRow(shiny::column(width = 6, shiny::actionButton(ns("files_back_button"), "Back")), 
                                                                                                       shiny::column(width = 6, align = "right", shiny::actionButton(ns("files_next_button"), "Next")))
                                                                                       
                                                               ),
                                                               
                                                               # Setup Project UI ----
                                                               shiny::conditionalPanel(condition="input.screen == 'Project'",
                                                                                       ns = shiny::NS(id),
                                                                                       shiny::div(id = "autonomous",
                                                                                                  format_sidebar(title = "PROJECT FOLDER", 
                                                                                                                 help_text = "Handwriter saves files to a project 
                                                                            folder on your computer as you analyze a questioned document. Choose 
                                                                            an empty folder to start a new analysis. If you want
                                                                            to continue an analysis, select that folder.",
                                                                                                                 module = caseMaindirUI(ns('case_maindir'))),
                                                                                                  shiny::fluidRow(shiny::column(width = 6, shiny::actionButton(ns("project_back_button"), "Back")), 
                                                                                                                  shiny::column(width = 6, align = "right", shiny::actionButton(ns("project_next_button"), "Next")))
                                                                                       ),
                                                               ),
                                                               
                                                               # Known Writing UI ----
                                                               shiny::conditionalPanel(condition="input.screen == 'Known Writing'",
                                                                                       ns = shiny::NS(id),
                                                                                       shiny::div(id = "autonomous",
                                                                                                  format_sidebar(title = "KNOWN WRITING",
                                                                                                                 help_text = "Where are the writer IDs located in the file names?",
                                                                                                                 module = caseKnownSidebarUI(ns('case_known')),
                                                                                                                 break_after_module = FALSE),
                                                                                                  shiny::fluidRow(shiny::column(width = 6, shiny::actionButton(ns("known_back_button"), "Back")), 
                                                                                                                  shiny::column(width = 6, align = "right", shiny::actionButton(ns("known_next_button"), "Next")))
                                                                                       ),
                                                               ),
                                                               
                                                               # Questioned Document UI ----
                                                               shiny::conditionalPanel(condition="input.screen == 'Questioned Document'",
                                                                                       ns = shiny::NS(id),
                                                                                       shiny::div(id = "autonomous",
                                                                                                  format_sidebar(title = "QUESTIONED DOCUMENT",
                                                                                                                 help_text = "Where are the writer IDs located in the file names?",
                                                                                                                 module = caseQDSidebarUI(ns('case_qd')),
                                                                                                                 break_after_module = FALSE),
                                                                                                  shiny::fluidRow(shiny::column(width = 6, shiny::actionButton(ns("qd_back_button"), "Back")), 
                                                                                                                  shiny::column(width = 6, align = "right", shiny::actionButton(ns("qd_next_button"), "Next")))
                                                                                       ),
                                                               ),
                                                               
                                                               # Report Button UI ----
                                                               shiny::conditionalPanel(condition="input.screen == 'Report'",
                                                                                       ns = shiny::NS(id),
                                                                                       shiny::div(id = "autonomous",
                                                                                                  format_sidebar(title = "REPORT",
                                                                                                                 help_text = "Download the report."),
                                                                                                  shiny::fluidRow(shiny::column(width = 3, shiny::actionButton(ns("report_back_button"), "Back")), 
                                                                                                                  shiny::column(width = 9, align = "right", reportSidebarUI(ns('report1'))))
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
                                                              # shinycssloaders::withSpinner(demoPreviewBodyUI(ns('demo1')))
                                              ),
                                              
                                              # Setup Requirements Display ----
                                              shiny::tabPanel(id = ns("Requirements"),
                                                              title = "Requirements",
                                              ),
                                              
                                              # Setup Files Display ----
                                              shiny::tabPanel(id = ns("Files"),
                                                              title = "Files",
                                              ),
                                              
                                              # Setup Project Display ----
                                              shiny::tabPanel(id = ns("Project"),
                                                              title = "Project",
                                              ),
                                              
                                              # Known Writing Display ----
                                              shiny::tabPanel(id = ns("Known Writing"),
                                                              title = "Known Writing",
                                                              shinycssloaders::withSpinner(caseKnownBodyUI(ns('case_known')))
                                              ),
                                              
                                              # Questioned Document Display ----
                                              shiny::tabPanel(id = ns("Questioned Document"), 
                                                              title = "Questioned Document",
                                                              shinycssloaders::withSpinner(caseQDBodyUI(ns('case_qd')))
                                              ),
                                              
                                              # Comparison Report Display ----
                                              shiny::tabPanel(id = ns("Report"),
                                                              title = "Report",
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
      shinyjs::disable("project_next_button")
      shinyjs::disable("known_next_button")
      shinyjs::disable("qd_next_button")
      
      # enable next buttons
      shiny::observe({
        # main_dir needs to be defined
        shiny::req(global$main_dir)
        shinyjs::enable("project_next_button")
      })
      shiny::observe({
        # model needs to be loaded
        shiny::req(global$model)
        shinyjs::enable("known_next_button")
      })
      shiny::observe({
        # analysis needs to be loaded
        shiny::req(global$analysis)
        shinyjs::enable("qd_next_button")
      })
      
      # demo next buttons
      shiny::observeEvent(input$demo_button, {shiny::updateTabsetPanel(session, "screen", selected = "Demo Preview")})
      shiny::observeEvent(input$demo_preview_next_button, {shiny::updateTabsetPanel(session, "screen", selected = "Demo Known")})
    
      # casework next buttons
      shiny::observeEvent(input$casework_button, {shiny::updateTabsetPanel(session, "screen", selected = "Requirements")})
      shiny::observeEvent(input$requirements_next_button, {shiny::updateTabsetPanel(session, "screen", selected = "Files")})
      shiny::observeEvent(input$files_next_button, {shiny::updateTabsetPanel(session, "screen", selected = "Project")})
      shiny::observeEvent(input$project_next_button, {shiny::updateTabsetPanel(session, "screen", selected = "Known Writing")})
      shiny::observeEvent(input$known_next_button, {shiny::updateTabsetPanel(session, "screen", selected = "Questioned Document")})
      shiny::observeEvent(input$qd_next_button, {shiny::updateTabsetPanel(session, "screen", selected = "Report")})
      
      # demo back buttons
      shiny::observeEvent(input$demo_preview_back_button, {shiny::updateTabsetPanel(session, "screen", selected = "Welcome")})
      shiny::observeEvent(input$demo_known_back_button, {shiny::updateTabsetPanel(session, "screen", selected = "Demo Preview")})
      
      # casework back buttons
      shiny::observeEvent(input$requirements_back_button, {shiny::updateTabsetPanel(session, "screen", selected = "Welcome")})
      shiny::observeEvent(input$files_back_button, {shiny::updateTabsetPanel(session, "screen", selected = "Requirements")})
      shiny::observeEvent(input$project_back_button, {shiny::updateTabsetPanel(session, "screen", selected = "Files")})
      shiny::observeEvent(input$known_back_button, {shiny::updateTabsetPanel(session, "screen", selected = "Project")})
      shiny::observeEvent(input$qd_back_button, {shiny::updateTabsetPanel(session, "screen", selected = "Known Writing")})
      shiny::observeEvent(input$report_back_button, {shiny::updateTabsetPanel(session, "screen", selected = "Questioned Document")})
      
      # STORAGE ----
      global <- shiny::reactiveValues(
        analysis = NULL,
        known_docs = NULL,
        main_dir = NULL,
        model = NULL,
        qd_image = NULL,
        qd_names = NULL,
        qd_paths = NULL,
      )
      
      # DEMO ----
      demoPreviewServer('demo_preview', global)
      
      # MAIN DIRECTORY ----
      caseMaindirServer('case_maindir', global)
      
      # KNOWN WRITING ----
      caseKnownServer('case_known', global)
      
      # QUESTIONED DOCUMENT ----
      caseQDServer('case_qd', global)
      
      # REPORT ----
      reportServer('report1', global)
      
    }
  )
}