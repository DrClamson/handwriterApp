innerUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::sidebarLayout(shiny::tags$div(id=ns("my-sidebar"),
                           shiny::sidebarPanel(width=3,
                                        shiny::fluidPage(
                                          
                                          # Welcome UI ----
                                          shiny::conditionalPanel(condition="input.prevreport == 'Welcome'",
                                                           ns = shiny::NS(id),
                                                           shiny::div(id = "autonomous",
                                                               format_sidebar(title = "GET STARTED",
                                                                              help_text = "Press the following button to start using the app to compare a questioned document to known writing samples."),
                                                               shiny::actionButton(ns("begin_button"), "Begin")
                                                           ),
                                          ),
                                          
                                          # Setup UI ----
                                          shiny::conditionalPanel(condition="input.prevreport == 'Setup'",
                                                           ns = shiny::NS(id),
                                                           shiny::div(id = "autonomous",
                                                               format_sidebar(title = "SETUP", 
                                                                              help_text = "The app saves files to the main 
                                                                            folder as you analyze a questioned document. Choose 
                                                                            an empty folder to start a new analysis. If you want
                                                                            to continue an analysis, select that folder.",
                                                                              module = maindirUI(ns('maindir1'))),
                                                               shiny::fluidRow(shiny::column(width = 6, shiny::actionButton(ns("setup_back_button"), "Back")), 
                                                                        shiny::column(width = 6, align = "right", shiny::actionButton(ns("setup_next_button"), "Next")))
                                                           ),
                                          ),
                                          
                                          # Known Writing UI ----
                                          shiny::conditionalPanel(condition="input.prevreport == 'Known Writing'",
                                                           ns = shiny::NS(id),
                                                           shiny::div(id = "autonomous",
                                                               format_sidebar(title = "KNOWN WRITING",
                                                                              help_text = "Where are the writer IDs located in the file names?",
                                                                              module = knownSidebarUI(ns('known1'))),
                                                               shiny::fluidRow(shiny::column(width = 6, shiny::actionButton(ns("known_back_button"), "Back")), 
                                                                        shiny::column(width = 6, align = "right", shiny::actionButton(ns("known_next_button"), "Next")))
                                                           ),
                                          ),
                                          
                                          # Questioned Document UI ----
                                          shiny::conditionalPanel(condition="input.prevreport == 'Questioned Document'",
                                                           ns = shiny::NS(id),
                                                           shiny::div(id = "autonomous",
                                                               format_sidebar(title = "QUESTIONED DOCUMENT",
                                                                              help_text = "Where are the writer IDs located in the file names?",
                                                                              module = qdSidebarUI(ns('qd1'))),
                                                               shiny::fluidRow(shiny::column(width = 6, shiny::actionButton(ns("qd_back_button"), "Back")), 
                                                                        shiny::column(width = 6, align = "right", shiny::actionButton(ns("qd_next_button"), "Next")))
                                                           ),
                                          ),
                                          
                                          # Report Button UI ----
                                          shiny::conditionalPanel(condition="input.prevreport == 'Report'",
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
                    shiny::tabsetPanel(id=ns("prevreport"),
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
                                                handwriting, handwriter provides a cutting-edge way to evaluate handwriting samples."),
                                                shiny::br(),
                                ),
                                
                                # Setup Display ----
                                shiny::tabPanel(id = ns("Setup"),
                                                title = "Setup"
                                ),
                                
                                # Known Writing Display ----
                                shiny::tabPanel(id = ns("Known Writing"),
                                                title = "Known Writing",
                                                shinycssloaders::withSpinner(knownBodyUI(ns('known1')))
                                ),
                                
                                # Questioned Document Display ----
                                shiny::tabPanel(id = ns("Questioned Document"), 
                                                title = "Questioned Document",
                                                shinycssloaders::withSpinner(qdBodyUI(ns('qd1')))
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
      shinyjs::disable("setup_next_button")
      shinyjs::disable("known_next_button")
      shinyjs::disable("qd_next_button")
      
      # enable next buttons
      shiny::observe({
        # main_dir needs to be defined
        shiny::req(global$main_dir)
        shinyjs::enable("setup_next_button")
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
      
      # change selected tab in main panel
      shiny::observeEvent(input$begin_button, {shiny::updateTabsetPanel(session, "prevreport", selected = "Setup")})
      shiny::observeEvent(input$setup_next_button, {shiny::updateTabsetPanel(session, "prevreport", selected = "Known Writing")})
      shiny::observeEvent(input$known_next_button, {shiny::updateTabsetPanel(session, "prevreport", selected = "Questioned Document")})
      shiny::observeEvent(input$qd_next_button, {shiny::updateTabsetPanel(session, "prevreport", selected = "Report")})
      
      shiny::observeEvent(input$setup_back_button, {shiny::updateTabsetPanel(session, "prevreport", selected = "Welcome")})
      shiny::observeEvent(input$known_back_button, {shiny::updateTabsetPanel(session, "prevreport", selected = "Setup")})
      shiny::observeEvent(input$qd_back_button, {shiny::updateTabsetPanel(session, "prevreport", selected = "Known Writing")})
      shiny::observeEvent(input$report_back_button, {shiny::updateTabsetPanel(session, "prevreport", selected = "Questioned Document")})
      
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
      
      # MAIN DIRECTORY ----
      maindirServer('maindir1', global)
      
      # KNOWN WRITING ----
      knownServer('known1', global)
      
      # QUESTIONED DOCUMENT ----
      qdServer('qd1', global)
      
      # REPORT ----
      reportServer('report1', global)
      
    }
  )
}