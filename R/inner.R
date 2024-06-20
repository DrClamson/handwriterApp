innerUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(tags$div(id=ns("my-sidebar"),
                           sidebarPanel(width=3,
                                        fluidPage(
                                          
                                          # Welcome UI ----
                                          conditionalPanel(condition="input.prevreport == 'Welcome'",
                                                           ns = NS(id),
                                                           div(id = "autonomous",
                                                               format_sidebar(title = "GET STARTED",
                                                                              help_text = "Press the following button to start using the app to compare a questioned document to known writing samples."),
                                                               actionButton(ns("begin_button"), "Begin")
                                                           ),
                                          ),
                                          
                                          # Setup UI ----
                                          conditionalPanel(condition="input.prevreport == 'Setup'",
                                                           ns = NS(id),
                                                           div(id = "autonomous",
                                                               format_sidebar(title = "SETUP", 
                                                                              help_text = "The app saves files to the main 
                                                                            folder as you analyze a questioned document. Choose 
                                                                            an empty folder to start a new analysis. If you want
                                                                            to continue an analysis, select that folder.",
                                                                              module = maindirUI(ns('maindir1'))),
                                                               actionButton(ns("setup_next_button"), "Next")
                                                           ),
                                          ),
                                          
                                          # Known Writing UI ----
                                          conditionalPanel(condition="input.prevreport == 'Known Writing'",
                                                           ns = NS(id),
                                                           div(id = "autonomous",
                                                               format_sidebar(title = "KNOWN WRITING",
                                                                              help_text = "Where are the writer IDs located in the file names?",
                                                                              module = knownSidebarUI(ns('known1'))),
                                                               actionButton(ns("known_next_button"), "Next"),
                                                           ),
                                          ),
                                          
                                          # Questioned Document UI ----
                                          conditionalPanel(condition="input.prevreport == 'Questioned Document'",
                                                           ns = NS(id),
                                                           div(id = "autonomous",
                                                               format_sidebar(title = "QUESTIONED DOCUMENT",
                                                                              help_text = "Where are the writer IDs located in the file names?",
                                                                              module = qdSidebarUI(ns('qd1'))),
                                                               actionButton(ns("qd_next_button"), "Next"),
                                                           ),
                                          ),
                                          
                                          # Report Button UI ----
                                          conditionalPanel(condition="input.prevreport == 'Report'",
                                                           ns = NS(id),
                                                           div(id = "autonomous",
                                                               format_sidebar(title = "REPORT",
                                                                              help_text = "Choose a document format and download the report.",
                                                                              module = reportSidebarUI(ns('report1')))
                                                           ),
                                          ),
                                        ))),
                  mainPanel(
                    tabsetPanel(id=ns("prevreport"),
                                
                                # Welcome Display ----
                                tabPanel(id = ns("Welcome"),
                                         title = "Welcome",
                                         h3("WELCOME TO HANDWRITER!"),
                                         p("Unlock the power of handwriting analysis with handwriter. Made to aid forensic examiners in their work,
                   this tool analyzes a handwritten document against a closed set of potential writers to determine the probability
                   that each writer wrote the document. Whether you are a forensic document examiner, legal professional, academic,
                   or simply curious about how statistics are applied to handwriting, handwriter provides a cutting-edge way to 
                   evaluate handwriting samples."),
                                         br(),
                                ),
                                
                                # Setup Display ----
                                tabPanel(id = ns("Setup"),
                                         title = "Setup"
                                ),
                                
                                # Known Writing Display ----
                                tabPanel(id = ns("Known Writing"),
                                         title = "Known Writing",
                                         knownBodyUI(ns('known1'))
                                ),
                                
                                # Questioned Document Display ----
                                tabPanel(id = ns("Questioned Document"), 
                                         title = "Questioned Document",
                                         qdBodyUI(ns('qd1'))
                                ),
                                
                                # Comparison Report Display ----
                                tabPanel(id = ns("Report"),
                                         title = "Report"
                                )  
                    )
                  )
    )
  )
}


innerServer <- function(id){
  moduleServer(
    id,
    function(input, output, session){
      # # NEXT BUTTONS ----
      # # disable next buttons at start
      # shinyjs::disable("setup_next_button")
      # shinyjs::disable("known_next_button")
      # shinyjs::disable("qd_next_button")
      # 
      # # enable next buttons
      # observe({
      #   # main_dir needs to be defined
      #   req(global$main_dir)
      #   shinyjs::enable("setup_next_button")
      # })
      # observe({
      #   # model needs to be loaded
      #   req(global$model)
      #   shinyjs::enable("known_next_button")
      # })
      # observe({
      #   # analysis needs to be loaded
      #   req(global$analysis)
      #   shinyjs::enable("qd_next_button")
      # })
      
      # change selected tab in main panel
      observeEvent(input$begin_button, {updateTabsetPanel(session, "prevreport", selected = "Setup")})
      observeEvent(input$setup_next_button, {updateTabsetPanel(session, "prevreport", selected = "Known Writing")})
      observeEvent(input$known_next_button, {updateTabsetPanel(session, "prevreport", selected = "Questioned Document")})
      observeEvent(input$qd_next_button, {updateTabsetPanel(session, "prevreport", selected = "Report")})
      
      # STORAGE ----
      global <- reactiveValues(
        analysis = NULL,
        known_docs = NULL,
        main_dir = NULL,
        model = NULL,
        qd_image = NULL,
        qd_name = NULL,
        qd_path = NULL,
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