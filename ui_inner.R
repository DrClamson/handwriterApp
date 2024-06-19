sidebarLayout(tags$div(id="my-sidebar",
                       sidebarPanel(width=3,
                                    fluidPage(
                                      
                                      # Welcome UI ----
                                      conditionalPanel(condition="input.prevreport == 'Welcome'",
                                                       div(id = "autonomous",
                                                           format_sidebar(title = "GET STARTED",
                                                                          help_text = "Press the following button to 
                                                                          start using the app to compare a questioned 
                                                                          document to known writing samples."),
                                                           actionButton("begin_button", "Begin")
                                                       ),
                                      ),
                                      
                                      # Setup UI ----
                                      conditionalPanel(condition="input.prevreport == 'Setup'",
                                                       div(id = "autonomous",
                                                           format_sidebar(title = "SETUP", 
                                                                          help_text = "The app saves files to the main 
                                                                          folder as you analyze a questioned document. Choose 
                                                                          an empty folder to start a new analysis. If you want
                                                                          to continue an analysis, select that folder.",
                                                                          module = maindirUI('maindir1')),
                                                           actionButton("setup_next_button", "Next")
                                                       ),
                                      ),
                                      
                                      # Known Writing UI ----
                                      conditionalPanel(condition="input.prevreport == 'Known Writing'",
                                                       div(id = "autonomous",
                                                           format_sidebar(title = "KNOWN WRITING",
                                                                          help_text = "Where are the writer IDs 
                                                                          located in the file names?",
                                                                          module = knownSidebarUI('known1')),
                                                           actionButton("known_next_button", "Next"),
                                                       ),
                                      ),
                                      
                                      # Questioned Document UI ----
                                      conditionalPanel(condition="input.prevreport == 'Questioned Document'",
                                                       div(id = "autonomous",
                                                           tags$h1(class = "responsive-text", "QUESTIONED DOCUMENT"),
                                                           helpText(id="qd_writerID_help", "Where are the writer IDs located in the file names?"),
                                                           fluidRow(column(5, set_indices(id = "qd_writer_start_char", label = "Start location")),
                                                                    column(5, set_indices(id = "qd_writer_end_char", label = "End location"))),
                                                           helpText(id="qd_docID_help", "Where are the document numbers located in the file names?"),
                                                           fluidRow(column(5, set_indices(id = "qd_doc_start_char", label = "Start location")),
                                                                    column(5, set_indices(id = "qd_doc_end_char", label = "End location"))),
                                                           helpText("Select the questioned document."),
                                                           fileInput("qd_upload", "", accept = ".png", multiple=FALSE),
                                                           br(),
                                                           actionButton("qd_next_button", "Next"),
                                                       ),
                                      ),
                                      
                                      # Report Button UI ----
                                      conditionalPanel(condition="input.prevreport == 'Report'",
                                                       div(id = "autonomous",
                                                           tags$h1(class = "responsive-text", "REPORT"),
                                                           br(),
                                                           radioButtons('format', 'Document format', c('PDF', 'Word', 'HTML'),
                                                                        inline = TRUE),
                                                           fluidRow(column(12, downloadButton("report", "Generate report"), align="center"))
                                                       ),
                                      ),
                                    ))),
              mainPanel(
                tabsetPanel(id="prevreport",
                            
                            # Welcome Display ----
                            tabPanel("Welcome",
                                     h3("WELCOME TO HANDWRITER!"),
                                     p("Unlock the power of handwriting analysis with handwriter. Made to aid forensic examiners in their work,
                 this tool analyzes a handwritten document against a closed set of potential writers to determine the probability
                 that each writer wrote the document. Whether you are a forensic document examiner, legal professional, academic,
                 or simply curious about how statistics are applied to handwriting, handwriter provides a cutting-edge way to 
                 evaluate handwriting samples."),
                                     br(),
                            ),
                            
                            # Setup Display ----
                            tabPanel("Setup",
                            ),
                            
                            # Known Writing Display ----
                            tabPanel("Known Writing", 
                                     knownBodyUI('known1')
                                     # withSpinner(uiOutput("known_display"))
                            ),
                            
                            # Questioned Document Display ----
                            tabPanel("Questioned Document", 
                                     withSpinner(uiOutput("qd_display"))
                            ),
                            
                            # Comparison Report Display ----
                            tabPanel("Report", 
                                     # withSpinner(uiOutput("report_display"))
                            )  
                )
              )
)