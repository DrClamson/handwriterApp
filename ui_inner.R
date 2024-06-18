sidebarLayout(tags$div(id="my-sidebar",
  sidebarPanel(width=3,
    fluidPage(

      # Welcome UI ----
      conditionalPanel(condition="input.prevreport == 'Welcome'",
                       div(id = "autonomous",
                           tags$h1(class = "responsive-text","GET STARTED"),
                           br(),
                           helpText("Press the following button to start using the app to compare a questioned document to known writing samples."),
                           br(),
                           actionButton("begin_button", "Begin")
                       ),
      ),
      
      # Setup UI ----
      conditionalPanel(condition="input.prevreport == 'Setup'",
                       div(id = "autonomous",
                           tags$h1(class = "responsive-text","SETUP"),
                           helpText(id='setup_help', "The app saves files to the main folder as you analyze a questioned document. Choose an empty folder to start a new analysis. If you want to continue an analysis, select that folder."),
                           maindirUI('maindir1'),
                           br(),
                           actionButton("setup_next_button", "Next")
                       ),
      ),
      
      # Known Writing UI ----
      conditionalPanel(condition="input.prevreport == 'Known Writing'",
                       div(id = "autonomous",
                           tags$h1(class = "responsive-text","KNOWN WRITING"),
                           helpText(id="known_writerID_help", "Where are the writer IDs located in the file names?"),
                           fluidRow(column(5, set_indices(id = "known_writer_start_char", label = "Start location")),
                                    column(5, set_indices(id = "known_writer_end_char", label = "End location"))),
                           helpText(id="known_docID_help", "Where are the document numbers located in the file names?"),
                           fluidRow(column(5, set_indices(id = "known_doc_start_char", label = "Start location")),
                                    column(5, set_indices(id = "known_doc_end_char", label = "End location"))),
                           helpText("Select three known writing samples from each person of interest."),
                           fileInput("known_upload", "", accept = ".png", multiple=TRUE),
                           br(),
                           actionButton("known_next_button", "Next"),
                       ),
      ),
      bsPopover(id="known_writerID_help", title="Help",
                content=paste("The app needs to know which persons of interest submitted which known writing samples.",
                              "The sample file names need to contain writer IDs and sample numbers, and the writer IDs and sample numbers need to be in the same location in every file name.",
                              "For example, if you have two persons of interest with writer IDs 0101 and 0107 respectively and both people submitted two writing samples, the file names could be<br>",
                              "<ul><li>w0101_sample1.png</li><li>w0101_sample2.png</li><li>w0107_sample1.png</li><li>w0101_sample2.png</li></ul>",
                              "The writer IDs are located in characters 2-5 and the sample numbers are located in characters 7-13."),
                "right",
                options = list(container = "body")),
      bsPopover(id="known_docID_help", title="Help",
                content=paste("The app needs to know which persons of interest submitted which known writing samples.",
                              "The sample file names need to contain writer IDs and sample numbers, and the writer IDs and sample numbers need to be in the same location in every file name.",
                              "For example, if you have two persons of interest with writer IDs 0101 and 0107 respectively and both people submitted two writing samples, the file names could be<br>",
                              "<ul><li>w0101_sample1.png</li><li>w0101_sample2.png</li><li>w0107_sample1.png</li><li>w0101_sample2.png</li></ul>",
                              "The writer IDs are located in characters 2-5 and the sample numbers are located in characters 7-13."),
                "right",
                options = list(container = "body")),
      
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
               withSpinner(uiOutput("known_display"))
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