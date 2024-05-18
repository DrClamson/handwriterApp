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
                           helpText("Choose an empty folder or a folder that contains handwriter analyses."),
                           br(),
                           shinyDirButton("main_dir", "Main folder", "Select a folder"),
                           verbatimTextOutput("dir", placeholder = TRUE),
                           br(),
                           actionButton("setup_next_button", "Next")
                       ),
      ),
      
      # Known Writing UI ----
      conditionalPanel(condition="input.prevreport == 'Known Writing'",
                       div(id = "autonomous",
                           helpText("Select the known writing samples."),
                           br(),
                           fileInput("known_upload", "Select files", accept = ".png", multiple=TRUE),
                           br(),
                           actionButton("known_next_button", "Next")
                       ),
      ),
      
      # Questioned Document UI ----
      conditionalPanel(condition="input.prevreport == 'Questioned Document'",
                       div(id = "autonomous",
                           helpText("Select the questioned document."),
                           br(),
                           fileInput("qd_upload", "", accept = ".png", multiple=FALSE),
                           br(),
                           actionButton("qd_next_button", "Next"),
                       ),
      ),
      
      # Report UI ----
      conditionalPanel(condition="input.prevreport == 'Comparison Report'",
          uiOutput("reportSelUI"),
      ),

      # Report Button UI ----
      conditionalPanel(condition="input.prevreport == 'Comparison Report'",
          uiOutput("reportDownUI"),
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
               p("Choose a main folder with the button to the left. The app will save files to this folder
                 as you analyze a questioned document. The main folder can be empty if you are starting a new analysis. 
                 If you would like to continue an analysis, select the appropriate folder as the main folder."),
               br()
      ),
      
      # Known Writing Display ----
      tabPanel("Known Writing", 
               uiOutput("known_display")
      ),

      # Questioned Document Display ----
      tabPanel("Questioned Document", 
               uiOutput("qd_display")
      ),

      # Comparison Report Display ----
      tabPanel("Comparison Report", 
               withSpinner(uiOutput("reportUI"))
      )  
    )
  )
)