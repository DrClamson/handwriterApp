sidebarLayout(tags$div(id="my-sidebar",
  sidebarPanel(width=3,
    fluidPage(

      ## Welcome Page
      conditionalPanel(condition="input.prevreport == 'Welcome'",
                       div(id = "autonomous",
                           tags$h1(class = "responsive-text","GET STARTED"),
                           br(),
                           helpText("Choose a working folder to get started."),
                           br(),
                           shinyDirButton("main_dir", "Working folder", "Upload"),
                           verbatimTextOutput("dir", placeholder = TRUE),
                           br(),
                           actionButton("next_main_dir", "Next")
                       ),
      ),
      
      ## Select Questioned Document
      conditionalPanel(condition="input.prevreport == 'Questioned Document'",
          fluidRow(column(12, uiOutput("qd_ui")))
      ),
      
      ## Select Known Writing Samples
      conditionalPanel(condition="input.prevreport == 'Known Writing'",
          uiOutput("known_ui"),
      ),
      conditionalPanel(condition="input.prevreport == 'Comparison Report'",
          uiOutput("reportSelUI"),
      ),

      ## Bullet Add to Comparison UI
      # conditionalPanel(condition="input.prevreport == 'Questioned Document'",
      #     fluidRow(
      #       column(12,textInput("bul_x3p_name", label="Bullet Name",value="",placeholder="Bullet Name Here ...")),
      #       column(12,actionButton("up_bull", label = "Add Bullet to Comparison List"),align="center")
      #     ),
      #     hr(),
      # ),

      ## Bullet Comparison UI
      # conditionalPanel(condition="input.prevreport == 'Questioned Document'",
      #     fluidRow(
      #       column(12,uiOutput("bull_sel")),
      #       column(12,actionButton("doprocess", label = "Compare Bullets"),align="center")
      #     ),
      # ),

      ## Download Report Button
      conditionalPanel(condition="input.prevreport == 'Comparison Report'",
          uiOutput("reportDownUI"),
      ),
  ))),
  mainPanel(
    tabsetPanel(id="prevreport",

      ## Welcome
      tabPanel("Welcome",
               h3("WELCOME TO HANDWRITER!"),
               p("The handwriter R package compares a questioned document with writing samples from persons of interest. 
                  This prototype demonstrates how our methods calculates the probability that each person of interest wrote the questioned document. 
                  It's a work in progress, evolving through feedback from diverse communities."),
               br(),
               h4("Getting Started"),
               p("Get started by choosing a working folder with the button to the left. The handwriter app will save files to this folder
                 as you analyze a questioned document. The working folder can be empty if you starting a analysis. If you would like to 
                 continue an analysis, select the appropriate folder as the working folder."),
               br()
      ),

      ## Questioned Document 
      tabPanel("Questioned Document", 
               uiOutput("qd_display")),

      ## Known Writing
      tabPanel("Known Writing", 
               uiOutput("known_display")
               ),

      ## Comparison Report
      tabPanel("Comparison Report", withSpinner(uiOutput("reportUI")))  
    )
  )
)