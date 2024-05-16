sidebarLayout(tags$div(id="my-sidebar",
  sidebarPanel(width=3,
    fluidPage(

      ## Welcome Page
      conditionalPanel(condition="input.prevreport == 'Welcome'",
          div(id = "autonomous",
                tags$h1(class = "responsive-text","GET STARTED"),
                br(),
                helpText("Press the following button to start using the app by uploading handwriting samples."),
                br(),
                actionButton("confirm_autonomous", "Begin")
            ),
      ),

      ## Handwriting Sample Select and Manipulate Input 
      conditionalPanel(condition="input.prevreport == 'Questioned Document'",
          fluidRow(column(12, uiOutput("qd_ui")))
      ),
      conditionalPanel(condition="input.prevreport == 'Preview Bullet'",
          uiOutput("prevSelUI"),
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
      ),

      ## Questioned Document RGL Windows
      tabPanel("Questioned Document", 
               # textOutput("test"),
               uiOutput("qd_display")),

      ## Questioned Document RGL Windows
      tabPanel("Preview Bullet", uiOutput("lpreview")),

      ## Comparison Report
      tabPanel("Comparison Report", withSpinner(uiOutput("reportUI")))  
    )
  )
)