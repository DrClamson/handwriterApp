#PREPROCESS TAB

tabPanel("Home", 
         sidebarLayout(
           sidebarPanel(width = 4,
                        h2("GET STARTED"),
                        p("Press the following button to start using the app by uploading handwriting samples."),
                        actionButton("begin", "Begin")
                        ),
           mainPanel(width = 8,
                     span(textOutput("error"), style="color:red"),
                     br(),
                     tabsetPanel(id = "plotset",
                                 tabPanel("Current Document",
                                          br(),
                                          imageOutput("preprocess_plot", brush = brushOpts(id = "preprocess_plot_brush", resetOnNew = TRUE))
                                 ),
                                 tabPanel("Apply Mask",
                                          br(),
                                          fluidRow(
                                            column(width = 2, actionButton("mask", "Mask Area")),
                                            column(width = 2, actionButton("undo_mask", "Undo Last Mask")),
                                            column(width = 2, actionButton("reset_mask", "Remove Mask")),
                                            column(width = 2, downloadButton("save_mask", "Save Mask"))
                                            
                                          ),
                                          hr(),
                                          imageOutput("preprocess_plot_masked", brush = brushOpts(id = "preprocess_plot_brush", resetOnNew = TRUE))
                                 )
                     ),
                     
           )
         ),
)