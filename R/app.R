handwriterApp <- function(...){
  # increase maximum allowed file size
  options(shiny.maxRequestSize = 30*1024^2)
  addResourcePath(prefix = "images", directoryPath = system.file(file.path("extdata", "images"), package = "handwriterApp"))
  
  ui <- shinyUI({
    fluidPage(title = "handwriter",
              useShinyjs(),
              tags$head(
                tags$link(
                  href = "https://fonts.googleapis.com/css?family=Montserrat:400,500,700,900|Ubuntu:400,500,700",
                  rel = "stylesheet",
                  type = "text/css"
                ),
                # tags$link(rel = "shortcut icon", href = "favicon.png", type = "image/png"),
                # tags$link(rel = "icon", href = "favicon.png", type = "image/png")
              ),
              includeCSS(system.file("extdata", "styles.css", package = "handwriterApp")),
              tags$div(id="app-container",
                       fluidRow(
                         column(width = 4, tags$a(target = "_blank", href="https://forensicstats.org", tags$img(src = "images/CSAFE_Tools_handwriter_cropped.png", height="100px"))),
                         column(width = 4, br()),
                         column(width = 4, tags$a(target = "_blank", href="https://forensicstats.org", tags$img(src = "images/handwriter_graphic.png", height="100px"), class="right-float")),
                       ),
                       tags$div(id="main-content",
                                navbarPage(
                                  tags$script(HTML("var header = $('.navbar > .container-fluid'); header.append('<div style=\"float:right\"><a href=\"https://forensicstats.org\"><img src=\"images/CSAFE-Tools_Stacked_white_cropped.png\" alt=\"alt\" style=\"float:right;width:117px;height:50px;padding-right:5px;\"> </a></div>'); console.log(header)")),
                                  tabPanel("Home",
                                           innerUI('inner1'),
                                  ),
                                  tabPanel( 
                                    "About",
                                    h4(HTML("CSAFE Tools is a software suite of state-of-the-art statistical libraries designed to assist practitioners in analyzing forensic data. This work was developed in collaboration with the Center for Statistics and Applications in Forensic Evidence (CSAFE) at Iowa State University and Omni Analytics Group. These procedures are fully open-source and transparent. For more details on the underlying code, please see the <a href='https://github.com/OAITI/bulletmatcher' target='_blank'>GitHub repository</a> for the companion R package.")),
                                    br(), br(),
                                    h4(HTML("This software is an implementation of...")),
                                    h4(HTML("This application will walk through the steps...")),
                                    hr()
                                  ),
                                  tabPanel("Instructions",),
                                  tabPanel("Contact",)
                                ))),
              # # Footer
              # tags$div(id="global-footer",
              #   fluidRow(
              #     column(width = 4,tags$img(src="csafe_tools_blue_h.png", alt="Logo", height = "40px")),
              #     column(width = 4,tags$p("195 Durham Center, 613 Morrill Road, Ames, Iowa, 50011")),
              #     column(width = 4,tags$p("(C) 2023 | All Rights Reserved", class="right-float"))
              #   )
              # )
    )  
  })
  
  # SERVER ------------------------------------------------------------------
  server <- function(input, output, session) {
    
    innerServer('inner1')
    
  }
  
  shinyApp(ui, server, ...)
}