handwriterApp <- function(...){
  # increase maximum allowed file size
  options(shiny.maxRequestSize = 30*1024^2)
  shiny::addResourcePath(prefix = "images", directoryPath = system.file(file.path("extdata", "images"), package = "handwriterApp"))
  
  ui <- shiny::shinyUI({
    shiny::fluidPage(title = "handwriter",
              shinyjs::useShinyjs(),
              shiny::tags$head(
                shiny::tags$link(
                  href = "https://fonts.googleapis.com/css?family=Montserrat:400,500,700,900|Ubuntu:400,500,700",
                  rel = "stylesheet",
                  type = "text/css"
                ),
                # tags$link(rel = "shortcut icon", href = "favicon.png", type = "image/png"),
                # tags$link(rel = "icon", href = "favicon.png", type = "image/png")
              ),
              shiny::includeCSS(system.file("extdata", "styles.css", package = "handwriterApp")),
              shiny::tags$div(id="app-container",
                       shiny::fluidRow(
                         shiny::column(width = 4, shiny::tags$a(target = "_blank", href="https://forensicstats.org", shiny::tags$img(src = "images/CSAFE_Tools_handwriter_cropped.png", height="100px"))),
                         shiny::column(width = 4, shiny::br()),
                         shiny::column(width = 4, shiny::tags$a(target = "_blank", href="https://forensicstats.org", shiny::tags$img(src = "images/handwriter_graphic.png", height="100px"), class="right-float")),
                       ),
                       shiny::tags$div(id="main-content",
                                shiny::navbarPage(
                                  shiny::tags$script(shiny::HTML("var header = $('.navbar > .container-fluid'); header.append('<div style=\"float:right\"><a href=\"https://forensicstats.org\"><img src=\"images/CSAFE-Tools_Stacked_white_cropped.png\" alt=\"alt\" style=\"float:right;width:117px;height:50px;padding-right:5px;\"> </a></div>'); console.log(header)")),
                                  shiny::tabPanel("Home",
                                           innerUI('inner1'),
                                  ),
                                  shiny::tabPanel( 
                                    "About",
                                    shiny::h4(shiny::HTML("CSAFE Tools is a software suite of state-of-the-art statistical libraries designed to assist practitioners in analyzing forensic data. This work was developed in collaboration with the Center for Statistics and Applications in Forensic Evidence (CSAFE) at Iowa State University and Omni Analytics Group. These procedures are fully open-source and transparent. For more details on the underlying code, please see the <a href='https://github.com/OAITI/bulletmatcher' target='_blank'>GitHub repository</a> for the companion R package.")),
                                    shiny::br(), 
                                    shiny::br(),
                                    shiny::h4(shiny::HTML("This software is an implementation of...")),
                                    shiny::h4(shiny::HTML("This application will walk through the steps...")),
                                    shiny::hr()
                                  ),
                                  shiny::tabPanel("Instructions",),
                                  shiny::tabPanel("Contact",)
                                )))
    )  
  })
  
  # SERVER ------------------------------------------------------------------
  server <- function(input, output, session) {
    innerServer('inner1')
  }
  
  shiny::shinyApp(ui, server, ...)
}