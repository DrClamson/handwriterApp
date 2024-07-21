handwriterApp <- function(...){
  # increase maximum allowed file size
  options(shiny.maxRequestSize = 30*1024^2)
  shiny::addResourcePath(prefix = "images", directoryPath = system.file(file.path("extdata", "images"), package = "handwriterApp"))
  
  ui <- shiny::shinyUI({
    shiny::fluidPage(title = "handwriter",
                     shinyjs::useShinyjs(),
                     shiny::includeCSS(system.file("extdata", "styles.css", package = "handwriterApp")),
                     shiny::tags$head(
                       shiny::tags$link(
                         href = "https://fonts.googleapis.com/css?family=Montserrat:400,500,700,900|Ubuntu:400,500,700",
                         rel = "stylesheet",
                         type = "text/css"
                       ),
                     ),
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
                                                         shiny::includeHTML(system.file(file.path("extdata", "HTML"), "about.HTML", package = "handwriterApp"))
                                                       ),
                                                       shiny::tabPanel(
                                                         "Contact",
                                                         shiny::includeHTML(system.file(file.path("extdata", "HTML"), "contact.HTML", package = "handwriterApp"))
                                                       )
                                                     ))),
                     # Footer
                     shiny::tags$div(id="global-footer",
                              shiny::fluidRow(
                                shiny::column(width = 4, shiny::tags$img(src="images/csafe_tools_blue_h.png", alt="Logo", height = "40px")),
                                shiny::column(width = 4, shiny::tags$p("195 Durham Center, 613 Morrill Road, Ames, Iowa, 50011")),
                                shiny::column(width = 4, shiny::tags$p("(C) 2023 | All Rights Reserved", class="right-float"))
                              )
                     )
    )  
  })
  
  # SERVER ------------------------------------------------------------------
  server <- function(input, output, session) {
    innerServer('inner1')
  }
  
  shiny::shinyApp(ui, server, ...)
}