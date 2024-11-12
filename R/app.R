# The 'handwriterApp' R package performs writership analysis of handwritten
# documents. Copyright (C) 2024 Iowa State University of Science and Technology
# on behalf of its Center for Statistics and Applications in Forensic Evidence
#
# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# this program.  If not, see <https://www.gnu.org/licenses/>.

#' Handwriter Application
#' 
#' Lauch a 'shiny' application for 'handwriter'.
#'
#' @param ... Other arguments passed on to 'onStart', 'options',
#'   'uiPattern', or 'enableBookmarking' of 'shiny::shinyApp'
#'
#' @return No return value, called to launch 'shiny' app
#'
#' @name handwriterApp
#' @rdname handwriterApp
#' @keywords Shiny
#' @export
#' @param ... Optional arguments passed to shiny::shinyApp
#' @examples
#' \dontrun{
#' handwriterApp()
#' }
#' 
#' @return A Shiny app
handwriterApp <- function(...){
  # increase maximum allowed file size
  options(shiny.maxRequestSize = 30*1024^2)
  shiny::addResourcePath(prefix = "images", directoryPath = system.file(file.path("extdata", "images"), package = "handwriterApp"))
  
  ui <- shiny::shinyUI({
    shiny::fluidPage(title = "handwriter",
                     shinyjs::useShinyjs(),
                     # use styles.css
                     shiny::includeCSS(system.file("extdata", "styles.css", package = "handwriterApp")),
                     # use Montserrat
                     shiny::tags$head(
                       shiny::tags$link(
                         href = "https://fonts.googleapis.com/css?family=Montserrat:400,500,700,900|Ubuntu:400,500,700",
                         rel = "stylesheet",
                         type = "text/css"
                       ),
                     ),
                     
                     # navigation bar
                     shiny::tags$div(id="app-container",
                                     # header
                                     shiny::fluidRow(
                                       shiny::column(width = 4, shiny::tags$a(target = "_blank", href="https://forensicstats.org", shiny::tags$img(src = "images/CSAFE_Tools_handwriter_cropped.png", height="100px"))),
                                       shiny::column(width = 4, shiny::br()),
                                       shiny::column(width = 4, shiny::tags$a(target = "_blank", href="https://forensicstats.org", shiny::tags$img(src = "images/handwriter_graphic.png", height="100px"), class="right-float")),
                                     ),
                                     
                                     shiny::tags$div(id="main-content",
                                                     shiny::navbarPage(id="my-navbar",
                                                                       
                                                                       # CSAFE Tools logo on navigation bar
                                                                       shiny::tags$script(shiny::HTML("var header = $('.navbar > .container-fluid'); header.append('<div style=\"float:right\"><a href=\"https://forensicstats.org\"><img src=\"images/CSAFE-Tools_Stacked_white_cropped.png\" alt=\"alt\" style=\"float:right;width:117px;height:50px;padding-right:5px;\"> </a></div>'); console.log(header)")),
                                                                       
                                                                       # navigation bar
                                                                       shiny::tabPanel(
                                                                         "Home",
                                                                         # NOTE: Because the actionButtons on the home page are placed inside cards, 
                                                                         # placing the home page inside a module becomes quite complicated.
                                                                         shiny::tags$div(id="indent-home",
                                                                                         shiny::fluidPage(
                                                                                           shiny::tags$h1("WELCOME TO HANDWRITER"),
                                                                                           shiny::tags$body(
                                                                                             shiny::tags$p("Unlock the power of handwriting analysis with handwriter. 
                                                                                                           This tool is designed to assist forensic examiners by analyzing 
                                                                                                           handwritten documents. Whether you are a forensic document examiner, legal professional, 
                                                                                                           academic, or simply curious about how statistics are applied to handwriting, 
                                                                                                           handwriter provides an automated way to evaluate handwriting samples.")), 
                                                                                           shiny::br(),
                                                                                           shiny::actionButton(class = "scenario-btn", "get_started", "Get Started"),
                                                                                           shiny::br(),
                                                                                           shiny::br()
                                                                                         )
                                                                         )
                                                                       ),
                                                                       
                                                                       shiny::tabPanel(
                                                                         "Scenarios",
                                                                         # NOTE: Because the actionButtons on the home page are placed inside cards, 
                                                                         # placing the home page inside a module becomes quite complicated.
                                                                         shiny::tags$div(id="indent-home",
                                                                                         shiny::fluidPage(
                                                                                           shiny::tags$h1("SCENARIOS"),
                                                                                           shiny::tags$h2("Choose the scenario that best describes your handwriting samples"),

                                                                                           bslib::layout_column_wrap(
                                                                                             width = 1/2,
                                                                                             bslib::card(class="scenario",
                                                                                                         bslib::card_header(shiny::tags$h2("SCENARIO 1"),
                                                                                                                            shiny::tags$body(shiny::tags$i("Compare a questioned document to another handwritten document."))),
                                                                                                         shiny::hr(),
                                                                                                         bslib::card_body(class = "scenario-body",
                                                                                                                          shiny::tags$p(shiny::tags$b("Requirements:")),
                                                                                                                          shiny::tags$ul(shiny::tags$li(shiny::tags$b("Questioned Document:"), "From an unknown writer."), 
                                                                                                                                         shiny::tags$li(shiny::tags$b("Comparison Document:"), "From a known or unknown writer.")),
                                                                                                                          shiny::tags$p(shiny::tags$b("Result:"), "A score-based likelihood ratio that expresses the support of the evidence in favor of the samples having been written by the same writer or different writers.")),
                                                                                                         shiny::hr(),
                                                                                                         shiny::tags$div(
                                                                                                           class = "text-center",  # Bootstrap class for centering
                                                                                                           shiny::actionButton(class = "scenario-btn",
                                                                                                                               "scenario1_button", "Scenario 1", width = "50%")
                                                                                                         )
                                                                                             ),
                                                                                             bslib::card(class="scenario",
                                                                                                         bslib::card_header(shiny::tags$h2("SCENARIO 2"),
                                                                                                                            shiny::tags$body(shiny::tags$i("Compare a questioned document to a group of known handwriting samples."))),
                                                                                                         shiny::hr(),
                                                                                                         bslib::card_body(shiny::tags$p(shiny::tags$b("Requirements:")),
                                                                                                                          shiny::tags$ul(shiny::tags$li(shiny::tags$b("Questioned Document:"), "From an unknown author."), 
                                                                                                                                         shiny::tags$li(shiny::tags$b("Additional Documents:"), "Three known writing samples from each writer in a group of potential writers. The questioned document MUST have been written by someone in this group")),
                                                                                                                          shiny::tags$p(shiny::tags$b("Result:"), "The posterior probability that each potential writer wrote the questioned document.")),
                                                                                                         shiny::hr(),
                                                                                                         shiny::tags$div(
                                                                                                           class = "text-center",  # Bootstrap class for centering
                                                                                                           shiny::actionButton(class = "scenario-btn",
                                                                                                                               "scenario2_button", "Scenario 2", width = "50%")
                                                                                                         )
                                                                                             ),
                                                                                           ),
                                                                                           shiny::br(),
                                                                                         )
                                                                         )
                                                                       ),
                                                                       
                                                                       shiny::tabPanel(
                                                                         "Scenario 1",
                                                                         scenario1UI('scen1'),
                                                                       ),
                                                                       
                                                                       shiny::tabPanel(
                                                                         "Scenario 2",
                                                                         scenario2UI('scen2'),
                                                                       ),
                                                                       
                                                                       shiny::navbarMenu("More",
                                                                                         shiny::tabPanel( 
                                                                                           "About",
                                                                                           shiny::includeHTML(system.file(file.path("extdata", "HTML"), "about.HTML", package = "handwriterApp"))
                                                                                         ),
                                                                                         
                                                                                         shiny::tabPanel( 
                                                                                           "Permitted Use",
                                                                                           shiny::includeHTML(system.file(file.path("extdata", "HTML"), "permitted_use.HTML", package = "handwriterApp"))
                                                                                         ),
                                                                                         
                                                                                         shiny::tabPanel(
                                                                                           "Contact",
                                                                                           shiny::includeHTML(system.file(file.path("extdata", "HTML"), "contact.HTML", package = "handwriterApp"))
                                                                                         ),
                                                                                         
                                                                                         shiny::tabPanel(
                                                                                           "License",
                                                                                           shiny::includeHTML(system.file(file.path("extdata", "HTML"), "license.HTML", package = "handwriterApp"))
                                                                                         )
                                                                       )
                                                                       
                                                                       
                                                     ))),
                     # footer
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
    
    # NOTE: Because the actionButtons on the home page are placed inside cards, 
    # placing the home page inside a module becomes quite complicated.
    
    # Switch to Scenarios tab
    shiny::observeEvent(input$get_started, {
      shiny::updateNavbarPage(session, "my-navbar", selected = "Scenarios")
    })
    
    # Switch to Scenario 1 tab
    shiny::observeEvent(input$scenario1_button, {
      shiny::updateNavbarPage(session, "my-navbar", selected = "Scenario 1")
    })
    
    # Switch to Scenario 1 tab
    shiny::observeEvent(input$scenario1_button, {
      shiny::updateNavbarPage(session, "my-navbar", selected = "Scenario 1")
    })
    
    # Switch to Scenario 2 tab
    shiny::observeEvent(input$scenario2_button, {
      shiny::updateNavbarPage(session, "my-navbar", selected = "Scenario 2")
    })
    
    scenario1Server('scen1')
    scenario2Server('scen2')
  }
  
  shiny::shinyApp(ui, server, ...)
}
