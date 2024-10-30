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

homeUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$div(id="indent-home",
                    shiny::fluidPage(
                      shiny::tags$h1("WELCOME TO HANDWRITER"),
                      shiny::tags$body("Unlock the power of handwriting analysis with handwriter. Choose the scenario
                                       that best describes your handwriting samples. Say something here about how this
                                       is for testing purposes only and not to be used in casework?"),
                      shiny::br(),
                      bslib::layout_column_wrap(
                        width = 1/3,
                        bslib::card(class="scenario",
                                    bslib::card_header(shiny::tags$h2("SCENARIO 1"),
                                                       shiny::tags$body(shiny::tags$i("Compare a questioned document to another handwritten document."))),
                                    shiny::hr(),
                                    bslib::card_body(class = "scenario-body",
                                                     shiny::tags$p(shiny::tags$b("Requirements:")),
                                                     shiny::tags$ul(shiny::tags$li(shiny::tags$b("Questioned Document:"), "From an unknown writer."), 
                                                                    shiny::tags$li(shiny::tags$b("Comparison Document:"), "From a known or unknown writer.")),
                                                     shiny::tags$p(shiny::tags$b("Result:"), "A score-based likelihood ratio that expresses the support of the evidence
                                                         in favor of the samples having been written by the same writer or different writers.")),
                                    shiny::hr(),
                                    shiny::tags$div(
                                      class = "text-center",  # Bootstrap class for centering
                                      shiny::actionButton(class = "scenario-btn",
                                                          ns("open_button"), "Scenario 1", width = "50%")
                                    )
                        ),
                        bslib::card(class="scenario",
                          bslib::card_header(shiny::tags$h2("SCENARIO 2"),
                                             shiny::tags$body(shiny::tags$i("Compare a questioned document to a group of known handwriting samples."))),
                          shiny::hr(),
                          bslib::card_body(shiny::tags$p(shiny::tags$b("Requirements:")),
                                           shiny::tags$ul(shiny::tags$li(shiny::tags$b("Questioned Document:"), "From an unknown author."), 
                                                          shiny::tags$li(shiny::tags$b("Additional Documents:"), "Three known writing samples from each writer in a group of potential writers. 
                                                                         The questioned document MUST have been written by someone in this group")),
                                           shiny::tags$p(shiny::tags$b("Result:"), "The posterior probability that each potential writer wrote the questioned document.")),
                          shiny::hr(),
                          shiny::tags$div(
                            class = "text-center",  # Bootstrap class for centering
                            shiny::actionButton(class = "scenario-btn",
                                                "closed-button", "Scenario 2", width = "50%")
                          )
                          ),
                      ),
                      shiny::br(),
                    )
    )
  )
}


homeServer <- function(id){
  shiny::moduleServer(
    id,
    function(input, output, session){
      shiny::observeEvent(input$open_button, {
        shiny::updateNavbarPage(session, "my-navbar", selected = "Open-Set")
      })
    }
  )
}