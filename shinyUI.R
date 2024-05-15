ui <- shinyUI({
  fluidPage(title = "ShoePrintMatcheR",
  tags$head(
        tags$link(
            href = "https://fonts.googleapis.com/css?family=Montserrat:400,500,700,900|Ubuntu:400,500,700",
            rel = "stylesheet",
            type = "text/css"
        ),
        tags$link(rel = "shortcut icon", href = "favicon.png", type = "image/png"),
        tags$link(rel = "icon", href = "favicon.png", type = "image/png")
  ),
  includeCSS("css/styles.css"),
  tags$div(id="app-container",
    tags$a(target = "_blank", href="https://forensicstats.org", tags$img(src = "images/CSAFE-Tools_Horizontal.png", width="300px")),
    tags$div(id="main-content",navbarPage(NULL,
            tabPanel("Home",
                    shinyjs::useShinyjs(),
                    shinyBS:::shinyBSDep,
                    add_busy_spinner(spin = "fading-circle"),
                    #span(textOutput("error"), style="color:red"),
                    tags$head(tags$script(src = "message-handler.js"), 
                              tags$style(HTML("input[type=\"number\"] {width: 80px;}")),
                              tags$style(HTML("hr {border-top: 1px solid #000000;}")),
                              tags$style(HTML('#save_document{background-color:#33ADFF} #save_document:hover{background-color:#3398FF} #save_document{color:white}')),
                              tags$style(HTML('#save_document_extract{background-color:#33ADFF} #save_document_extract:hover{background-color:#3398FF} #save_document_extract{color:white}')),
                              tags$style(HTML('#save_batch{background-color:#33ADFF} #save_batch:hover{background-color:#3398FF} #save_batch{color:white}')),
                              tags$style(HTML('#save_mask{background-color:#33ADFF} #save_mask:hover{background-color:#3398FF} #save_mask{color:white}'))),
                    source(file.path("ui", "home.R"), local = TRUE)$value
                  ),
            tabPanel( 
                      "About",
                      h4(HTML("CSAFE Tools is a software suite of state-of-the-art statistical libraries designed to assist practitioners in analyzing forensic data. This work was developed in collaboration with the Center for Statistics and Applications in Forensic Evidence (CSAFE) at Iowa State University and Omni Analytics Group. These procedures are fully open-source and transparent. For more details on the underlying code, please see the <a href='https://github.com/OAITI/bulletmatcher' target='_blank'>GitHub repository</a> for the companion R package.")),
                      br(), br(),
                      h4(HTML("This software is an implementation of a front-end to the <a href='https://github.com/CSAFE-ISU/bulletr' target='_blank'>bulletr package</a>.")),
                      h4(HTML("This application will walk through the steps used to programmatically determine the probability that two bullets were fired from the same gun. During discharge, as a bullet travels out of the chamber, it is imprinted with a groove signature that is unique to that gun’s barrel. The grooved pattern of a gun’s barrel is so distinct that the striations that are imprinted on a set of fired bullet need only be matched across a small region for there to be statistical confidence of a match; therefore probabilistic comparisons can be made at the bullet land level which represent only one-sixth of a bullet.<br><br>
                                Hare, E., Hofmann, H., and Carriquiry, A., Algorithmic Approaches to Match Degraded Land Impressions. Law, Probability and Risk, mgx018, <a href='https://doi.org/10.1093/lpr/mgx018' target='_blank'>https://doi.org/10.1093/lpr/mgx018</a><br>
                                Hare, E., Hofmann, H., and Carriquiry, A., Automatic Matching of Bullet Land Impressions. Annals of Applied Statistics. doi: 10.1214/17-AOAS1080"
                      )),
                      hr()
            ),
            tabPanel("Instructions",),
            tabPanel("Contact",),
  ))),
  # Footer
  tags$div(id="global-footer",
    fluidRow(
      column(width = 4,tags$img(src="csafe_tools_blue.png", alt="Logo", height = "60px")),
      column(width = 4,tags$p("195 Durham Center, 613 Morrill Road, Ames, Iowa, 50011")),
      column(width = 4,tags$p("(C) 2023 | All Rights Reserved", class="right-float"))
    )
  ))  
})
