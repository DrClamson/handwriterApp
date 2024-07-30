caseReportSidebarUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(shiny::column(12, shiny::downloadButton(ns("report"), "Generate report"), align="center"))
  )
}

caseReportServer <- function(id, global) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      output$report <- shiny::downloadHandler(
        filename = function() {
          'report.pdf'
        },
        content = function(file) {
          rmd_name <- system.file(file.path("extdata", "report_templates"), 
                                  "report_pdf.Rmd", 
                                  package = "handwriterApp")
          src <- normalizePath(rmd_name)
          
          # Copy the report file to a temporary directory before processing it, in
          # case we don't have write permissions to the current working dir (which
          # can happen when deployed).
          tempReport <- file.path(tempdir(), basename(rmd_name))
          file.copy(src, tempReport, overwrite = TRUE)
          
          # Set up parameters to pass to Rmd document
          params <- list(
            main_dir = global$main_dir,
            analysis = global$analysis,
            model = global$model
          )
          
          # Knit the document, passing in the `params` list, and eval it in a
          # child of the global environment (this isolates the code in the document
          # from the code in this app).
          rmarkdown::render(tempReport, 
                            output_file = file,
                            params = params,
                            envir = new.env(parent = globalenv())
          )
        }
      )
    }
  )
}