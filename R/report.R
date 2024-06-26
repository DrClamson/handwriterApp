reportSidebarUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(shiny::column(12, shiny::downloadButton(ns("report"), "Generate report"), align="center"))
  )
}

reportServer <- function(id, global) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$report <- downloadHandler(
        filename = function() {
          paste0('report_', global$qd_processed$docname, '.pdf')
        },
        content = function(file) {
          rmd_name <- system.file(file.path("extdata", "report_templates"), "report_pdf.Rmd", package = "handwriterApp")
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
            known_docs = global$known_docs,
            model = global$model,
            qd_doc = global$qd_processed
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