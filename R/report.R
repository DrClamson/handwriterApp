reportSidebarUI <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(ns('format'), 'Document format', c('PDF', 'Word', 'HTML'),
                 inline = TRUE),
    fluidRow(column(12, downloadButton(ns("report"), "Generate report"), align="center"))
  )
}

reportServer <- function(id, global) {
  moduleServer(
    id,
    function(input, output, session) {
      output$report <- downloadHandler(
        filename = function() {
          paste('report', 
                sep = '.', 
                switch(input$format, 
                       PDF = 'pdf', 
                       HTML = 'html', 
                       Word = 'docx'))
        },
        content = function(file) {
          rmd_name <- switch(input$format,
                             PDF = 'report_pdf.Rmd', 
                             HTML = 'report_html.Rmd', 
                             Word = 'report_word.Rmd')
          src <- normalizePath(rmd_name)
          
          # Copy the report file to a temporary directory before processing it, in
          # case we don't have write permissions to the current working dir (which
          # can happen when deployed).
          tempReport <- file.path(tempdir(), rmd_name)
          file.copy(src, tempReport, overwrite = TRUE)
          
          # Set up parameters to pass to Rmd document
          params <- list(
            main_dir = global$main_dir,
            analysis = global$analysis,
            known_docs = global$known_docs,
            model = global$model,
            qd_doc = global$doc
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