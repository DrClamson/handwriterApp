set_indices <- function(id, label){
  numericInput(id, label=label, value=1, min=1, max=100)
}

format_sidebar <- function(title, help_text, module = NULL){
  output <- shiny::tagList(
    shiny::tags$h1(class = "responsive-text", title),
    shiny::br(),
    shiny::helpText(help_text),
    module,
    shiny::br(),
  )
  return(output)
}