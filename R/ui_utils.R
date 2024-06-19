set_indices <- function(id, label){
  numericInput(id, label=label, value=1, min=1, max=100)
}

format_sidebar <- function(title, help_text, module = NULL){
  output <- tagList(
    tags$h1(class = "responsive-text", title),
    br(),
    helpText(help_text),
    module,
    br(),
  )
  return(output)
}