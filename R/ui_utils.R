set_indices <- function(id, label){
  numericInput(id, label=label, value=1, min=1, max=100)
}