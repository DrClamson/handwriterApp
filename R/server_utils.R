#' Copy Known Writing Samples to Project Directory
#' 
#' When the user selects known writing samples the files are 
#' copied to main_dir > data > model_docs so that all documents
#' used in the analysis are stored in the project folder. The folder
#' name model_docs is used because the handwriter package will use the
#' known writing samples in this folder to fit a Bayesian hierarchical
#' model.
#'
#' @param main_dir A filepath to the project folder
#' @param known_paths The filepaths of the writing samples 
#' @param known_names The filenames of the writing samples
#'
#' @return NULL
#'
#' @noRd
copy_known_files_to_project <- function(main_dir, known_paths, known_names){
  lapply(1:length(known_paths), function(i) {
    file.copy(known_paths[i], file.path(main_dir, "data", "model_docs", known_names[i]))}
  )
}

#' Copy Questioned Document to Project Directory
#' 
#' When the user selects a questioned document, the file is 
#' copied to main_dir > data > questioned_docs so that all documents
#' used in the analysis are stored in the project folder. 
#'
#' @param main_dir A filepath to the project folder
#' @param qd_paths The filepaths of the questioned documents
#' @param qd_names The filenames of the questioned documents
#'
#' @return NULL
#'
#' @noRd
copy_qd_to_project <- function(main_dir, qd_paths, qd_names){
  lapply(1:length(qd_paths), function(i) {
    file.copy(qd_paths[i], file.path(main_dir, "data", "questioned_docs", qd_names[i]))}
  )
}

#' Create a Directory
#'
#' This helper function creates a directory if it doesn't already exist.
#'
#' @param folder A filepath for the new directory
#'
#' @return NULL
#' 
#' @noRd
create_dir <- function(folder){
  if (!dir.exists(folder)){
    dir.create(folder)
  }
}

#' List Handwriting Samples in the model_docs Folder
#' 
#' This helper function lists the known handwriting samples
#' in main_dir > data > model_docs and returns the filenames
#' either in a dataframe or a list.
#'
#' @param main_dir A filepath to the project folder
#' @param output_dataframe TRUE or FALSE. If TRUE, the filenames
#' are retuned in a dataframe. If FALSE, the filenames are returned
#' in a list.
#'
#' @return Filenames in a dataframe or list
#'
#' @noRd
list_model_docs <- function(main_dir, output_dataframe = TRUE){
  if (dir.exists(file.path(main_dir, "data", "model_docs"))){
    docs <- list.files(file.path(main_dir, "data", "model_docs"), pattern = ".png")
    if (output_dataframe){
      return(data.frame('files' = docs))
    } else {
      return(docs)
    }
  }
}

#' Get the Filepath of the Questioned Documents
#' 
#' This helper function returns the full filepath of the questioned 
#' document in main_dir > data > questioned_docs.
#'
#' @param main_dir A filepath to the project folder
#'
#' @return Filename
#' 
#' @noRd
list_qd_paths <- function(main_dir){
  return(list.files(file.path(main_dir, "data", "questioned_docs"), pattern = ".png", full.names = TRUE))
}

#' Get the Filenames of the Questioned Documents
#' 
#' This helper function returns the full filepath of the questioned 
#' document in main_dir > data > questioned_docs.
#'
#' @param main_dir A filepath to the project folder
#'
#' @return Filename
#' 
#' @noRd
list_qd_names <- function(qd_paths){
  if (length(qd_paths) > 0 ) {
    # get names list where names are the filepaths and values are the filenames
    qd_names <- sapply(qd_paths, basename)
    # swap names and values so the names are the filenames and the values are the filepaths
    qd_names <- setNames(names(qd_names), qd_names)
    return(qd_names)
  }
}

#' Load Analysis
#' 
#' This helper function loads the analysis created by 
#' handwriter::analyze_questioned_document and saved as main_dir >
#' data > analysis.rds.
#'
#' @param main_dir A filepath to the project folder
#'
#' @return Analysis as a list
#'
#' @noRd
load_analysis <- function(main_dir){
  if (file.exists(file.path(main_dir, "data", "analysis.rds"))){
    return(readRDS(file.path(main_dir, "data", "analysis.rds")))
  }
}

#' Load Model
#' 
#' This helper function loads the model created by 
#' handwriter::fit_model and saved as main_dir >
#' data > model.rds.
#'
#' @param main_dir A filepath to the project folder
#'
#' @return Model as a list
#'
#' @noRd
load_model <- function(main_dir) {
  if (file.exists(file.path(main_dir, "data", "model.rds"))){
    return(readRDS(file.path(main_dir, "data", "model.rds")))
  }
}

#' Load Processed Questioned Document
#' 
#' This helper function loads the questioned document
#' processed with handwriter::processDocument and saved in
#' main_dir > data > questioned_graphs.
#'
#' @param main_dir A filepath to the project folder
#' @param qd_path Filenpath of the questioned document
#'
#' @return Processed document as a list
#'
#' @noRd
load_processed_qd <- function(main_dir, qd_path){
  graphs <- list.files(file.path(main_dir, "data", "questioned_graphs"), pattern = ".rds", full.names = TRUE)
  qd_name <- stringr::str_replace(basename(qd_path), '.png', "")
  graphs <- graphs[grepl(qd_name, graphs)]
  graphs <- readRDS(graphs)
  return(graphs)
}

#' Load Questioned Document
#' 
#' This helper function loads the questioned document
#' as an image with the magick package.
#'
#' @param qd_path The filepath for the questioned document
#'
#' @return An image
#'
#' @noRd
load_qd <- function(qd_path){
  if (!is.null(qd_path) && file.exists(qd_path)){
    return(magick::image_read(qd_path))
  }
}

#' Make Dataframe of Posterior Probabilities
#' 
#' This helper function formats the posterior probabilities of 
#' writership stored in analysis calculated with 
#' handwriter::analyze_questioned_document. The posterior probabilities
#' are placed in a dataframe with columns Known Writer and Posterior Probability
#' of Writership. The posterior probabilities are formatted as percentages.
#'
#' @param analysis analysis created with handwriter::analyze_questioned_document
#'
#' @return Processed document as a list
#'
#' @noRd
make_posteriors_df <- function(analysis){
  df <- analysis$posterior_probabilities
  
  # Format posterior probabilities as percentage
  qd_columns <- colnames(df)[-1]
  df <- df %>% tidyr::pivot_longer(cols = tidyr::all_of(qd_columns), 
                             names_to = "qd", 
                             values_to = "post_probs")
  df <- df %>% dplyr::mutate(post_probs = paste0(100*post_probs, "%"))
  df <- df %>% tidyr::pivot_wider(names_from = "qd", values_from = "post_probs")
  
  # Change "known_writer" to "Known Writer"
  colnames(df)[1] <- "Known Writer"
  
  return(df)
}

#' Setup Main Directory
#' 
#' This helper function creates directory called "data" inside the main directory.
#' Inside the "data" directory, "questioned_docs," "questioned_graphs," and "model_docs"
#' directories are created. The data object "template" is saved in the "data" directory
#' as template.rds. These directories and the template.rds file are required by the 
#' handwriter package to analyze a questioned document.
#'
#' @param analysis analysis created with handwriter::analyze_questioned_document
#'
#' @return Processed document as a list
#'
#' @noRd
setup_main_dir <- function(main_dir){
    # create directory structure in main directory
    create_dir(file.path(main_dir, "data"))
    create_dir(file.path(main_dir, "data", "questioned_docs"))
    create_dir(file.path(main_dir, "data", "questioned_graphs"))
    create_dir(file.path(main_dir, "data", "model_docs"))
    
    # handwriter requires template.rds to exist in main_dir > data
    saveRDS(template, file.path(main_dir, "data", "template.rds"))
}