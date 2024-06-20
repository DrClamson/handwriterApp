copy_known_files_to_project <- function(main_dir, known_paths, known_names){
  lapply(1:length(known_paths), function(i) {
    file.copy(known_paths[i], file.path(main_dir, "data", "model_docs", known_names[i]))}
  )
}

copy_qd_to_project <- function(main_dir, qd_path, qd_name){
  file.copy(qd_path, file.path(main_dir, "data", "questioned_docs", qd_name))
}

create_dir <- function(folder){
  if (!dir.exists(folder)){
    dir.create(folder)
  }
}

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

list_qd <- function(main_dir){
  if (length(list.files(file.path(main_dir, "data", "questioned_docs"), pattern = ".png")) == 1){
    return(list.files(file.path(main_dir, "data", "questioned_docs"), pattern = ".png", full.names = TRUE)[1])
  }
}

load_analysis <- function(main_dir){
  if (file.exists(file.path(main_dir, "data", "analysis.rds"))){
    return(readRDS(file.path(main_dir, "data", "analysis.rds")))
  }
}

load_model <- function(main_dir) {
  if (file.exists(file.path(main_dir, "data", "model.rds"))){
    return(readRDS(file.path(main_dir, "data", "model.rds")))
  }
}

load_processed_qd <- function(main_dir){
  if (length(list.files(file.path(main_dir, "data", "questioned_graphs"), pattern = ".rds")) == 1){
    graphs <- list.files(file.path(main_dir, "data", "questioned_graphs"), pattern = ".rds", full.names = TRUE)[1]
    return(readRDS(graphs))
  }
}

load_qd <- function(qd_path){
  if (!is.null(qd_path) && file.exists(qd_path)){
    return(magick::image_read(qd_path))
  }
}

make_posteriors_df <- function(analysis){
  df <- analysis$posterior_probabilities
  colnames(df) <- c("Known Writer", "Posterior Probability of Writership")
  df <- df %>% dplyr::mutate(`Posterior Probability of Writership` = paste0(100*`Posterior Probability of Writership`, "%"))
  return(df)
}

setup_main_dir <- function(main_dir){
    # create directory structure in main directory
    create_dir(file.path(main_dir, "data"))
    create_dir(file.path(main_dir, "data", "questioned_docs"))
    create_dir(file.path(main_dir, "data", "questioned_graphs"))
    create_dir(file.path(main_dir, "data", "model_docs"))
    
    # handwriter requires template.rds to exist in main_dir > data
    saveRDS(template, file.path(main_dir, "data", "template.rds"))
}