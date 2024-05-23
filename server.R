## Load Libraries
library(shiny)
library(shinyjs)
library(shinyBS)
library(ggplot2)
library(bslib)
library(bsicons)
library(shinycssloaders)
library(randomForest)
library(dplyr)
library(DT)

## Load handwriter
library(handwriter)

## Config
options(shiny.maxRequestSize = 30*1024^2)
addResourcePath("images", "images")


# HELPER FUNCTIONS --------------------------------------------------------
create_dir <- function(folder){
  if (!dir.exists(folder)){
    dir.create(folder)
  }
}

parse_image <- function(x, title){
  card(
    card_header(class = "bg-dark", title),
    max_height = 300,
    full_screen = TRUE,
    imageOutput(x, height=300,width=400),
  )
}

parse_plot <- function(x, title){
  card(
    card_header(class = "bg-dark", title),
    max_height = 300,
    full_screen = TRUE,
    plotOutput(x),
  )
}

plot_qd_profile <- function (clusters, K=40, facet = TRUE) {
  counts <- clusters %>% dplyr::select(writer, docname, cluster)
  # count graphs per cluster
  counts <- counts %>% 
    dplyr::group_by(docname, writer, cluster) %>%
    dplyr::summarize(count = n())
  
  # add missing clusters
  docs <- unique(counts$docname)
  dfs <- list()
  dfs <- lapply(docs, function(doc) {
    doc_df <- counts %>% dplyr::filter(docname == doc)
    missing_clusters <- setdiff(1:K, doc_df$cluster)
    # create data frame for missing clusters
    new_df <- data.frame(docname = rep(doc, length(missing_clusters)),
                         writer = rep(doc_df$writer[1], length(missing_clusters)),
                         cluster = missing_clusters, 
                         count = rep(0, length(missing_clusters)))
    # add to current data frame
    doc_df <- rbind(doc_df, new_df)
    return(doc_df)
  })
  # combine individual data frames into a single data frame
  counts <- do.call(rbind, dfs)
  
  single_doc <- ifelse(length(unique(counts$writer)) == 1, TRUE, FALSE)
  if (single_doc) {
    p <- counts %>% dplyr::mutate(cluster = as.integer(cluster)) %>% 
      ggplot2::ggplot(aes(x = cluster, y = count, color = writer)) + 
      geom_line() + geom_point() + scale_x_continuous(breaks = as.integer(unique(counts$cluster))) + 
      theme_bw()
  }
  else {
    p <- counts %>% dplyr::mutate(cluster = as.integer(cluster)) %>% 
      ggplot2::ggplot(aes(x = cluster, y = count, group = interaction(writer, 
                                                                      doc), color = writer)) + geom_line() + geom_point() + 
      scale_x_continuous(breaks = as.integer(unique(counts$cluster))) + 
      theme_bw()
  }
  if (facet) {
    p <- p + facet_wrap(~writer)
  }
  return(p)
}

plot_known_profiles <- function (clusters, K=40, facet = TRUE) {
  counts <- clusters %>% dplyr::select(writer, docname, cluster)
  # count graphs per cluster
  counts <- counts %>% 
    dplyr::group_by(docname, writer, cluster) %>%
    dplyr::summarize(count = n())
  
  # add missing clusters
  docs <- unique(counts$docname)
  dfs <- list()
  dfs <- lapply(docs, function(doc) {
    doc_df <- counts %>% dplyr::filter(docname == doc)
    missing_clusters <- setdiff(1:K, doc_df$cluster)
    # create data frame for missing clusters
    new_df <- data.frame(docname = rep(doc, length(missing_clusters)),
                         writer = rep(doc_df$writer[1], length(missing_clusters)),
                         cluster = missing_clusters, 
                         count = rep(0, length(missing_clusters)))
    # add to current data frame
    doc_df <- rbind(doc_df, new_df)
    return(doc_df)
  })
  # combine individual data frames into a single data frame
  counts <- do.call(rbind, dfs)
  
  single_doc <- ifelse(length(unique(counts$writer)) == 1, TRUE, FALSE)
  if (single_doc) {
    p <- counts %>% dplyr::mutate(cluster = as.integer(cluster)) %>% 
      ggplot2::ggplot(aes(x = cluster, y = count, color = writer)) + 
      geom_line() + geom_point() + scale_x_continuous(breaks = as.integer(unique(counts$cluster))) + 
      theme_bw()
  }
  else {
    p <- counts %>% dplyr::mutate(cluster = as.integer(cluster)) %>% 
      ggplot2::ggplot(aes(x = cluster, y = count, group = interaction(writer, 
                                                                      doc), color = writer)) + geom_line() + geom_point() + 
      scale_x_continuous(breaks = as.integer(unique(counts$cluster))) + 
      theme_bw()
  }
  if (facet) {
    p <- p + facet_wrap(~writer)
  }
  return(p)
}

## Render RGL Widget UI
# parse_rglui <- function(x)
# {
# 	card(
# 		card_header(class = "bg-dark",paste0("Land ",x)),
# 		max_height = 300,
# 		full_screen = FALSE,
# 		rglwidgetOutput(paste0("x3prgl",x),height=300,width=400),
# 	)
# }
# parse_rgluiprev <- function(x)
# {
# 	card(
# 		card_header(class = "bg-dark",paste0("Land ",x)),
# 		max_height = 300,
# 		full_screen = FALSE,
# 		rglwidgetOutput(paste0("x3prglprev",x),height=300,width=400),
# 	)
# }
# 
# ## Render Land into image with CrossCut line
# render_land <- function(src,x3p,ccut)
# {
# 	imgsrc <- gsub(".x3p$",".png",src)
# 	image_x3p(x3p_sample(x3p_add_vline(x3p,xintercept = ccut, size = 20, color = "#ea2b1f"),m=5),zoom=1)
# 	snapshot3d(imgsrc,webshot=TRUE)
# 	return(imgsrc)
# }


# SERVER ------------------------------------------------------------------

server <- function(input, output, session) {
  
# NEXT BUTTONS ----
  # disable next buttons at start
  shinyjs::disable("setup_next_button")
  shinyjs::disable("known_next_button")
  shinyjs::disable("qd_next_button")
  
  # enable next buttons
  observe({
    # main_dir needs to be defined
    req(global$main_dir)
    shinyjs::enable("setup_next_button")
  })
  observe({
    # model needs to be loaded
    req(global$model)
    shinyjs::enable("known_next_button")
  })
  observe({
    # analysis needs to be loaded
    req(global$analysis)
    shinyjs::enable("qd_next_button")
  })
  
  # change selected tab in main panel
  observeEvent(input$begin_button, {updateTabsetPanel(session, "prevreport", selected = "Setup")})
  observeEvent(input$setup_next_button, {updateTabsetPanel(session, "prevreport", selected = "Known Writing")})
  observeEvent(input$known_next_button, {updateTabsetPanel(session, "prevreport", selected = "Questioned Document")})
  observeEvent(input$qd_next_button, {updateTabsetPanel(session, "prevreport", selected = "Report")})

  
# STORAGE ----
  global <- reactiveValues(
    analysis = NULL,
    known_docs = NULL,
    main_dir = NULL,
    model = NULL,
    qd_image = NULL,
    qd_name = NULL,
    qd_path = NULL,
  )
  

# FOLDERS ----
  shinyDirChoose(
    input,
    'main_dir',
    roots = c(home = '~'),
    filetypes = c('', 'txt', 'bigWig', "tsv", "csv", "bw")
  )
  
  dir <- reactive(input$main_dir)
  
  # display folder path below button
  output$dir <- renderText({
    global$main_dir
  })
  
  # update main directory to the selected directory
  observeEvent(ignoreInit = TRUE,
               eventExpr = {
                 input$main_dir
               },
               handlerExpr = {
                 # update main directory
                 if (!"path" %in% names(dir())) {
                   return()
                 }
                 home <- normalizePath("~")
                 global$main_dir <- file.path(home, paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep))
                 
                 if (length(list.files(global$main_dir)) == 0){
                   # create directory structure in main directory
                   create_dir(file.path(global$main_dir, "data"))
                   create_dir(file.path(global$main_dir, "data", "questioned_docs"))
                   create_dir(file.path(global$main_dir, "data", "questioned_graphs"))
                   create_dir(file.path(global$main_dir, "data", "model_docs"))
                   
                   # copy template to main directory > data
                   file.copy(file.path("data", "template.RDS"), file.path(global$main_dir, "data", "template.RDS"))
                 } else {
                   # load files if they exist
                   if (file.exists(file.path(global$main_dir, "data", "model_docs"))){
                     global$known_docs <- data.frame('files' = list.files(file.path(global$main_dir, "data", "model_docs")))
                   }
                   if (file.exists(file.path(global$main_dir, "data", "model.rds"))){
                     global$model <- readRDS(file.path(global$main_dir, "data", "model.rds"))
                   }
                   if (length(list.files(file.path(global$main_dir, "data", "questioned_docs"))) == 1){
                     global$qd_path <- list.files(file.path(global$main_dir, "data", "questioned_docs"), full.names = TRUE)[1]
                     global$qd_image <- magick::image_read(global$qd_path)
                   }
                   if (length(list.files(file.path(global$main_dir, "data", "questioned_graphs"))) == 1){
                     graphs <- list.files(file.path(global$main_dir, "data", "questioned_graphs"), full.names = TRUE)[1]
                     global$doc <- readRDS(graphs)
                   }
                   if (file.exists(file.path(global$main_dir, "data", "analysis.rds"))){
                     global$analysis <- readRDS(file.path(global$main_dir, "data", "analysis.rds"))
                   }
                 }
  })
  

# KNOWN WRITING ----
  # load known images and save to temp directory > data > model_docs
  observeEvent(input$known_upload, {
    known_paths <- input$known_upload$datapath
    known_names <- input$known_upload$name
    
    # copy known docs to temp directory > data > model_docs
    lapply(1:length(known_paths), function(i) file.copy(known_paths[i], file.path(global$main_dir, "data", "model_docs", known_names[i])))
    
    # list known docs
    global$known_docs <- data.frame('files' = list.files(file.path(global$main_dir, "data", "model_docs")))
    
    # TO-DO: I had to manually delete the problems.txt file from data > model_graphs for fit_model to run
    global$model <- handwriter::fit_model(template_dir = global$main_dir,
                                          model_images_dir = file.path(global$main_dir, "data", "model_docs"),
                                          num_iters = 4000,
                                          num_chains = 1,
                                          num_cores = 1,
                                          writer_indices = c(2, 5),
                                          doc_indices = c(7, 13)
    )
  })
  
  output$known_docs <- renderTable({global$known_docs})
  
  output$known_profiles <- renderPlot({handwriter::plot_credible_intervals(model = global$model,
                                                                           facet = TRUE)})
  
  # UI to display known handwriting samples and plots
  output$known_display <- renderUI({
    if(is.null(global$model)) {return(NULL)}
    
    # display
    bsCollapse(id = "known_display",
               bsCollapsePanel("Known writing samples", tableOutput("known_docs")),
               bsCollapsePanel("Writer profiles", plotOutput("known_profiles"))
    )
  })
  

# QUESTIONED DOCUMENT ----

  # load QD image for display and copy to temp dir > data > questioned_docs
  observeEvent(input$qd_upload, {
    global$qd_path <- input$qd_upload$datapath
    global$qd_name <- input$qd_upload$name
    
    global$qd_image <- NULL
    global$qd_image <- magick::image_read(global$qd_path)
    
    # copy qd to temp directory
    file.copy(global$qd_path, file.path(global$main_dir, "data", "questioned_docs", global$qd_name))
    
    # process and save to global$main_dir > data > questioned_graphs
    global$doc <- handwriter::processDocument(file.path(global$main_dir, "data", "questioned_docs", global$qd_name))
    saveRDS(global$doc, file.path(global$main_dir, "data", "questioned_graphs", stringr::str_replace(global$qd_name, ".png", "_proclist.rds")))
    
    # analyze
    global$analysis <- analyze_questioned_documents(template_dir = global$main_dir,
                                                    questioned_images_dir = file.path(global$main_dir, "data", "questioned_docs"),
                                                    model = global$model,
                                                    num_cores = 1,
                                                    writer_indices = c(2, 5),
                                                    doc_indices = c(7, 13))
  })
  
  output$qd_image <- renderImage({
    tmp <- global$qd_image %>%
      image_write(tempfile(fileext='png'), format = 'png')
    
    # return a list
    list(src = tmp, contentType = "image/png")
  }, deleteFile = FALSE)
  
  output$qd_nodes <- renderPlot({
    handwriter::plotNodes(global$doc, nodeSize = 2)
  })
  
  output$qd_profile <- renderPlot({
    plot_cluster_fill_counts(global$analysis, facet=FALSE)
  })
  
  output$qd_analysis <- renderTable({
    df <- global$analysis$posterior_probabilities
    colnames(df) <- c("Known Writer", "Posterior Probability of Writership")
    df <- df %>% dplyr::mutate(`Posterior Probability of Writership` = paste0(100*`Posterior Probability of Writership`, 
                                                                              "%"))
  })
  
  # UI to display QD and plots
  output$qd_display <- renderUI({
    if(is.null(global$analysis)) {return(NULL)}
    
    # display QD image, graphs plot, and clusters plot
    bsCollapse(id = "qd_display",
               bsCollapsePanel("Preview", 
                               card(
                                 card_header(class = "bg-dark", ""),
                                 # max_height = 300,
                                 full_screen = TRUE,
                                 imageOutput("qd_image"))
                               ),
               bsCollapsePanel("Processed", 
                               p(class = "text-muted", "The handwriting in the questioned document is split into 
                                        component shapes called graphs."),
                               plotOutput("qd_nodes")),
               bsCollapsePanel("Writer profile", plotOutput("qd_profile")),
               bsCollapsePanel("Writership analysis", 
                               p(class = "text-muted", "The posterior probability that each writer in the closed-set of writers wrote the questioned document."),
                               tableOutput("qd_analysis"))
    )
  })
  
# REPORT ----

  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = function() {
      paste('report', sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'))
    },
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(
        analysis = global$analysis,
        known_docs = global$known_docs,
        model = global$model,
        qd_path = global$qd_path,
        qd_doc = global$doc
      )
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      # rmarkdown::render(tempReport, output_file = file,
      #                   params = params,
      #                   envir = new.env(parent = globalenv())
      # )
      # 
      library(rmarkdown)
      out <- render('report.Rmd',
                    switch(
                      input$format,
                      PDF = bookdown::pdf_document2(), HTML = bookdown::html_document2(), Word = word_document()
                    ),
                    params = params,
                    envir = new.env(parent = globalenv()))
      file.rename(out, file)
    }
  )

  # output$report_display <- renderUI({
  #   if(is.null(global$analysis)) {return(NULL)}
  #   
  #   bsCollapse(id = "collapseReport", open = "Panel 1",
  #              bsCollapsePanel("QD Preview", 
  #                              plotOutput("report_qd_image")
  #                              ),
  #              bsCollapsePanel("Panel 2", "This panel has a generic plot. ",
  #                              "and a 'success' style.")
  #   )
  # })

}