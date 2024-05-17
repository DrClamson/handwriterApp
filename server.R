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

plot_clusters <- function (clusters, K=40, facet = TRUE) {
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

  observeEvent(input$confirm_autonomous, {updateTabsetPanel(session, "prevreport", selected = "Folders")})

# STORAGE ----
  global <- reactiveValues(main_dir = getwd())
  

# FOLDERS -----------------------------------------------------------------
  shinyDirChoose(
    input,
    'main_dir',
    roots = c(home = '~'),
    filetypes = c('', 'txt', 'bigWig', "tsv", "csv", "bw")
  )
  
  dir <- reactive(input$main_dir)
  
  output$dir <- renderText({
    global$main_dir
  })
  
  # update main directory to the selected directory
  observeEvent(ignoreNULL = TRUE,
               eventExpr = {
                 input$main_dir
               },
               handlerExpr = {
                 if (!"path" %in% names(dir())) {
                   return()
                 }
                 home <- normalizePath("~")
                 # update main directory
                 global$main_dir <- file.path(home, paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep))
  })
  
  observeEvent(input$main_dir, {
    # create directory structure copy template there
    create_dir(file.path(global$main_dir, "data"))
    create_dir(file.path(global$main_dir, "data", "questioned_docs"))
    create_dir(file.path(global$main_dir, "data", "questioned_graphs"))
    create_dir(file.path(global$main_dir, "data", "model_docs"))
    
    # copy template to directory
    file.copy(file.path("data", "template.RDS"), file.path(global$main_dir, "data", "template.RDS"))
  })
  
  ## Push current bullet data to all bullet data object
  # observeEvent(input$up_bull,{
  # 							# if(nrow(bulldata$cbull)==0) return(NULL)
  # 							allbull <- bulldata$allbull
  # 							allbull <- allbull[!(allbull$bullet %in% input$bul_x3p_name),]
  # 							bull <- bulldata$cbull
  # 							bull$bullet <- input$bul_x3p_name
  # 							bull$land <- 1:nrow(bull)
  # 							bulldata$allbull <- rbind(allbull,bull)
  # 							disable("up_bull")
  # 			})
  

# QUESTIONED DOCUMENT ----

  # UI to upload questioned document
  output$qd_ui <- renderUI({fileInput("qd_upload", "Select the questioned document", accept = ".png", multiple=FALSE)})
  
  # load QD image for display and copy to temp dir > data > questioned_docs
  observeEvent(input$qd_upload, {
    global$qd_path <- input$qd_upload$datapath
    global$qd_name <- input$qd_upload$name
    
    global$qd_image <- NULL
    global$qd_image <- magick::image_read(global$qd_path)
    # info <- image_info(global$qd_image)
    
    # copy qd to temp directory
    file.copy(global$qd_path, file.path(global$main_dir, "data", "questioned_docs", global$qd_name))
    
    # testing only
    global$test_qd_docs <- list.files(file.path(global$main_dir, "data", "questioned_docs"))
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
  
  output$qd_profiles <- renderPlot({
    plot_clusters(global$qd_profiles, K=40)
  })
  
  # UI to display QD and plots
  output$qd_display <- renderUI({
    if(is.null(input$qd_upload)) {return(NULL)}
    
    progress <- shiny::Progress$new(); on.exit(progress$close())
    
    # refresh on Tab Change
    temp_refresh <- input$prevreport
    
    # process and save document to global$main_dir > data > questioned_graphs
    progress$set(message = "Processing document", value = .25)
    global$doc <- handwriter::processDocument(file.path(global$main_dir, "data", "questioned_docs", global$qd_name))
    saveRDS(global$doc, file.path(global$main_dir, "data", "questioned_graphs", stringr::str_replace(global$qd_name, ".png", "_proclist.rds")))
    
    # load template
    progress$set(message = "Loading cluster template", value = .5)
    global$template <- readRDS(file.path(global$main_dir, "data", "template.RDS"))
    
    # get cluster fill counts
    progress$set(message = "Getting cluster fill counts", value = .5)
    # TO-DO: allow user to set writer and doc indices
    handwriter::get_clusters_batch(template = global$template,
                                   input_dir = file.path(global$main_dir, "data", "questioned_graphs"),
                                   output_dir = file.path(global$main_dir, "data", "questioned_clusters"),
                                   writer_indices = c(2, 5),
                                   doc_indices = c(7, 13))
    global$qd_profiles <- readRDS(file.path(global$main_dir, "data", "questioned_clusters", stringr::str_replace(global$qd_name, ".png", ".rds")))
    
    # display QD image, graphs plot, and clusters plot
    navset_card_tab(
      height = 450,
      nav_panel(
        "Preview",
        full_screen = TRUE,
        p(class = "text-muted", "Here is a preview of the selected questioned document."),
        imageOutput("qd_image")
      ),
      nav_panel(
        "Graphs",
        full_screen = TRUE,
        p(class = "text-muted", "The handwriting is split into component shapes called graphs."),
        plotOutput("qd_nodes")
      ),
      nav_panel(
        "Writer Profile",
        full_screen = TRUE,
        p(class = "text-muted", "A writer profile for the questioned document is estimated by grouping the graphs into clusters of 
          similar shapes and counting the number of graphs in each cluster. The idea is that different writers generally produce different
          shapes at differing frequencies."),
        plotOutput("qd_profiles")
      )
    )
  })
  
  
# KNOWN WRITING ----
  ## UI to upload known writing samples
  output$known_ui <- renderUI({fileInput("known_upload", "Select the known writing samples", accept = ".png", multiple=TRUE)})
  
  # load known images and save to temp directory > data > model_docs
  observeEvent(input$known_upload, {
    global$known_paths <- input$known_upload$datapath
    global$known_names <- input$known_upload$name

    # copy known docs to temp directory > data > model_docs
    lapply(1:length(global$known_paths), function(i) file.copy(global$known_paths[i], file.path(global$main_dir, "data", "model_docs", global$known_names[i])))
    
    # list known docs
    global$known_docs <- data.frame('files' = list.files(file.path(global$main_dir, "data", "model_docs")))
  })
  
  output$known_docs <- renderTable({global$known_docs})
  
  output$known_profiles <- renderPlot({handwriter::plot_cluster_fill_counts(formatted_data = model,
                                                                            facet = TRUE)})
  
  # UI to display known handwriting samples and plots
  output$known_display <- renderUI({
    if(is.null(input$known_upload)) {return(NULL)}
    
    progress <- shiny::Progress$new(); on.exit(progress$close())
    
    # # refresh on Tab Change
    # temp_refresh <- input$prevreport
    
    # TO-DO: Allow users to change writer indices and doc indices 
    progress$set(message = "Fitting statistical model", value = .5)
    global$model <- handwriter::fit_model(template_dir = global$main_dir,
                                          model_images_dir = file.path(global$main_dir, "data", "model_docs"),
                                          num_iters = 10,
                                          num_chains = 1,
                                          num_cores = 1,
                                          writer_indices = c(2, 5),
                                          doc_indices = c(7, 18),
    )
    
    # display
    navset_card_tab(
      height = 450,
      nav_panel(
        "Samples",
        full_screen = TRUE,
        p(class = "text-muted", "These are the selected known writing samples."),
        tableOutput("known_docs")
      ),
      nav_panel(
        "Profiles",
        full_screen = TRUE,
        plotOutput("known_profiles")
      )
    )
  })
  
  #################################################################################
  ## Preview Bullet Selection
  #################################################################################
  # 	output$prevSelUI <- renderUI({
  #   									if(nrow(bulldata$allbull)==0) return(NULL)
  #   									allbull <- bulldata$allbull
  #   									selectInput("prev_bul","Preview Bullet",choices=unique(allbull$bullet),selected=NULL,multiple = FALSE)
  #   						})
  # 	output$lpreview <- renderUI({
  # 									if(nrow(bulldata$allbull)==0) return(NULL)
  # 									if(length(input$prev_bul)==0) return(NULL)
  # 									progress <- shiny::Progress$new();on.exit(progress$close())
  # 
  # 									## Refresh on Tab Change
  # 									temp_refresh <- input$prevreport
  # 
  # 									## Render Bullet
  # 									allbull <- bulldata$allbull
  # 									bull <- allbull[allbull$bullet==input$prev_bul,]
  # 
  # 									progress$set(message = "Rendering Previews", value = .75)
  # 									for(idx in 1:nrow(bull)) 
  # 									{
  # 										local({
  # 												cidx <- idx
  # 												output[[paste0("x3prglprev",idx)]] <- renderRglwidget({
  # 																									image_x3p(x3p_sample(bull$x3pv[[cidx]],m=5),zoom=1)
  # 																									rglwidget()
  # 																					})
  # 											})
  # 									}
  # 
  # 									## UI
  # 									layout_column_wrap(
  # 										width = 1/3,
  # 										!!!lapply(1:nrow(bull),parse_rgluiprev)
  # 									)
  # 						})
  #################################################################################
  #################################################################################
  
  
  #################################################################################
  ## Compare Bullet Selection and processing
  #################################################################################
  #    	output$bull_sel <- renderUI({
  #   									if(nrow(bulldata$allbull)==0) return(NULL)
  #   									allbull <- bulldata$allbull
  #   									checkboxGroupInput(
  #   										"bullcompgroup",
  #   										label = "Selects Bullets to Compare", 
  #     									choices = unique(bulldata$allbull$bullet),
  #     									selected = unique(bulldata$allbull$bullet)
  #     								)
  #   						})
  # 
  #   	observeEvent(input$doprocess,{
  # 								if(length(input$bullcompgroup)==0) return(NULL)
  # 								progress <- shiny::Progress$new();on.exit(progress$close())
  # 
  # 								## Fetch All Bullets
  # 								bullets <- bulldata$allbull
  # 								resolution <- x3p_get_scale(bullets$x3p[[1]])
  # 
  # 								## Get the ideal Cross Sections
  # 								progress$set(message = "Get the ideal Cross Sections", value = 0)
  # 								bullets$crosscut <- sapply(bullets$x3p,x3p_crosscut_optimize)
  # 								bullets$ccdata <- mapply(x3p_crosscut,bullets$x3p,bullets$crosscut,SIMPLIFY=FALSE)
  # 
  # 								## Get the Groove Locations
  # 								progress$set(message = "Get the Groove Locations", value = .05)
  # 								bullets$grooves <- lapply(bullets$ccdata,function(x) cc_locate_grooves(x,method = "middle", adjust = 30, return_plot = FALSE))
  # 
  # 								## Extracting Signal
  # 								progress$set(message = "Extracting Signal", value = .1)
  # 								bullets$sigs <- mapply(function(ccdata,grooves) cc_get_signature(ccdata, grooves, span1 = 0.75, span2 = 0.03) ,bullets$ccdata,bullets$grooves,SIMPLIFY=FALSE)
  # 								bullets$bulletland <- paste0(bullets$bullet,"-", bullets$land)
  # 								lands <- unique(bullets$bulletland)
  # 
  # 								## Align Signal
  # 								progress$set(message = "Align Signal", value = .15)
  # 								comparisons <- data.frame(expand.grid(land1 = lands, land2 = lands), stringsAsFactors = FALSE)
  # 								comparisons$aligned <- mapply(function(x,y,bullets) sig_align(bullets$sigs[bullets$bulletland == x][[1]]$sig, bullets$sigs[bullets$bulletland == y][[1]]$sig),comparisons$land1,comparisons$land2,MoreArgs=list(bullets=bullets),SIMPLIFY=FALSE)
  # 
  # 								## Evaluating Features
  # 								progress$set(message = "Evaluating Features", value = .2)
  # 								comparisons$ccf0 <- sapply(comparisons$aligned,function(x) extract_feature_ccf(x$lands))
  # 								comparisons$lag0 <- sapply(comparisons$aligned,function(x) extract_feature_lag(x$lands))
  # 								comparisons$D0 <- sapply(comparisons$aligned,function(x) extract_feature_D(x$lands))
  # 								comparisons$length0 <- as.numeric(sapply(comparisons$aligned,function(x) extract_feature_length(x$lands)))
  # 								comparisons$overlap0 <- sapply(comparisons$aligned,function(x) extract_feature_overlap(x$lands))
  # 
  # 								## Evaluating Striation Marks
  # 								progress$set(message = "Evaluating Striation Marks", value = .25)
  # 								comparisons$striae <- lapply(comparisons$aligned,sig_cms_max,span=75)
  # 								
  # 								## Evaluating Features
  # 								progress$set(message = "Evaluating Features", value = .3)
  # 								comparisons$cms_per_mm <- mapply(function(x,y,resolution) extract_feature_cms_per_mm(x$lines,y$lands,resolution),comparisons$striae,comparisons$striae,MoreArgs=list(resolution=resolution),SIMPLIFY=FALSE)
  # 								comparisons$matches0 <- as.numeric(sapply(comparisons$striae,function(s) bulletxtrctr:::extract_helper_feature_n_striae(s$lines, type = "peak", match = TRUE)))
  # 								comparisons$mismatches0 <- as.numeric(sapply(comparisons$striae,function(s) bulletxtrctr:::extract_helper_feature_n_striae(s$lines, type = "peak", match = FALSE)))
  # 								
  # 								## Extracting Features
  # 								progress$set(message = "Extracting Features", value = .35)
  # 								comparisons$bulletA <- sapply(strsplit(as.character(comparisons$land1),"-"),"[[",1)
  # 								comparisons$bulletB <- sapply(strsplit(as.character(comparisons$land2),"-"),"[[",1)
  # 								comparisons$landA <- sapply(strsplit(as.character(comparisons$land1),"-"),"[[",2)
  # 								comparisons$landB <- sapply(strsplit(as.character(comparisons$land2),"-"),"[[",2)
  # 								comparisons$features <- mapply(extract_features_all,comparisons$aligned,comparisons$striae,MoreArgs=list(resolution=resolution),SIMPLIFY=FALSE)
  # 								comparisons$legacy_features <- mapply(extract_features_all_legacy,comparisons$striae,MoreArgs=list(resolution=resolution),SIMPLIFY=FALSE)
  # 
  # 								## Scaling Features
  # 								progress$set(message = "Scaling Features", value = .4)
  # 								features <- tidyr::unnest(comparisons[,c("land1", "land2", "ccf0", "bulletA", "bulletB", "landA", "landB", "features")])
  # 								features <- features %>% mutate(cms = cms_per_mm,matches = matches_per_mm,mismatches = mismatches_per_mm,non_cms = non_cms_per_mm)
  # 
  # 								## Predicting RandomForest Scores
  # 								progress$set(message = "Predicting RandomForest Scores", value = .45)
  # 								features$rfscore <- predict(rtrees, newdata = features, type = "prob")[,2]
  # 
  # 								## Preparing Data for Report
  # 								progress$set(message = "Preparing Report Data", value = .5)
  # 								bullet_scores <- features %>% group_by(bulletA, bulletB) %>% tidyr::nest()
  # 								bullet_scores$bullet_score <- sapply(bullet_scores$data,function(d) max(compute_average_scores(land1 = d$landA, land2 = d$landB, d$rfscore)))
  # 								bullet_scores$data <- lapply(bullet_scores$data,function(d) cbind(d,samesource=bullet_to_land_predict(land1 = d$landA, land2 = d$landB, d$rfscore,difference=0.1)))
  # 								
  # 								
  # 								# Rendering Bullet Images for Report
  # 								bullets$x3pimg <- NA
  # 								for(idx in 1:nrow(bullets))
  # 								{
  # 									progress$set(message = "Rendering Report Objects", value = round(seq(from=.55,to=.85,length.out=nrow(bullets)),2)[idx])
  # 									bullets$x3pimg[idx] <- render_land(bullets$source[idx],bullets$x3pv[[idx]],bullets$crosscut[idx])	
  # 								}
  # 
  # 								## Saving Report Data
  # 								progress$set(message = "Preparing Report", value = .9)
  # 								bulldata$comparison <- list(bullets=bullets,comparisons=comparisons,features_scaled=features,bullet_scores=bullet_scores)
  # 
  # 								## Update the selected Panel
  # 								updateTabsetPanel(session, "prevreport", selected = "Comparison Report")
  # 
  # 								# Debug
  # 								# saveRDS(list(comparison = bulldata$comparison),"~/Downloads/aa.RDS")
  # 				})
  #   	#################################################################################
  #   	#################################################################################
  # 
  # 
  #   	#################################################################################
  # 	## Generate Bullet Comparison Report UI
  #   	#################################################################################
  #   	## Side Panel UI
  #   	output$reportSelUI <- renderUI({
  #   										if(is.null(bulldata$comparison)) return(NULL)
  #   										all_bullets <- unique(bulldata$comparison$bullet_scores$bulletA)
  #   										list(
  #   											selectInput("comp_bul1","Compare Bullet",choices=all_bullets,selected=all_bullets[1]),
  #   											selectInput("comp_bul2","With Bullet",choices=all_bullets,selected=all_bullets[2]),
  #   											hr()
  #   										)
  #   							})
  # 
  #   	## Side Panel UI Download Report
  #   	output$reportDownUI <- renderUI({
  #   										if(is.null(bulldata$comparison)) return(NULL)
  #   										fluidRow(column(12,screenshotButton(label = "Download Report", id = "reportUI",filename="Bullet Comparison Report",scale=2),align="center"))
  #   							})
  # 
  #   	## Main Panel UI Bullet Comparison Report
  #   	output$reportUI <- renderUI({
  #   									if(is.null(bulldata$comparison)) return(NULL)
  #   									if(is.null(input$comp_bul1) | is.null(input$comp_bul2)) return(NULL)
  # 
  #   									## Bullet Comparison Report
  #   									BullComp <- list(
  # 				  										fluidRow(
  # 											          				column(6,plotOutput("bull_comp")),
  # 											          				column(6,plotOutput("land_comp"))
  # 											        	),
  # 											        	br(),br(),
  # 											        	fluidRow(column(12,plotOutput("land_visCC"),align="center")),
  # 											        	br(),br(),
  # 											        	fluidRow(column(12,plotOutput("land_visSig"),align="center")),
  # 											        	br()
  # 								       			)
  # 
  #   									## Land Comparison Collapsable Report
  #   									LandComp <- list()
  #   									bullet_scores <- bulldata$comparison$bullet_scores
  #   									bullet_scores <- bullet_scores[bullet_scores$bulletA==input$comp_bul1 & bullet_scores$bulletB==input$comp_bul2,]
  #   									bullet_scores$data[[1]] <- bullet_scores$data[[1]][bullet_scores$data[[1]]$samesource,]
  #   									if(nrow(bullet_scores$data[[1]])>0)
  #   									{
  #   										## Collect Land wise Data
  #   										bsldata <- bullet_scores$data[[1]]
  #   										odridx <- order(bsldata$rfscore,decreasing=TRUE)
  # 
  #   										## Generate Collapsible UI Panel List in a loop
  #   										bsCollapsePanelList <- list()
  #   										for(idx in 1:length(odridx))
  #   										{
  #   											#########################################################################################################
  #   											## Data Table Comparison
  #   											#########################################################################################################
  #   											BullCompBulls <- bulldata$comparison$bullets
  #   											temptable <- data.frame(
  #   																		Feature = c("Left Land File","Left Land MD5","Right Land File","Right Land MD5","Cross Correlation Function","Mean Distance bw Matching Striae","Signature Length in Millimeters","Matches Per Millimeter","Mismatches Per Millimeter","CMS Per Millimeter","Non-CMS Per Millimeter","Peak Sum Per Millimeter"),
  #   																		Value = c(
  #   																					BullCompBulls$filename[BullCompBulls$bullet==input$comp_bul1 & BullCompBulls$land == bsldata$landA[odridx[idx]]],
  #   																					BullCompBulls$md5sum[BullCompBulls$bullet==input$comp_bul1 & BullCompBulls$land == bsldata$landA[odridx[idx]]],
  #   																					BullCompBulls$filename[BullCompBulls$bullet==input$comp_bul1 & BullCompBulls$land == bsldata$landA[odridx[idx]]],
  #   																					BullCompBulls$md5sum[BullCompBulls$bullet==input$comp_bul2 & BullCompBulls$land == bsldata$landB[odridx[idx]]],
  #   																					round(bsldata$ccf[odridx[idx]],3),
  #   																					round(bsldata$D[odridx[idx]],3),
  #   																					round(bsldata$length_mm[odridx[idx]],3),
  #   																					round(bsldata$matches_per_mm[odridx[idx]],3),
  #   																					round(bsldata$mismatches_per_mm[odridx[idx]],3),
  #   																					round(bsldata$cms_per_mm[odridx[idx]],3),
  #   																					round(bsldata$non_cms_per_mm[odridx[idx]],3),
  #   																					round(bsldata$sum_peaks[odridx[idx]],3)
  #   																				)
  #   															)
  #   											temptable_dt <- datatable(temptable,rownames=FALSE,options = list(paging = FALSE,ordering=FALSE,searching=FALSE,bLengthChange = FALSE,bInfo = FALSE))
  #   											#########################################################################################################
  #   											#########################################################################################################
  # 
  #   											#########################################################################################################
  #   											## RGL Render Comparison
  #   											#########################################################################################################
  #   											local({
  #   												cidx <- idx
  # 	  											BullCompBulls <- bulldata$comparison$bullets
  # 	  											rglLidx <- which(BullCompBulls$bullet==input$comp_bul1 & BullCompBulls$land == bsldata$landA[odridx[cidx]])
  # 	  											rglRidx <- which(BullCompBulls$bullet==input$comp_bul2 & BullCompBulls$land == bsldata$landB[odridx[cidx]])
  # 	  											rglL <- BullCompBulls$x3pimg[[rglLidx]]
  # 	  											rglR <- BullCompBulls$x3pimg[[rglRidx]]
  # 	  											output[[paste0("rglWinL",idx)]] = renderImage({list(src = rglL, contentType = 'image/png')}, deleteFile = FALSE)
  # 	  											output[[paste0("rglWinR",idx)]] = renderImage({list(src = rglR, contentType = 'image/png')}, deleteFile = FALSE)
  #   											})
  #   											temp_rgl <- fluidRow(
  #   																		column(1,),
  #   																		column(5,imageOutput(paste0("rglWinL",idx)),align="left"),
  #   																		column(5,imageOutput(paste0("rglWinR",idx)),align="left"),
  #   																		column(1,)
  #   															)
  #   											#########################################################################################################
  #   											#########################################################################################################
  # 
  # 
  #   											#########################################################################################################
  #   											## Groove Plot
  #   											#########################################################################################################
  #   											# local({
  #   											# 	cidx <- idx
  # 	  										# 	BullCompBulls <- bulldata$comparison$bullets
  # 	  										# 	GroovePlotLidx <- which(BullCompBulls$bullet==input$comp_bul1 & BullCompBulls$land == bsldata$landA[odridx[idx]])
  # 	  										# 	GroovePlotRidx <- which(BullCompBulls$bullet==input$comp_bul2 & BullCompBulls$land == bsldata$landB[odridx[idx]])
  # 	  										# 	output[[paste0("GroovePlotL",idx)]] = renderPlot({
  # 	  										# 														BullCompBulls$grooves[[GroovePlotLidx]]$plot +
  # 	  										# 														xlab("Position along width of Land in Microns (1 Millimeter = 1000 Microns)") +
  # 											#   														ylab("Surface Height in Microns") + 
  # 	  										# 														ggtitle(paste0("Location of the grooves in Land : ",bsldata$land1[odridx[cidx]])) 
  # 	  										# 													})
  # 	  										# 	output[[paste0("GroovePlotR",idx)]] = renderPlot({
  # 	  										# 														BullCompBulls$grooves[[GroovePlotRidx]]$plot +
  # 	  										# 														xlab("Position along width of Land in Microns (1 Millimeter = 1000 Microns)") +
  # 	  										# 														ylab("Surface Height in Microns") + 
  # 	  										# 														ggtitle(paste0("Location of the grooves in Land : ",bsldata$land2[odridx[cidx]]))
  # 	  										# 													})
  #   											# })
  #   											local({
  #   												cidx <- idx
  # 	  											BullCompBulls <- bulldata$comparison$bullets
  # 	  											GroovePlotLidx <- which(BullCompBulls$bullet==input$comp_bul1 & BullCompBulls$land == bsldata$landA[odridx[idx]])
  # 	  											GroovePlotRidx <- which(BullCompBulls$bullet==input$comp_bul2 & BullCompBulls$land == bsldata$landB[odridx[idx]])
  # 	  											GroovesL <- as.numeric(BullCompBulls$grooves[[GroovePlotLidx]]$groove)
  # 	  											GroovesR <- as.numeric(BullCompBulls$grooves[[GroovePlotRidx]]$groove)
  # 	  											CCDataL <- BullCompBulls$ccdata[[GroovePlotLidx]] - GroovesL[1]
  # 	  											CCDataR <- BullCompBulls$ccdata[[GroovePlotRidx]] - GroovesR[1]
  # 	  											output[[paste0("GroovePlotL",idx)]] = renderPlot({
  # 	  																								CCDataL %>% 
  # 											  														ggplot(aes(x = x, y = value)) + 
  # 																									geom_line() +
  # 																									theme_bw()+
  # 																									geom_vline(xintercept = 0, colour = "blue") + 
  # 																									geom_vline(xintercept = GroovesL[2]-GroovesL[1], colour = "blue") +
  # 																									scale_x_continuous(breaks=c(0,round(as.numeric(GroovesL[2]-GroovesL[1]),0),round(seq(min(CCDataL$x),max(CCDataL$x),by=500),-2))) +
  # 	  																								xlab("Position along width of Land in Microns\n(1 Millimeter = 1000 Microns)") +
  # 											  														ylab("Surface Height in Microns") + 
  # 	  																								ggtitle(paste0("Location of the grooves in Land : ",bsldata$land1[odridx[cidx]]))+
  # 	  																								theme(
  # 																								  		axis.text=element_text(size=16),
  # 																								  		axis.title=element_text(size=18),
  # 																								  		plot.title = element_text(size=22,face="bold"),
  # 																								  		axis.text.x = element_text(angle = 90, hjust = 1)
  # 																						  			) 
  # 	  																							})
  # 	  											output[[paste0("GroovePlotR",idx)]] = renderPlot({
  # 	  																								CCDataR %>% 
  # 											  														ggplot(aes(x = x, y = value)) + 
  # 																									geom_line() +
  # 																									theme_bw()+
  # 																									geom_vline(xintercept = 0, colour = "blue") + 
  # 																									geom_vline(xintercept = GroovesR[2]-GroovesR[1], colour = "blue") +
  # 																									scale_x_continuous(breaks=c(0,round(as.numeric(GroovesR[2]-GroovesR[1]),0),round(seq(min(CCDataR$x),max(CCDataR$x),by=500),-2))) +
  # 	  																								xlab("Position along width of Land in Microns\n(1 Millimeter = 1000 Microns)") +
  # 	  																								ylab("Surface Height in Microns") + 
  # 	  																								ggtitle(paste0("Location of the grooves in Land : ",bsldata$land2[odridx[cidx]]))+
  # 	  																								theme(
  # 																								  		axis.text=element_text(size=16),
  # 																								  		axis.title=element_text(size=18),
  # 																								  		plot.title = element_text(size=22,face="bold"),
  # 																								  		axis.text.x = element_text(angle = 90, hjust = 1)
  # 																						  			) 
  # 	  																							})
  #   											})
  #   											temp_groove <- fluidRow(
  #   																		column(6,plotOutput(paste0("GroovePlotL",idx)),align="center"),
  #   																		column(6,plotOutput(paste0("GroovePlotR",idx)),align="center")
  #   															)
  #   											#########################################################################################################
  #   											#########################################################################################################
  # 
  # 
  # 											#########################################################################################################
  #   											## Signal Comparison
  #   											#########################################################################################################
  #   											local({
  #   												cidx <- idx
  # 	  											BullCompComps <- bulldata$comparison$comparisons
  # 
  # 	  											SigPlotData <- BullCompComps$aligned[
  # 	  																					(BullCompComps$bulletA == input$comp_bul1)&
  # 	  																					(BullCompComps$bulletB == input$comp_bul2)&
  # 	  																					(BullCompComps$landA == bsldata$landA[odridx[idx]])&
  # 	  																					(BullCompComps$landB == bsldata$landB[odridx[idx]])
  # 	  																				][[1]]$lands
  # 	  											SigPlotData <- tidyr::gather(SigPlotData,Signal, value, sig1, sig2)
  # 	  											SigPlotData$Signal[SigPlotData$Signal=="sig1"] <- "Left Land"
  # 	  											SigPlotData$Signal[SigPlotData$Signal=="sig2"] <- "Right Land"
  # 												output[[paste0("SigPlot",idx)]] = renderPlot({
  # 																								ggplot(SigPlotData,aes(x = x, y = value, colour = Signal)) + 
  # 																							    geom_line(na.rm=TRUE) +
  # 																							  	theme_bw() +
  # 																							  	scale_color_brewer(palette = "Dark2") +
  # 																							  	xlab("Position along width of Land in Microns (1 Millimeter = 1000 Microns)") +
  # 																							  	ylab("Signal in Microns") +
  # 																							  	ggtitle("Alignment of two Bullet Lands")+
  # 																							  	theme(
  # 																								  		axis.text=element_text(size=16),
  # 																								  		axis.title=element_text(size=18),
  # 																								  		legend.title=element_text(size=18),
  # 																								  		legend.text=element_text(size=16),
  # 																								  		plot.title = element_text(size=22,face="bold"),
  # 																								  		axis.text.x = element_text(angle = 90, hjust = 1)
  # 																						  			) 
  # 																					})
  #   											})
  #   											temp_signal <- fluidRow(column(12,plotOutput(paste0("SigPlot",idx)),align="center"))
  #   											#########################################################################################################
  #   											#########################################################################################################
  # 
  #   											## Combine Results
  #   											panel_name <- paste0(bsldata$land1[odridx[idx]]," vs ",bsldata$land2[odridx[idx]]," (RF Score = ",round(bsldata$rfscore[odridx[idx]],4),")")
  #   											# bsCollapsePanelList[[idx]] <- bsCollapsePanel(panel_name, temptable_dt, br(), temp_groove, br(), temp_signal, style = "primary")
  #   											bsCollapsePanelList[[idx]] <- bsCollapsePanel(panel_name, temptable_dt, br(),temp_rgl, temp_groove, br(), temp_signal, style = "primary")
  #   										}
  # 
  #   										## Generate Collapsible UI Panel
  #   										LandComp <- do.call(bsCollapse,args=c(id = "collapseExample",multiple=TRUE,bsCollapsePanelList))
  #   									}
  # 
  #   									## If no Land Match
  #   									if(nrow(bullet_scores$data[[1]])==0) LandComp$children <- list(fluidRow(column(12,h3("No Land Matches in this Bullet Pair."),align="center")),br())
  # 
  #   									## Return Full Collapsible Report
  #   									return(c(BullComp,LandComp$children))
  #   						})
  #   	#################################################################################
  #   	#################################################################################
  # 
  # 
  #   	#################################################################################
  # 	## Generate Bullet Comparison Report Server Outputs
  #   	#################################################################################
  #   	## Bullet Comparison Heatmap
  #   	output$bull_comp <- renderPlot({
  #   									if(is.null(bulldata$comparison)) return(NULL)
  # 
  #   									bullet_scores <- bulldata$comparison$bullet_scores
  #   									bullet_scores$selsource <- FALSE
  #   									bullet_scores$selsource[bullet_scores$bulletA==input$comp_bul1 & bullet_scores$bulletB==input$comp_bul2] <- TRUE
  #   									bullet_scores$selsource[bullet_scores$bulletB==input$comp_bul1 & bullet_scores$bulletA==input$comp_bul2] <- TRUE
  #   									bullet_scores %>% 
  # 									  ggplot(aes(x = bulletA, y = bulletB, fill = bullet_score, colour=selsource)) +
  # 									  geom_tile() +
  # 									  labs(fill="Bullet Score") +
  # 									  scale_fill_gradient2(low = "grey80", high = "darkorange", midpoint = .5) +
  # 									  scale_colour_manual(global = c("black", "black")) +
  # 									  geom_tile(size = 1, data = bullet_scores %>% filter(selsource)) +
  # 									  geom_text(aes(label = round(bullet_score, 2)),size=6) +
  # 									  xlab("Bullet Name") +
  # 									  ylab("Bullet Name") +
  # 									  guides(colour="none") +
  # 									  coord_equal() +
  # 									  theme(
  # 									  		axis.text=element_text(size=16),
  # 									  		axis.title=element_text(size=18),
  # 									  		legend.title=element_text(size=18),
  # 									  		legend.text=element_text(size=16)
  # 									  	)
  #   						})
  # 
  #   	## Land Comparison Heatmap
  #   	output$land_comp <- renderPlot({
  #   									if(is.null(bulldata$comparison)) return(NULL)
  #   									if(is.null(input$comp_bul1) | is.null(input$comp_bul2)) return(NULL)
  # 
  #   									bullet_scores <- bulldata$comparison$bullet_scores
  #   									bullet_scores <- bullet_scores[bullet_scores$bulletA==input$comp_bul1 & bullet_scores$bulletB==input$comp_bul2,]
  #   									features <- bullet_scores %>% tidyr::unnest(data)
  #   									features %>% 
  # 									  ggplot(aes(x = landA, y = landB, fill = rfscore, colour=samesource)) +
  # 									  geom_tile() +
  # 									  labs(fill="Land Score") +
  # 									  scale_fill_gradient2(low = "grey80", high = "darkorange", midpoint = .5) +
  # 									  scale_colour_manual(global = c("black", "black")) +
  # 									  geom_tile(size = 1, data = features %>% filter(samesource)) +
  # 									  geom_text(aes(label = round(rfscore, 2)),size=6) +
  # 									  xlab(paste0("Land Name","(Bullet ",features$bulletA[1],")")) +
  # 									  ylab(paste0("Land Name","(Bullet ",features$bulletB[1],")")) +
  # 									  guides(colour="none") +
  # 									  coord_equal()+
  # 									  theme(
  # 									  		axis.text=element_text(size=16),
  # 									  		axis.title=element_text(size=18),
  # 									  		legend.title=element_text(size=18),
  # 									  		legend.text=element_text(size=16)
  # 									  	)
  #   						})
  # 
  #   	## Visualize Cross Cuts 
  #   	output$land_visCC <- renderPlot({
  #   									if(is.null(bulldata$comparison)) return(NULL)
  #   									if(is.null(input$comp_bul1) | is.null(input$comp_bul2)) return(NULL)
  # 
  #   									bullets <- bulldata$comparison$bullets
  #   									bullets <- bullets[bullets$bullet %in% c(input$comp_bul1,input$comp_bul2),]
  #   									crosscuts <- bullets %>% tidyr::unnest(ccdata)
  #   									crosscuts$x <- crosscuts$x/1000
  #   									CCplot <- crosscuts %>% 
  # 											  ggplot(aes(x = x, y = value)) + 
  # 											  geom_line() +
  # 											  facet_grid(bullet~land, labeller="label_both") +
  # 											  theme_bw()+
  # 											  xlab("Position along width of Land in Millimeters (1 Millimeter = 1000 Microns)") +
  # 											  ylab("Surface Height in Microns") + 
  # 											  ggtitle("Cross-section of the bullet land at the ideal cross-section location")+
  # 											  theme(
  # 											  		axis.text=element_text(size=16),
  # 											  		axis.title=element_text(size=18),
  # 											  		legend.title=element_text(size=18),
  # 											  		legend.text=element_text(size=16),
  # 											  		plot.title = element_text(size=22,face="bold"),
  # 											  		strip.text = element_text(size=18),
  # 											  		axis.text.x = element_text(angle = 90, hjust = 1)
  # 									  			)
  # 									return(CCplot)
  #   						})
  # 
  #   	## Visualize Signals
  #   	output$land_visSig <- renderPlot({
  #   									if(is.null(bulldata$comparison)) return(NULL)
  #   									if(is.null(input$comp_bul1) | is.null(input$comp_bul2)) return(NULL)
  # 
  #   									bullets <- bulldata$comparison$bullets
  #   									bullets <- bullets[bullets$bullet %in% c(input$comp_bul1,input$comp_bul2),]
  #   									signatures <- bullets %>% select(source,bullet,land, sigs) %>% tidyr::unnest(sigs)
  #   									signatures$x <- signatures$x/100
  #   									Sigplot <- signatures %>% 
  # 												  filter(!is.na(sig),!is.na(raw_sig)) %>%
  # 												  ggplot(aes(x = x)) + 
  # 												  geom_line(aes(y = raw_sig), colour = "grey70",show.legend = T) +
  # 												  geom_line(aes(y = sig), colour = "grey30",show.legend = T) +
  # 												  facet_grid(bullet~land, labeller="label_both") +
  # 												  ylim(c(-5,5)) +
  # 												  theme_bw() +
  # 												  xlab("Position along width of Land in Millimeters (1 Millimeter = 1000 Microns)") +
  # 												  ylab("Signal in Microns") +
  # 												  ggtitle("Raw and LOESS-smoothed Signal for Bullet Profile")+
  # 												  theme(
  # 											  		axis.text=element_text(size=16),
  # 											  		axis.title=element_text(size=18),
  # 											  		legend.title=element_text(size=18),
  # 											  		legend.text=element_text(size=16),
  # 											  		plot.title = element_text(size=22,face="bold"),
  # 											  		strip.text = element_text(size=18),
  # 											  		axis.text.x = element_text(angle = 90, hjust = 1)
  # 									  			)
  # 									return(Sigplot)
  #   						})
  #################################################################################
  #################################################################################
}