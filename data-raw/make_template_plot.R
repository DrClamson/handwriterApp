templateK40 <- readRDS(file.path("data-raw", "templateK40.rds"))
handwriter::plot_cluster_centers(templateK40)
ggplot2::ggsave(filename = "inst/extdata/images/template.png", bg=NULL)
