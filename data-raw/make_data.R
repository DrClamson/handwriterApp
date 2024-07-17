templateK8 <- readRDS(file.path("data-raw", "templateK8.rds"))
templateK8$template_graphs <- NULL
usethis::use_data(templateK8, overwrite = TRUE)

templateK40 <- readRDS(file.path("data-raw", "templateK40.rds"))
templateK40$template_graphs <- NULL
usethis::use_data(templateK40, overwrite = TRUE)
