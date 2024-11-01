# make example rates data frame for plot_writer_profiles

rates <- handwriterRF::cfr

rates <- rates %>% dplyr::filter(docname %in% c("w0004_s01_pLND_r01", "w0004_s01_pWOZ_r02"))
plot_writer_profiles(rates)

usethis::use_data(rates)
