plot_writer_profiles <- function(rates) {

  rates <- rates %>% 
    tidyr::pivot_longer(cols = -tidyr::any_of(c("docname", "total_graphs")), names_to = "cluster", values_to = "rate") %>%
    dplyr::mutate(docname = factor(docname),
                  cluster = as.integer(stringr::str_replace(cluster, "cluster", "")))
  p <- rates %>% 
    ggplot2::ggplot(ggplot2::aes(x = cluster, 
                                 y = rate, 
                                 color = docname)) + 
    ggplot2::geom_line() + 
    ggplot2::geom_point() + 
    ggplot2::theme_bw()
  
  return(p)
}
