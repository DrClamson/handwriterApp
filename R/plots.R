plot_writer_profile <- function(counts) {
  add_missing_clusters <- function(counts) {
    counts <- counts %>% 
      tidyr::pivot_longer(cols = -c(1, 2, 3), names_to = "cluster", values_to = "count") %>%
      dplyr::mutate(cluster = as.integer(cluster))
    missing <- setdiff(1:40, counts$cluster)
    docname <- unique(counts$docname)
    writer <- unique(counts$writer)
    doc <- unique(counts$doc)
    missing <- data.frame("docname" = docname, 
                          "writer" = writer,
                          "doc" = doc,
                          "cluster" = missing,
                          "count" = 0)
    counts <- rbind(counts, missing)
    return(counts)
  }
  
  counts <- add_missing_clusters(counts)
  counts <- counts %>% dplyr::mutate(writer = factor(writer),
                                     doc = factor(doc),
                                     cluster = as.integer(cluster))
  p <- counts %>% 
    ggplot2::ggplot(ggplot2::aes(x = cluster, 
                                 y = count, 
                                 group = interaction(writer, doc), 
                                 linetype = writer)) + 
    ggplot2::geom_line() + 
    ggplot2::geom_point() + 
    # ggplot2::scale_x_continuous(breaks = as.integer(unique(counts$cluster))) + 
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none")

  return(p)
}
