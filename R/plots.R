# The 'handwriterApp' R package performs writership analysis of handwritten
# documents. Copyright (C) 2024 Iowa State University of Science and Technology
# on behalf of its Center for Statistics and Applications in Forensic Evidence
#
# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# this program.  If not, see <https://www.gnu.org/licenses/>.

# Internal Functions ------------------------------------------------------

#' Plot Writer Profiles
#'
#' Create a line plot of cluster fill rates for one or more documents, where the
#' cluster fill rates serve as writer profiles. Each cluster fill rates for each
#' document are plotted as different colored lines.
#'
#' @param rates A data frame of cluster fill rates created with
#'   \code{\link[handwriterRF]{get_cluster_fill_rates}}
#'
#' @return A line plot
#'
#' @export
#' 
#' @examples
#' plot_writer_profiles(rates)
#' 
plot_writer_profiles <- function(rates) {
  # prevent note: "no visible binding for global variable"
  docname <- cluster <- rate <- NULL

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
