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

#' Small Cluster Template with 8 Clusters
#'
#' A small cluster template created by 'handwriter' with K=8
#' clusters. This template was created from 10 handwriting samples from the
#' CSAFE Handwriting Database. This small template should only be used for
#' examples. Use the 'templateK40' for casework.
#'
#' 'handwriter' splits handwriting samples into component shapes
#' called *graphs*. The graphs are sorted into 8 clusters with a K-Means
#' algorithm. See 'handwriter' for more details.
#'
#' @format A list containing the contents of the cluster template.
#' \describe{
#' \item{centers_seed}{An integer for the random number generator use to select the
#' starting cluster centers for the K-Means algorithm.}
#' \item{cluster}{A vector of cluster assignments
#'   for each graph used to create the cluster template. The clusters are numbered sequentially 1, 2,...,K.}
#' \item{centers}{The final cluster centers produced by the K-Means algorithm.}
#' \item{K}{The number of clusters in the template.}
#' \item{n}{The number of training graphs to used to create the template.}
#' \item{docnames}{A vector that lists the training document from which each graph originated.}
#' \item{writers}{A vector that lists the writer of each graph.}
#' \item{iters}{The maximum number of iterations for the K-means
#'   algorithm.}
#' \item{changes}{A vector of the number of graphs that
#'   changed clusters on each iteration of the K-means algorithm.}
#' \item{outlierCutoff}{A vector of the outlier cutoff values calculated on
#'   each iteration of the K-means algorithm.}
#' \item{stop_reason}{The reason the
#'   K-means algorithm terminated.}
#' \item{wcd}{A matrix of the within cluster
#'   distances on each iteration of the K-means algorithm. More specifically,
#'   the distance between each graph and the center of the cluster to which it
#'   was assigned  on each iteration.}
#' \item{wcss}{A vector of the
#'   within-cluster sum of squares on each iteration of the K-means algorithm.}}
#' @examples
#' # view cluster fill counts for the template training documents
#' template_data <- handwriter::format_template_data(templateK8)
#' handwriter::plot_cluster_fill_counts(template_data, facet = TRUE)
#'
#' @md
"templateK8"

#' Cluster Template with 40 Clusters
#'
#' A cluster template created by 'handwriter' with K=40
#' clusters. This template was created from 100 handwriting samples from the
#' CSAFE Handwriting Database. This template is suitable for casework.
#'
#' 'handwriter' splits handwriting samples into component shapes
#' called *graphs*. The graphs are sorted into 40 clusters with a K-Means
#' algorithm. See 'handwriter' for more details.
#'
#' @format A list containing the contents of the cluster template.
#' \describe{
#' \item{centers_seed}{An integer for the random number generator use to select the
#' starting cluster centers for the K-Means algorithm.}
#' \item{cluster}{A vector of cluster assignments
#'   for each graph used to create the cluster template. The clusters are numbered sequentially 1, 2,...,K.}
#' \item{centers}{The final cluster centers produced by the K-Means algorithm.}
#' \item{K}{The number of clusters in the template.}
#' \item{n}{The number of training graphs to used to create the template.}
#' \item{docnames}{A vector that lists the training document from which each graph originated.}
#' \item{writers}{A vector that lists the writer of each graph.}
#' \item{iters}{The maximum number of iterations for the K-means
#'   algorithm.}
#' \item{changes}{A vector of the number of graphs that
#'   changed clusters on each iteration of the K-means algorithm.}
#' \item{outlierCutoff}{A vector of the outlier cutoff values calculated on
#'   each iteration of the K-means algorithm.}
#' \item{stop_reason}{The reason the
#'   K-means algorithm terminated.}
#' \item{wcd}{The within cluster
#'   distances on the final iteration of the K-means algorithm. More specifically,
#'   the distance between each graph and the center of the cluster to which it
#'   was assigned  on each iteration. The output of 'handwriter::make_clustering_template' stores
#'   the within cluster distances on each iteration, but the previous iterations were removed here to reduce the file size.}
#' \item{wcss}{A vector of the
#'   within-cluster sum of squares on each iteration of the K-means algorithm.}}
#' @examples
#' # view number of clusters
#' templateK40$K
#' 
#' # view number of iterations
#' templateK40$iters
#' 
#' # view cluster centers
#' templateK40$centers
#'
#' @md
"templateK40"
