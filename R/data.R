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
#' @keywords cluster
#' @md
"templateK40"

#' Cluster Fill Rates
#'
#' A data frame of cluster fill rates for two handwritten documents:
#' w0004_s01_pLND_r01.png and w0004_s01_pWOZ_r02.png. Both documents are from
#' the CSAFE Handwriting Database.
#'
#' 'handwriter' splits handwriting in the documents into component shapes called
#' *graphs*. The graphs are sorted into 40 clusters using the cluster template
#' 'templateK40'. The rates data frame shows the proportion of graphs from each
#' document assigned to each cluster. The rates estimate a writer profile for
#' the writer of a document.
#'
#' @format A data frame
#' \describe{
#' \item{docname}{The file name of the document without the file extension.}
#' \item{total_graphs}{The total number of graphs in the document.}
#'   \item{cluster1}{The proportion of graphs in cluster 1}
#'   \item{cluster2}{The proportion of graphs in cluster 2}
#'   \item{cluster3}{The proportion of graphs in cluster 3}
#'   \item{cluster4}{The proportion of graphs in cluster 4}
#'   \item{cluster5}{The proportion of graphs in cluster 5}
#'   \item{cluster6}{The proportion of graphs in cluster 6}
#'   \item{cluster7}{The proportion of graphs in cluster 7}
#'   \item{cluster8}{The proportion of graphs in cluster 8}
#'   \item{cluster9}{The proportion of graphs in cluster 9}
#'   \item{cluster10}{The proportion of graphs in cluster 10}
#'   \item{cluster11}{The proportion of graphs in cluster 11}
#'   \item{cluster12}{The proportion of graphs in cluster 12}
#'   \item{cluster13}{The proportion of graphs in cluster 13}
#'   \item{cluster14}{The proportion of graphs in cluster 14}
#'   \item{cluster15}{The proportion of graphs in cluster 15}
#'   \item{cluster16}{The proportion of graphs in cluster 16}
#'   \item{cluster17}{The proportion of graphs in cluster 17}
#'   \item{cluster18}{The proportion of graphs in cluster 18}
#'   \item{cluster19}{The proportion of graphs in cluster 19}
#'   \item{cluster20}{The proportion of graphs in cluster 20}
#'   \item{cluster21}{The proportion of graphs in cluster 21}
#'   \item{cluster22}{The proportion of graphs in cluster 22}
#'   \item{cluster23}{The proportion of graphs in cluster 23}
#'   \item{cluster24}{The proportion of graphs in cluster 24}
#'   \item{cluster25}{The proportion of graphs in cluster 25}
#'   \item{cluster26}{The proportion of graphs in cluster 26}
#'   \item{cluster27}{The proportion of graphs in cluster 27}
#'   \item{cluster28}{The proportion of graphs in cluster 28}
#'   \item{cluster29}{The proportion of graphs in cluster 29}
#'   \item{cluster30}{The proportion of graphs in cluster 30}
#'   \item{cluster31}{The proportion of graphs in cluster 31}
#'   \item{cluster32}{The proportion of graphs in cluster 32}
#'   \item{cluster33}{The proportion of graphs in cluster 33}
#'   \item{cluster34}{The proportion of graphs in cluster 34}
#'   \item{cluster35}{The proportion of graphs in cluster 35}
#'   \item{cluster36}{The proportion of graphs in cluster 36}
#'   \item{cluster37}{The proportion of graphs in cluster 37}
#'   \item{cluster38}{The proportion of graphs in cluster 38}
#'   \item{cluster39}{The proportion of graphs in cluster 39}
#'   \item{cluster40}{The proportion of graphs in cluster 40}}
#' @examples
#' plot_writer_profiles(rates)
#'
#' @keywords cluster
#' @md
"rates"
