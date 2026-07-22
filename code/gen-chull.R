#' Generate a convex hull around clusters to be added to data
#'
#' Supplements a data set with information needed to draw a
#' convex hull.
#'
#' @param data data set
#' @param cl numeric vector of cluster labels
#' @return list with convex hull containing points and edges
#' @keywords cluster
#' @importFrom cxhull cxhull
#' @importFrom dplyr arrange mutate filter count
#' @export
#' @examples
#' data(clusters)
#' cl_dist <- dist(clusters[,1:5])
#' cl_hw <- hclust(cl_dist, method="ward.D2")
#' cl_w <- cutree(cl_hw, 3)
#' cl_chull <- gen_chull(clusters[,1:5], cl_w)
#' if (interactive()) {
#'   require(tourr)
#'   animate_xy(cl_chull$data[,1:5],
#'     col=cl_chull$data[,6],
#'     edges=cl_chull$edges,
#'     edges.col=cl_chull$edge_clr,
#'     axes="bottomleft")
#' }
gen_chull <- function(data, cl) {
  n <- nrow(data)
  p <- ncol(data)
  try (if(p < 2) stop("Number of variables needs to be at least 2."))
  #if (!is.factor(cl)) cl = factor(cl)
  
  # Check for duplicates
  # Calculation of convex hull cannot have duplicates
  dup <- duplicated(data)
  cl_data <- data |>
    filter(!dup) |>
    mutate(cl = cl) |>
    arrange(cl)
  
  # Arranging by cluster id is important to define edges
  ncl <- cl_data |>
    count(cl) |>
    arrange(cl) |>
    mutate(cumn = cumsum(n))
  
  # Calculate the convex hull for each cluster
  phull <- NULL
  for (i in unique(cl_data$cl)) {
    x <- cl_data |>
      dplyr::filter(cl == i)
    ph <- cxhull(as.matrix(x[,1:p]))$edges
    if (i > 1) {
      ph <- ph + ncl$cumn[i-1]
    }
    ph <- cbind(ph, rep(i, nrow(ph)))
    phull <- rbind(phull, ph)
  }
  phull <- as.data.frame(phull)
  colnames(phull) <- c("from", "to", "cl")
  phull$cl <- factor(phull$cl)
  cl_data$cl <- factor(cl_data$cl)
  
  return(list(data=cl_data,
              edges=as.matrix(phull[,1:2]),
              edge_clr=phull[,3]))
}