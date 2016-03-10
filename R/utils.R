#' plot.point
#' color the point on the plot with given color
#'
#' @param point 
#' @param color defaults to cadetblue
#'
#' @return # coming soon
#' @export
#'
#' @examples # coming soon
plot.point <- function(point, color = 'cadetblue') {
  points(point[1], point[2], col = color, pch = 19)
}

#' Title
#'
#' @param x 
#' @param y 
#'
#' @return # coming soon
#' @export
#'
#' @examples # coming soon
angle <- function(x, y) {
  x1 <- x[1]
  x2 <- x[2]
  y1 <- y[1]
  y2 <- y[2]  
  dot = x1*x2 + y1*y2      # dot product
  det = x1*y2 - y1*x2      # determinant
  angle = atan2(det, dot)  # atan2(y, x) or atan2(sin, cos)
  return(angle)
}

#' Title
#'
#' @param rad 
#'
#' @return # coming soon
#' @export
#'
#' @examples # coming soon
rad2deg <- function(rad) {(rad * 180) / (pi)}

#' Title
#'
#' @param points 
#'
#' @return # coming soon
#' @export
#'
#' @examples # coming soon
dist.from.origin <- function(points) {
  (points[1] - points[2])**2
}

#' Title
#'
#' @param x 
#'
#' @return # coming soon
#' @export
#'
#' @examples # coming soon
my.mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#' Title
#'
#' @param point1 
#' @param point2 
#'
#' @return # coming soon
#' @export
#'
#' @examples # coming soon
distance <- function(point1, point2) {
  sqrt((point2[1] - point1[1])**2 + (point2[2] - point1[2])**2)
}

#' Title
#'
#' @param matrix 
#'
#' @return # coming soon
#' @export
#'
#' @examples # coming soon
distances <- function(matrix) {
  n <- nrow(matrix)
  dist.matrix <- matrix(NA, n, n)
  for (idx in 1:n) {
    x.current <- matrix[idx,]
    x.current.expanded <- matrix(x.current, nrow = n, ncol = 2, byrow = TRUE)
    dist.matrix[idx, ] <- calcDist(matrix, x.current.expanded, 2)
  }
  return(dist.matrix)
}

#' Title
#'
#' @param x 
#'
#' @return # coming soon
#' @export
#'
#' @examples # coming soon
rotate <- function(x) { t(apply(x, 2, rev)) }

#' Title
#'
#' @param points 
#' @param relativity 
#'
#' @return # coming soon
#' @export
#'
#' @examples # coming soon
nearorfar.from.origin <- function(points, relativity = 'nearest') {
  dists <- apply(points, 1, dist.from.origin)
  pos <- NA
  if (relativity == 'nearest') {
    pos <- which.min(dists)
  } else if (relativity == 'furthest') {
    pos <- which.max(dists)
  }
  return(pos)
}

#' Title
#'
#' @param point 
#' @param points 
#' @param order 
#'
#' @return # coming soon
#' @export
#'
#' @examples # coming soon
nearest.point <- function(point, points, order=1) {
  current.row <- row.match(point, points)
  x.distances <- distances(points)[current.row,]
  ordered.distances <- x.distances[order(x.distances)]
  nearest.idx <- setdiff(order(x.distances), current.row)[order]
  return(points[nearest.idx,])
}

#' Title
#'
#' @param point 
#' @param points 
#'
#' @return # coming soon
#' @export
#'
#' @examples # coming soon
neighbors.directions <- function(point, points) {
  current.row <- row.match(point, points)
  x.distances <- distances(points)[current.row,]
  neighbors <- setdiff(which(x.distances <= sqrt(2)), current.row)
  neighbors.points <- points[neighbors,]
  relative.positions <- NA
  if (length(neighbors) > 1) {
    relative.positions <- apply(neighbors.points, 1, function(n) {
      relative.position(point,n)
    })
  } else {
    relative.positions <- relative.position(point, neighbors.points)
  }
  directions <- relative.positions
  return(list(directions = directions, neighbors = neighbors.points))
}

