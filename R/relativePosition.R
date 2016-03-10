#' relative.position
#'
#' @param point1 
#' @param point2 
#'
#' @return position 1-8 (where 1 is top left corner and increments are made in clockwise direction) of point2 relative to point1
#' @export
#'
#' @examples # coming soon
relative.position <- function(point1, point2) {
  position <- NA
  if (all(point1-c(1,-1) == point2)) {
    position <- 1
  } else if (all(point1+c(0,1) == point2)) {
    position <- 2
  } else if (all(point1+c(1,1) == point2)) {
    position <- 3
  } else if (all(point1+c(1,0) == point2)) {
    position <- 4
  } else if (all(point1+c(1,-1) == point2)) {
    position <- 5
  } else if (all(point1-c(0,1) == point2)) {
    position <- 6
  } else if (all(point1-c(1,1) == point2)) {
    position <- 7
  } else if (all(point1-c(1,0) == point2)) {
    position <- 8
  }
  
  return(position)
}

# Return the point at relative position [1,8] to the current point
relative.pt <- function(pixels, current.point, position) {
  pt.idx <- NA
  if (is.null(nrow(pixels))) return(pixels)
  # FIXME: not sure why this is necessary, should not be called to start?
  if (length(position) == 0) return(pixels)

  if (position == 1) {
    pt.idx <- my.row.match(current.point-c(1,-1), pixels)
  } else if (position == 2) {
    pt.idx <- my.row.match(current.point+c(0,1), pixels)
  } else if (position == 3) {
    pt.idx <- my.row.match(current.point+c(1,1), pixels)
  } else if (position == 4) {
    pt.idx <- my.row.match(current.point+c(1,0), pixels)
  } else if (position == 5) {
    pt.idx <- my.row.match(current.point+c(1,-1), pixels)
  } else if (position == 6) {
    pt.idx <- my.row.match(current.point-c(0,1), pixels)
  } else if (position == 7) {
    pt.idx <- my.row.match(current.point-c(1,1), pixels)
  } else if (position == 8) {
    pt.idx <- my.row.match(current.point-c(1,0), pixels)
  }
  
  return(pixels[pt.idx,])
}

