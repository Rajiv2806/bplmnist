# find which rows match the first item in the point
#' Title
#'
#' @param pt 
#' @param m 
#'
#' @return # coming soon
#' @export
#'
#' @examples # coming soon
my.row.match <- function(pt, m) {
  matching.rows.idcs <- which(m[,1] == pt[1])
  matching.pt1 <- m[matching.rows.idcs,]
  matching.col.idcs <- ifelse(is.null(nrow(matching.pt1)), 1, which(matching.pt1[,2] == pt[2]))
  return(matching.rows.idcs[matching.col.idcs][1])
}
