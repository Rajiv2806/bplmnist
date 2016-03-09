# Collect a training set
# returns the training set and their indices from the argument of thinned ints
#' Title
#'
#' @param thinned.ints 
#'
#' @return # coming soon
#' @export
#'
#' @examples # coming soon
collect.training.set <- function(thinned.ints) {
  train.set <- list()
  train.labels <- c()
  train.idcs <- c()
  while (length(train.labels) < 10) {
    for (i in 1:length(thinned.ints)) {
      label <- thinned.ints[[i]]$label
      if (!(label %in% train.labels)) {
        train.idcs <- append(train.idcs, i)
        print(paste('Collected a:', label))
        train.labels <- append(train.labels, label)
        train.set <- append(train.set, list(thinned.ints[[i]]))
      }
    }
  }
  return(list(train.set = train.set, train.idcs = train.idcs))
}
