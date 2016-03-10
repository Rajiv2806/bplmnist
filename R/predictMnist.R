#' Title
#'
#' @param train.objects 
#' @param test.set 
#'
#' @return # coming soon
#' @export
#'
#' @examples # coming soon
predict.mnist <- function(train.objects, test.set) {
  # generate apriori probabilities
  (test.distribution <- matrix(append(c(0:9), rep(0,10)), nrow = 10, ncol = 2))
  for (idx in 1:length(test.set)) {
    test.distribution[(test.set[[idx]]$label+1),2] <- test.distribution[(test.set[[idx]]$label+1),2] + 1
  }
  apriori.test.probs <- test.distribution[,2]/length(test.set)
  
  # make sure we didn't do anything silly
  # TODO: add assertthat for this test
  sum(sapply(test.set, function(obj) { obj$label == 0}))/length(test.set) == apriori.test.probs[1]
  
  # create distribution of first major steps
  # 
  first.steps <- rep(0, 8)
  for (i in 1:8) {
    first.steps[i] <- sum(sapply(train.objects, function(obj) {
      fs <- obj$first.major.step
      ifelse(is.na(fs), FALSE, fs == i)
    }))
  }
  first.steps.dist <- first.steps/10
  
  # test objects
  errors <- 0
  for (i in 1:length(test.set)) {
    test.digit <- test.set[[i]]
    estimate <- train.digit(test.digit)
    
    # compare our test object with our training objects: e.g. predict the integer which has the most likely distribution of steps for the longest stroke, weighted by the difference it the length of the first stroke and weighted by the total number of strokes
    res <- lapply(train.objects, function(train.obj) {
      step.prob <- estimate$step.probs%*%train.obj$step.probs
      # probability given distribution of total steps for training object
      length.prob <- dpois(estimate$total.steps, lambda = train.obj$total.steps)
      strokes.prob <- dpois(estimate$num.real.strokes, lambda = train.obj$num.real.strokes)
      changes.prob <- dpois(estimate$changes.in.direction, lambda = train.obj$changes.in.direction)
      # if the first major step is the same, we want the probability of that major step given all the data 
      first.major.step.prob <- if ((!is.na(estimate$first.major.step)) &&
                                   (!is.na(train.obj$first.major.step)) &&
                                   (train.obj$first.major.step == estimate$first.major.step)) {
        first.steps.dist[estimate$first.major.step]
      } else {
        ifelse(is.na(estimate$first.major.step), 0.1, max(0.1*first.steps.dist[estimate$first.major.step], 0.1))
      }
      # all the likelihoods times the prior
      return(strokes.prob*first.major.step.prob*length.prob*changes.prob*apriori.test.probs[train.obj$label+1])
    })
    pred <- which.max(res)-1
    actual <- test.digit$label
    if (pred != actual) errors <- errors + 1
  }
  accuracy <- 1-errors/length(test.set)
  return(list(accuracy = accuracy, apriori.test.probs = apriori.test.probs))
}
