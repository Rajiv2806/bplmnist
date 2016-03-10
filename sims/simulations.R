if (!require('devtools')) install.packages('devtools')
devtools::install_github("abarciauskas-bgse/bplmnist")
library(bplmnist)

data("mnist")
digits <- data

ntests <- 100
accuracies <- rep(NA, ntests)
random.accuracies <- rep(0, ntests)
iter <- 1

while (iter < ntests) {
  print(paste0('starting test: ', iter))
  # Select a random subset of digits
  rand.idcs <- sample(1:nrow(digits),100)

  thinned.ints <- collect.digit.skeletons(digits[rand.idcs,])

  # collect a training set
  training.collection <- collect.training.set(thinned.ints)
  train.set <- training.collection$train.set
  train.idcs <- training.collection$train.idcs

  # and a test set
  test.idcs <- setdiff(1:length(rand.idcs), train.idcs)
  test.set <- thinned.ints[test.idcs]

  train.objects <- list()
  for (i in 0:9) {
    digit <- train.set[[which(sapply(train.set, function(dig) { dig$label == i }))]]
    train.objects[[i+1]] <- train.digit(digit)#, animation = TRUE)
  }

  (test.distribution <- matrix(append(c(0:9), rep(0,10)), nrow = 10, ncol = 2))
  for (idx in 1:length(test.set)) {
    test.distribution[(test.set[[idx]]$label+1),2] <- test.distribution[(test.set[[idx]]$label+1),2] + 1
  }

  apriori.test.probs <- test.distribution[,2]/length(test.set)

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
  accuracies[iter] <- accuracy

  random.draws <- which(rmultinom(length(test.set), size = 1, prob = apriori.test.probs) == 1, arr.ind = TRUE)[,1]

  random.errors <- 0
  for (i in 1:length(test.set)) {
    if (test.set[[i]]$label != random.draws[i]) random.errors <- random.errors + 1
  }
  random.accuracies[iter] <- (random.accuracy <- 1 - random.errors/length(test.set))

  iter <- iter + 1
}
mean(accuracies)
mean(random.accuracies)

# ADD ME - write to unique file


sims <- read.csv('data/sims.csv')
randsims <- read.csv('data/sims-random.csv')

# ADD ME - save images
plot(as.numeric(randsims[,1]), ylim = c(0,1), xlim = c(0,100), col = 'red', type = 'l')
lines(randsims, col = 'red')
lines(sims, col = 'blue')

simsbin <- read.csv('data/sims-binary.csv')
randsimsbin <- read.csv('data/sims-binary-random.csv')

# ADD ME - save images
plot(as.numeric(randsimsbin[,1]), ylim = c(0,1), xlim = c(0,100), col = 'red', type = 'l')
lines(randsimsbin, col = 'red')
lines(simsbin, col = 'blue')

