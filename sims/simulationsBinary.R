if (!require('devtools')) install.packages('devtools')
devtools::install_github("abarciauskas-bgse/bplmnist")
library(bplmnist)

data("mnist")
digits <- data
binary.digits <- subset(digits, X4 == 0 | X4 == 1)
all.digit.skeletons <- collect.digit.skeletons(binary.digits)

ntests <- 100
accuracies <- rep(NA, ntests)
random.accuracies <- rep(0, ntests)
iter <- 1

while (iter < ntests) {
  print(paste0('starting test: ', iter))
  # Select a random subset of digits
  rand.idcs <- sample(1:nrow(binary.digits),100)
  # shuffle the skeletons so we get a new training and test set for this iteration
  digit.skeletons <- all.digit.skeletons[rand.idcs]

  # collect a training set
  training.collection <- collect.training.set(digit.skeletons, set.length = 2)
  train.set <- training.collection$train.set
  train.idcs <- training.collection$train.idcs

  # and a test set
  test.idcs <- setdiff(1:length(rand.idcs), train.idcs)
  test.set <- digit.skeletons[test.idcs]

  train.objects <- list()
  for (i in 0:1) {
    digit <- train.set[[which(sapply(train.set, function(dig) { dig$label == i }))]]
    train.objects[[i+1]] <- train.digit(digit)
  }

  (results <- predict.mnist(train.objects, test.set, set.length = 2))
  accuracy <- results$accuracy
  apriori.test.probs <- results$apriori.test.probs
  accuracies[iter] <- accuracy

  random.draws <- which(rmultinom(length(test.set), size = 1, prob = apriori.test.probs) == 1, arr.ind = TRUE)[,1]

  random.errors <- 0
  for (i in 1:length(test.set)) {
    if (test.set[[i]]$label != random.draws[i]) random.errors <- random.errors + 1
  }
  random.accuracies[iter] <- (random.accuracy <- 1 - random.errors/length(test.set))

  iter <- iter + 1
}

