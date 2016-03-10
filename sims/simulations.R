if (!require('devtools')) install.packages('devtools')
devtools::install_github("abarciauskas-bgse/bplmnist")
library(bplmnist)

data("mnist")
digits <- data

ntests <- 100
accuracies <- rep(NA, ntests)
random.accuracies <- rep(0, ntests)
all.digit.skeletons <- collect.digit.skeletons(digits)
iter <- 1

while (iter <= ntests) {
  print(paste0('starting test: ', iter))
  # Select a random subset of digits
  rand.idcs <- sample(1:nrow(digits))
  # shuffle the skeletons so we get a new training and test set for this iteration
  digit.skeletons <- all.digit.skeletons[rand.idcs,]

  # collect a training set
  training.collection <- collect.training.set(digit.skeletons)
  train.set <- training.collection$train.set
  train.idcs <- training.collection$train.idcs

  # and a test set
  test.idcs <- setdiff(1:length(rand.idcs), train.idcs)
  test.set <- digit.skeletons[test.idcs]

  train.objects <- list()
  for (i in 0:9) {
    digit <- train.set[[which(sapply(train.set, function(dig) { dig$label == i }))]]
    train.objects[[i+1]] <- train.digit(digit)#, animation = TRUE)
  }

  accuracy <- predict.mnist(train.objects, test.set)
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

# Write to unique file
simdata <- cbind(accuracies, random.accuracies)
colnames(simdata) <- c('accuracy.algorithm','accuracy.random')
ts <- as.numeric(Sys.time)
filename <- paste0(ts, 'sims.csv', row.names = FALSE)

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

