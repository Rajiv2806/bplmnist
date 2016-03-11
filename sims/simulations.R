if (!require('devtools')) install.packages('devtools')
devtools::install_github("abarciauskas-bgse/bplmnist")
library(bplmnist)

data("mnist")
digits <- data
# WARNING: takes ~3 minutes 
all.digit.skeletons <- collect.digit.skeletons(digits)

ntests <- 100
accuracies <- rep(NA, ntests)
random.accuracies <- rep(0, ntests)
iter <- 1

system.time(while (iter <= ntests) {
  print(paste0('starting test: ', iter))
  # Select a random subset of digits
  rand.idcs <- sample(1:nrow(digits))
  # shuffle the skeletons so we get a new training and test set for this iteration
  digit.skeletons <- all.digit.skeletons[rand.idcs]

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
    train.objects[[i+1]] <- train.digit(digit)
  }

  results <- predict.mnist(train.objects, test.set)
  accuracy <- results$accuracy
  print(paste0('Accuracy: ', accuracy))
  apriori.test.probs <- results$apriori.test.probs
  accuracies[iter] <- accuracy

  random.draws <- which(rmultinom(length(test.set), size = 1, prob = apriori.test.probs) == 1, arr.ind = TRUE)[,1]

  random.errors <- 0
  for (i in 1:length(test.set)) {
    if (test.set[[i]]$label != random.draws[i]) random.errors <- random.errors + 1
  }
  random.accuracies[iter] <- (random.accuracy <- 1 - random.errors/length(test.set))

  iter <- iter + 1
})

mean(accuracies)
mean(random.accuracies)

# Write to unique file
simdata <- cbind(accuracies, random.accuracies)
colnames(simdata) <- c('accuracy.algorithm','accuracy.random')
ts <- as.numeric(Sys.time)
filename <- paste0(ts, 'sims.csv')
write.csv(simdata, filename, row.names = FALSE)

# generate a plot of the results
simresults <- read.csv(filename)
# ADD ME - save images
plot(simresults[,'accuracy.random'], ylim = c(0,1), xlim = c(0,100), col = 'red', type = 'l')
lines(simresults[,'accuracy.algorithm'], col = 'red')

# randsims <- read.csv('data/sims-random.csv')
# sims <- read.csv('data/sims.csv')
# png('sims1.png')
# plot(as.numeric(as.matrix(randsims)), ylim = c(0,1), xlim = c(0,100), col = 'red',type = 'l')
# lines(sims, col = 'blue')
# dev.off()

# randsims <- read.csv('data/sims-binary-random.csv')
# sims <- read.csv('data/sims-binary.csv')
# png('sims2.png')
# plot(as.numeric(as.matrix(randsims)), ylim = c(0,1), xlim = c(0,100), col = 'red',type = 'l')
# lines(sims, col = 'blue')
# dev.off()

