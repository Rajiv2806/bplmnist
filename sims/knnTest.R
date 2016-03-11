library(nnet)
library(class)

k.collect.training.set <- function(digits.subset, set.length = 10) {
  k.train.set <- digits.subset[1,]
  train.labels <- c()
  train.idcs <- c()
  while (length(train.labels) < set.length) {
    for (i in 1:nrow(digits.subset)) {
      label <- digits.subset[i,'X4']
      if (!(label %in% train.labels)) {
        train.idcs <- append(train.idcs, i)
        print(paste('Collected a:', label))
        train.labels <- append(train.labels, label)
        set.idx <- as.numeric(label+1)
        k.train.set <- rbind(k.train.set, digits.subset[i,])
      }
    }
  }
  return(list(k.train.set = k.train.set, train.idcs = train.idcs))
}
ntests <- 100
iter <- 1
accuracies.mat <- matrix(NA, nrow = ntests, ncol = 4)

while (iter <= ntests) {
  rand.idcs <- sample(1:nrow(digits), 100)
  digits.subset <- digits[rand.idcs,]
  
  k.t.set <- k.collect.training.set(digits.subset)
  # remove first row, for bad coding
  k.train.set <- k.t.set$k.train.set[2:11,]
  k.train.idcs <- k.t.set$train.idcs
  
  k.test.idcs <- setdiff(1:length(rand.idcs), k.train.idcs)
  k.test.set <- digits.subset[k.test.idcs,]
  
  k.train.set.cl <- k.train.set[,'X4']
  k.train.set.x <- k.train.set[,setdiff(colnames(k.train.set), 'X4')]
  k.test.set.cl <- k.test.set[,'X4']
  k.test.set.x <- k.test.set[,setdiff(colnames(k.test.set), 'X4')]
  
  k.preds <- knn(train = k.train.set.x,
                 test = k.test.set.x,
                 cl = k.train.set.cl,
                 k = 1)
  errors <- 0
  for (predi in 1:length(k.preds)) {
    if (k.preds[predi] != k.test.set.cl[predi]) {
      errors <- errors + 1
    }
  }
  accuracy <- 1-errors/length(k.preds)
  accuracies.mat[iter,] <- accuracy
  
  k.train.set$X4 <- as.factor(k.train.set$X4)
  k.train.set <- data.frame(k.train.set)
  sizes <- 1:3
  for (size in sizes) {
    mod.nn <- nnet(formula = (X4 ~ .), data = k.train.set, size = size)
    nnet.preds <- as.numeric(predict(mod.nn, k.test.set.x, type = 'class'))
    
    errors <- 0
    for (predi in 1:length(nnet.preds)) {
      if (nnet.preds[predi] != k.test.set.cl[predi]) {
        errors <- errors + 1
      }
    }
    accuracy <- 1-errors/length(nnet.preds)
    accuracies.mat[iter,(size+1)] <- accuracy
  }
  
  iter <- iter + 1
}

colnames(accuracies.mat) <- c('knn', 'nnet1', 'nnet2', 'nnet3')
write.csv(accuracies.mat, '~/Box Sync/abarciausksas/myfiles/bplmnist/data/othermodelssims.csv', row.names = FALSE)

png('others.png')
plot(accuracies.mat[,'knn'], ylim = c(0,1), xlim = c(0,100), col = 'cadetblue3', type = 'l')
lines(accuracies.mat[,'nnet1'], col = 'aquamarine2')
lines(accuracies.mat[,'nnet2'], col = 'aquamarine3')
lines(accuracies.mat[,'nnet3'], col = 'aquamarine4')
dev.off()
