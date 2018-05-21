# load utils functions read.dataset() split.data() confusion.matrix()
source("utils.R") 

deps <- c("utils.R") 

dependencies.loader(deps)

# INSTALE LAS librerÃ­as nnet y e1071

library(nnet) # neural network library
library(e1071) # svm library


# cm is the confusion matrix that is got by the method confusion.matrix()
# k is the index the class
true.positives <- function(cm, k) {
  res <- 0

  #######
  res <- cm[k,k]
  print(res)
  ########

  res
}

# cm is the confusion matrix that is got by the method confusion.matrix()
# k is the index the class
true.negatives <- function(cm, k) {
  res <- 0

  #######
  cm <- cm[-k,-k]
  res <- sum(cm)
  ########

  res
}

# cm is the confusion matrix that is got by the method confusion.matrix()
# k is the index the class
false.positives <- function(cm, k) {
  res <- 0

  #######
  res <- sum(cm[-k,k])
  ########

  res
}

# cm is the confusion matrix that is got by the method confusion.matrix()
# k is the index the class
false.negatives <- function(cm, k) {
  res <- 0

  #######
  res <- sum(cm[k,-k])
  ########

  res
}


accuracy <- function(true.pos, true.neg, false.pos, false.neg) {
  res <- 0

  #######
  res <- (true.pos+true.neg)/(true.pos+true.neg+false.pos+false.neg)
  ########

  return(res)
}

precision <- function(true.pos, false.pos) {
  res <- 0

  #######
  res <- true.pos / (true.pos+false.pos)
  ########

  return(res)
}

recall <- function(true.pos, false.neg) {
  res <- 0

  #######
  res <- true.pos / (true.pos+false.neg)
  ########

  return(res)
}

f.measure <- function(true.pos, true.neg, false.pos, false.neg) {
  res <- 0

  #######
  prec <- precision(true.pos,false.pos)
  rec <- recall(true.pos, false.neg)
  
  res <- 2*(prec*rec)/(prec+rec)
  ########

  return(res)
}

results <- function(test.set, predicted.by){
  #matriz de confusiÃ³n para los resultados de ANN
  mat <- confusion.matrix(test.set$class, predicted.by)
  
  print("Matriz de confusion")
  print(mat)
  
  levs <- colnames(mat) #lista de labels/target o "niveles" de clasificaciÃ³n
  # print("levs")
  # print(levs)
  #iteramos para mostrar los resultados (accuracy,recall,precision,fmeasure,etc)
  #de la clasificaciÃ³n en cada clase
  #imprimimos al final los resultados
  for (k in 1:length(levs)){
    
    tp <- true.positives(mat, k)
    tn <- true.negatives(mat, k)
    fp <- false.positives(mat, k)
    fn <- false.negatives(mat, k)
    
    acc <- accuracy(tp, tn, fp, fn)
    prec <- precision(tp,fp)
    rec <- recall(tp, fn)
    f <- f.measure(tp, tn, fp, fn)
    
    cat("\nk = ",k, ", Class:", levs[k], " tp:",tp," tn:",tn," fp:",fp,"  fn:", fn,
        "\nAccuracy: ", acc,
        "\nPrecision:", prec,
        "\nRecall    ", rec,
        "\nF-measure:", f)
  }
  return(acc)
  
}

run.annvssvm <- function()
{
  ## Read dataset
  data <- read.dataset("../data/faces.csv")
  
  ## Split for train and test sets
  splits <- split.data(data, 0.3)
  train.set <- splits$train
  test.set <- splits$test
  # print(test.set)
  
  ann <- NULL # clasificador aprendido por la red neuronal usando la funciÃ³n nnet
  svm <- NULL # clasificado aprendido por SVM usando la funciÃ³n svm
  
  predicted.by.ann <- c() #resultados de la predicciÃ³n usando el modelo ANN (mÃ©todo predict)
  predicted.by.svm <- c() #resultados de la predicciÃ³n usando el modelo SVM (mÃ©todo predict)
  
  #######
  # print(dim(train.set))
  ann <- nnet(formula=class~., data=train.set, size=3, MaxNWts= 3000)
  svm <- svm(formula=class~., data= train.set)
  # print(svm)
  predicted.by.ann <- predict(ann, test.set, "class")
  predicted.by.svm <- predict(svm, test.set)
  ########
  print("Calculo para ANN")
  results(test.set,predicted.by.ann)
  print("Calculo para SVM")
  results(test.set,predicted.by.svm)
  
  
  #######
  #
  # ADD YOUR CODE HERE
  #
  ########
  
  # mat_ann
}

