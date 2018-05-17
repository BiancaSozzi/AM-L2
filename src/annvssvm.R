# load utils functions read.dataset() split.data() confusion.matrix()
source("utils.R") 

deps <- c("utils.R") 

dependencies.loader(deps)

# INSTALE LAS librerías nnet y e1071

library(nnet) # neural network library
library(e1071) # svm library


# cm is the confusion matrix that is got by the method confusion.matrix()
# k is the index the class
true.positives <- function(cm, k) {
  res <- 0

  #######
  
  res <- cm[k,k]
  # print(paste("TP",res))
  
  ########

  res
}

# cm is the confusion matrix that is got by the method confusion.matrix()
# k is the index the class
true.negatives <- function(cm, k) {
  res <- 0

  #######
  
  res <- sum(cm[-k,-k])
  # print(paste("TN",res))
  
  ########

  res
}

# cm is the confusion matrix that is got by the method confusion.matrix()
# k is the index the class
false.positives <- function(cm, k) {
  res <- 0

  #######
  
  res <- sum(cm[-k,k])
  # print(paste("FP",res))
  
  ########

  res
}

# cm is the confusion matrix that is got by the method confusion.matrix()
# k is the index the class
false.negatives <- function(cm, k) {
  res <- 0

  #######
  
  res <- sum(cm[k,-k])
  # print(paste("FN",res))
  
  ########

  res
}


accuracy <- function(true.pos, true.neg, false.pos, false.neg) {
  res <- 0

  #######
  res <- (true.pos + true.neg)/(true.pos + true.neg + false.pos + false.neg)
  ########

  return(res)
}

precision <- function(true.pos, false.pos) {
  res <- 0

  #######
  res<- true.pos/(true.pos + false.pos)
  ########

  return(res)
}

recall <- function(true.pos, false.neg) {
  res <- 0

  #######
  
  res<- true.pos/(true.pos + false.neg)
  
  ########

  return(res)
}

f.measure <- function(true.pos, true.neg, false.pos, false.neg) {
  res <- 0

  #######
 
  pre <- precision(true.pos, false.pos)
  rec <- recall(true.pos, false.neg)
  
  res<- 2*((pre*rec)/(pre+rec))
  
  ########

  return(res)
}


results <- function(test.set, predicted.by){
  

  #matriz de confusión para los resultados de ANN
  mat <- confusion.matrix(test.set$class, predicted.by)
  
  print("Matriz de Confusi�n")
  print(mat)
  
  levs <- colnames(mat) #lista de labels/target o "niveles" de clasificación
  #left - right - straight - up
  
  #iteramos para mostrar los resultados (accuracy,recall,precision,fmeasure,etc)
  #de la clasificación en cada clase
  #imprimimos al final los resultados
  for (k in 1:length(levs)){
    
    tp <- true.positives(mat, k) #clasifico bien
    tn <- true.negatives(mat, k) #clasifico bien
    fp <- false.positives(mat, k) #clasifico mal
    fn <- false.negatives(mat, k) #clasifico mal
    
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
}


run.annvssvm <- function()
{
  ## Read dataset
  data <- read.dataset("../data/faces.csv")
  
  ## Split for train and test sets
  splits <- split.data(data, 0.3)
  # print(splits)
  train.set <- splits$train
  test.set <- splits$test
  
  ann <- NULL # clasificador aprendido por la red neuronal usando la función nnet
  svm <- NULL # clasificado aprendido por SVM usando la función svm
  
  predicted.by.ann <- c() #resultados de la predicción usando el modelo ANN (método predict)
  predicted.by.svm <- c() #resultados de la predicción usando el modelo SVM (método predict)
  
  #######
  ann <- nnet(formula=class~.,data= train.set, size=3, MaxNWts = 3000)
  svm <- svm(formula=class~., data= train.set)
  
  predicted.by.ann <- predict(ann, test.set, "class")
  predicted.by.svm <- predict(svm, test.set)
  
  ########
  
  print("\n Calculo para ann")
  results(test.set, predicted.by.ann)
  print("Calculo para svm")
  results(test.set, predicted.by.svm)
  
  
  # CREE LA MATRIZ DE CONFUSIÓN PARA LOS RESULTADOS CON SVM
  # PROCEDA A MOSTRAR LAS MÉTRICAS
  # PUEDE BASARSE EN EL CÓDIGO ANTERIOR PARA ANN
  #######
  #
  # ADD YOUR CODE HERE
  #
  ########
  
  # mat_ann
}

