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
  print(res)
  ########

  res
}

# cm is the confusion matrix that is got by the method confusion.matrix()
# k is the index the class
true.negatives <- function(cm, k) {
  res <- 0

  #######
  #
  # ADD YOUR CODE HERE
  #
  ########

  res
}

# cm is the confusion matrix that is got by the method confusion.matrix()
# k is the index the class
false.positives <- function(cm, k) {
  res <- 0

  #######
  #
  # ADD YOUR CODE HERE
  #
  ########

  res
}

# cm is the confusion matrix that is got by the method confusion.matrix()
# k is the index the class
false.negatives <- function(cm, k) {
  res <- 0

  #######
  #
  # ADD YOUR CODE HERE
  #
  ########

  res
}


accuracy <- function(true.pos, true.neg, false.pos, false.neg) {
  res <- 0

  #######
  #
  # ADD YOUR CODE HERE
  #
  ########

  return(res)
}

precision <- function(true.pos, false.pos) {
  res <- 0

  #######
  #
  # ADD YOUR CODE HERE
  #
  ########

  return(res)
}

recall <- function(true.pos, false.neg) {
  res <- 0

  #######
  #
  # ADD YOUR CODE HERE
  #
  ########

  return(res)
}

f.measure <- function(true.pos, true.neg, false.pos, false.neg) {
  res <- 0

  #######
  #
  # ADD YOUR CODE HERE
  #
  ########

  return(res)
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
  
  ann <- NULL # clasificador aprendido por la red neuronal usando la función nnet
  svm <- NULL # clasificado aprendido por SVM usando la función svm
  
  predicted.by.ann <- c() #resultados de la predicción usando el modelo ANN (método predict)
  predicted.by.svm <- c() #resultados de la predicción usando el modelo SVM (método predict)
  
  #######
  # print(dim(train.set))
  ann <- nnet(formula=class~., data=train.set, size=3, MaxNWts= 3000)
  svm <- svm(formula=class~., data= train.set)
  # print(svm)
  predicted.by.ann <- predict(ann, test.set, "class")
  predicted.by.svm <- predict(svm, test.set)
  ########
  
  #matriz de confusión para los resultados de ANN
  mat_ann <- confusion.matrix(test.set$class, predicted.by.ann)
  
  print("Matriz de confusion")
  print(mat_ann)
  
  levs <- colnames(mat_ann) #lista de labels/target o "niveles" de clasificación
  # print("levs")
  # print(levs)
  #iteramos para mostrar los resultados (accuracy,recall,precision,fmeasure,etc)
  #de la clasificación en cada clase
  #imprimimos al final los resultados
  for (k in 1:length(levs)){
    
    tp_ann <- true.positives(mat_ann, k)
    tn_ann <- true.negatives(mat_ann, k)
    fp_ann <- false.positives(mat_ann, k)
    fn_ann <- false.negatives(mat_ann, k)
    
    acc_ann <- accuracy(tp_ann, tn_ann, fp_ann, fn_ann)
    prec_ann <- precision(tp_ann,fp_ann)
    rec_ann <- recall(tp_ann, fn_ann)
    f_ann <- f.measure(tp_ann, tn_ann, fp_ann, fn_ann)
    
    # cat("\nk = ",k, ", Class:", levs[k], " tp:",tp_ann," tn:",tn_ann," fp:",fp_ann,"  fn:", fn_ann,
    #     "\nAccuracy: ", acc_ann,
    #     "\nPrecision:", prec_ann,
    #     "\nRecall    ", rec_ann,
    #     "\nF-measure:", f_ann)
  }
  
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

