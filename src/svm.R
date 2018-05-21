source("utils.R")
source('annvssvm.R')
deps <- c("utils.R")

dependencies.loader(deps)

# INSTALE LAS LIBRERIAS nnet y e1071, si no las tiene instaladas

library(nnet)
library(e1071)

# Method to split the set data into two subset where one of subsets has perc
# percent of elements of data
#
split.data <- function(data, perc=1) {
  index <- sample(1:nrow(data), round(nrow(data) * perc))

  train <- data[-index,]
  test <- data[index,]

  return(list(train=train, test=test))
}

# METODO svm.kfold
# En k-fold cross-validation, los datos son particionados aleatoriamente en k subconjuntos
# De los k subconjuntos, uno sólo de ellos es usado para validación y testeo del modelo,
# dejando los k-1 subconjuntos restantes para entrenamiento. El proceso de cross-validation
# es repetido k veces (los k-folds), con cada uno de los k subconjuntos usando tan sólo uno
# para validación.
# Luego, los k resultados pueden ser promediados para mostrar una simple estimación.
# Al repetirse este método aleatoriamente sobre subconjuntos, todas las observaciones son usadas
# tanto para entrenamiento como validación, y cada subconjunto es usado para validación
# exactamente una vez.
# PARAMETROS
# data: datos de entrenamiento usados para aprender el modelo svm.model
# k: el número de subconjuntos en el que debe particionarse los datos
# gammas: array con valores gamma
# costs: array con valores de costos


svm.kfold <- function(gammas,costs,data,k)
{

  best.model <- NULL

  summary <- data.frame()

  all.indexes <- sample(nrow(data), nrow(data), replace=FALSE)

  validate.set.size <- trunc( nrow(data)/(k) )

  iterations <- 1:k

  max.accuracy <- 0
  best.gamma <- NULL
  best.cost <- NULL

  for(gamma in gammas){

    for(cost in costs){

      sum.accuracy <- 0
      accuracy.avg <- 0

      for(i in iterations) {

        #######
        
        hasta <- i * validate.set.size
        desde <- hasta - validate.set.size + 1
        test.subset <- data[desde:hasta,]
        train.subset <- data[-(desde:hasta),]
        svm <- svm(formula=class~., data=train.subset)
        predicted.by.svm <- predict(svm, test.subset)
        acc <- results(test.subset, predicted.by.svm)
        sum.accuracy <- sum.accuracy + acc
        
        ########
      }
      
      accuracy.avg <- sum.accuracy / k

      if(!is.na(accuracy.avg) && max.accuracy < accuracy.avg) {
        max.accuracy <- accuracy.avg
        best.gamma <- gamma
        best.cost <- cost
      }

      cat("gamma: ",gamma, " cost:",cost," accuracy:",accuracy.avg," max.accuracy:",max.accuracy,"\n")
      summary <- rbind(summary, c(gamma=gamma, cost=cost, accuracy=accuracy.avg))
    }
  }

  cat("Best parameters\n gamma: ",best.gamma, " cost:",best.cost," accuracy:",max.accuracy,"\n")
  cat("Train svm model with best parameters...\n")
  best.model <- svm(formula=class ~ . , data=data, cost=best.cost, gamma=best.gamma)
  ret <- list(model=best.model, summary=summary)
  ret
}

results <- function(test.set, predicted.by){
  #matriz de confusión para los resultados de ANN
  mat <- confusion.matrix(test.set$class, predicted.by)
  
  print("Matriz de confusion")
  print(mat)
  
  levs <- colnames(mat) #lista de labels/target o "niveles" de clasificación
  # print("levs")
  # print(levs)
  #iteramos para mostrar los resultados (accuracy,recall,precision,fmeasure,etc)
  #de la clasificación en cada clase
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
    
    return(acc)
  }
  
}

run.kfold.experiment <- function()
{
  # trabajaremos con 5-fold- cross validation
  k <- 5
  gammas <- c(0.000001,0.00001,0.0001,0.001,0.01,0.1)
  costs <- c(1,10,50,100,400,1000)

  ## Read dataset
  data <- read.csv('../data/faces.csv')

  ## Split for train and test sets
  splits <- split.data(data, 0.3)
  train.set <- splits$train
  test.set <- splits$test
  ## Entrenar un modelo SVM usando kfold cross-validation
  svm.model <- svm.kfold(gammas,costs,train.set,k)

  pred.svm <- predict(svm.model$model, test.set)
  result.svm <- cbind(test.set$class, factor(pred.svm))
  acc <- length(result.svm[result.svm[,1]==result.svm[,2]]) / length(result.svm)
  cat("Accuracy with testing data: ", acc, "\n")
}
