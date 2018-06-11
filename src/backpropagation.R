source("utils.R")

# Activation Function for the hidden variables
sigmoid <- function(x) {
  res <- 0

  #######
  res <- 1 / (1 + exp(-x))
  ########

  res
}

# Derivative of activation function of the hidden variables
dx.sigmoid <- function(x) {
  res <- 0

  #######
  res <- exp(-x) / ((1 + exp(-x)) ^ 2)
  ########

  res
}

# Activation function for the output variables
softmax <- function(x, xk) {
  res <- 0
  #######
  #x <- completo
  #xk <- actual
  res <- exp(xk)/sum(exp(x))
  ########

  res
}

# Derivative of activation function of the output variables
dx.softmax <- function(x, xk) {
  res <- 0
  #######
  #
  # ADD YOUR CODE HERE
  #
  ########

  res
}

# 1-of-K coding scheme to indicate the class. Given a factor this function
# returns a vector that follows the 1-of-K coding scheme
#
coding <- function(label) {
  t <- rep(0, length(levels(label)))

  for (i in 1:length(t))
    if (label == levels(label)[i]) {
      t[i] <- 1

      return(t)
    }

  t
}

# Function that determines when backpropagation algorithm should stop
has.satisfied.condition <- function(errors, eps, max.iter) {
  if (length(errors) > max.iter) return(TRUE)

  lasts <- tail(errors, 2)

  if (abs(lasts[2] - lasts[1]) <= eps) return(TRUE)


  return(FALSE)
}


# The sum-of-squares error function. 
# The backpropagation algorithm tries to minimize it

# The backpropagation algorithm tries to minimize it
error.function <- function(y, t) {
  # sum((y - t)^2) / 2 # <- regression error
  -sum(log(y)*t) # <- cross-entropy for multiclass

}
# Backpropagation algorithm
#
#
# The parameters are:
#
#  train.set: data.frame where its rows are examples
#
#  formula: a formula indicating which attribute of train.set is the class
#
#  eta: size of steep for gradient descent
#
#  n.out: number of output variables
#
#  n.hidden: number of hidden variables
#
#
# Return a list with the following components
#
#  wjk: learned weights between the hidden variables and output variables
#
#  wji: learned weights between the hidden variables and input variables
#
#  iters: number of iteration of algorithm
#
#  errors: vector of errors, each elements is the valur of error.function
#  during a iteraton of algorithm
#
backprop <- function(train.set, formula, eta=0.05, n.out, n.hidden, eps=1e-3, max.iter=10000) {
  
  # plus one to add the bias parameter
  n.in <- ncol(train.set)

  # matrices of weights of the neuronal network
  wji <- matrix(runif(n.in * n.hidden, -.05, .05), n.hidden, n.in)
  wkj <- matrix(runif(n.hidden * n.out, -.05, .05), n.out, n.hidden)

  # matrices of derivates of error with respecto to weights
  dx.wji <- matrix(0, n.hidden, n.in)
  dx.wkj <- matrix(0, n.out, n.hidden)

  # activations for the hidden layer
  a.j <- rep(0, nrow(wji))
  # values of function of activation for the hidden layer
  z.j <- rep(0, nrow(wji))

  # activations for the output layer
  a.k <- rep(0, nrow(wkj))
  # values of function of activation for the output layer, that represents the
  # output of the neuronal network: y_k(x,w) = z.k
  z.k <- rep(0, nrow(wkj))

  # counter for number of iteration
  n.iter <- 0

  # each element this vector is error of a iteration of backpropagation
  errors <- c(.Machine$double.xmax)

  repeat  {
    error <- 0

    for (m in 1:nrow(train.set)) {
      # add bias
      x.i <- append(as.matrix(model.frame(formula, train.set[m,])[-1]), 1, 0) #agrega un 1 al principio
      t <- coding(train.set[m, as.character(formula[2])]) ## devuelve el valor binario de la clase de la fila

      # propagate the input forward through the network
      #######
      for(j in 1:n.hidden){
        for(i in 1:length(x.i)){
          a.j[j] <- a.j[j] + wji[j,i] * x[i] 
        }
        z.j[j] <- sigmoid(a.j[j])
      }
      
      for(k in 1:n.out){
        for(j in 1:n.hidden){
          a.k[k] <- a.k[k] + wkj[k,j] * z.j[j] 
        }
        z.k[k] <- softmax(a.k,a.k[k])
      }
      ########

      # propagate the errors backward through the network
      #######
      #PAG 14 APUNTE 
      fi.j <- t(as.matrix(z.j))
      dk <- as.matrix(z.k - t)
      dx.wkj <- dk %*% fi.j # derivada de E respecto de wkj
      
      dh <- dx.sigmoid(a.j)
      fi.k <- t(as.matrix(wkj))
      sum.wkj_dk <- fi.k %*% dk
      dj <- dh * sum.wkj_dk
      dx.wij <- dj %*% x.i 
      
      # fi.i <- t(as.matrix)
      ########

      # gradient descent
      #
      # save current weights
      wji.old <- wji
      wkj.old <- wkj

      #######
      #
      # ADD YOUR CODE HERE
      #
      ########

      # compute the error of this iteration
      error <- error + error.function(z.k, t)
    }

    cat("ERROR:", error, "\n")

    errors <- append(errors, error)
    n.iter <- n.iter + 1

    if (has.satisfied.condition(errors, eps, max.iter))
      return(list(wkj=wkj.old, # weights kj of previous iteration
                  wji=wji.old, # weights ji of previous iteration
                  errors=errors,
                  iters=n.iter))  # return learned weights

    wji <- wji.old
    wkj <- wkj.old
  }
}
#
# FUNCION PRINCIPAL
#
run.backpropagation.experiment<- function()
{
  set.seed(1)
  datap <- 10
  ## Read dataset  
  data <- read.dataset("../data/faces.csv")
  data <- data[1:datap,] # estoy limitando el entrenamiento a 10 ejemplos
  
  ## Split for train and test sets
  splits <- split.data(data, 0.3)
  train.set <- splits$train
  test.set <- splits$test
  
  ## bp: modelo de red neuronal aprendido usando backpropagation
  ## La función puede demorarse al correr. Para depurar los errores de programación,
  ## ejecute backprop con un conjunto chico de train.set y max.iter < 20.
  bp <- backprop(train.set[],formula = class ~ .,eta=0.05, n.out = 4, n.hidden = 3, eps=1e-3, max.iter=10)
  
  ## grafique los errores por cada iteración de backpropagation
  #######
  #
  # ADD YOUR CODE HERE
  #
  ########
  
  ## utilizamos el conjunto de testeo para clasificar
  cla <- clasificar(test.set,formula=class ~ .,bp)
  
  
#  return(list(bp.model=bp,aciertos=cla$aciertos,fallidos=cla$fallidos))
  
}
#
# FUNCIÓN PARA SER UTILIZADA EN CLASIFICACIÓN DE LOS EJEMPLOS  
#

clasificar <- function(train.set, formula,bp) {
  
  n.in <- ncol(train.set)
  n.out <- nrow(bp$wkj)
  n.hidden <- nrow(bp$wji) 
  
  wji <- bp$wji
  wkj <- bp$wkj
  
  
  # activations for the hidden layer
  a.j <- rep(0, nrow(wji))
  # values of function of activation for the hidden layer
  z.j <- rep(0, nrow(wji))
  
  # activations for the output layer 
  a.k <- rep(0, nrow(wkj))
  # values of function of activation for the output layer, that represents the
  # output of the neuronal network: y_k(x,w) = z.k
  z.k <- rep(0, nrow(wkj))
  
  resultado<-matrix(data=0,nrow=nrow(train.set),ncol=n.out)
  acertado <- 0
  fallido <- 0
  
  for (m in 1:nrow(train.set)) {# for each example to classify
    
    # add bias
    x <- append(as.matrix(model.frame(formula, train.set[m,])[-1]), 1, 0)
    t <- coding(train.set[m, as.character(formula[2])])
    
    # propagate the input forward through the network
    
    #######
    #
    # ADD YOUR CODE HERE
    #
    ########
 
    temp <- rep(0,n.out)
    
    temp[which.max(z.k)] <- 1
    
    resultado[m,]<-temp
    
    print(c("comparando",temp,t,sum(temp==t)))
    
    if (sum(temp == t) == n.out)
      acertado <- acertado + 1
    else
      fallido <- fallido + 1
  }
  
  cat("FALLIDOS", fallido,"ACIERTOS",acertado, "\n")
  
  return(list(resultado=resultado,aciertos=acertado,errados=fallido))
  
}