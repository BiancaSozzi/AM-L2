# These tests should be executed into directory src
#

source("backpropagation.R")
source("annvssvm.R")
colorize <- FALSE
#colorize <- require("xtermStyle", quietly = TRUE)

TOL <<- 1e-4

print.result <- function(real, tested) {
  
  res <- (real - tested) <= TOL

  if(colorize){
    if (res) cat(style("[OK]",fg="green"))
    else cat(style("[ERROR]",fg="red"));cat(style())
  }else{
    if (res) cat("[OK]")
    else cat("[ERROR]")
  }  
  
  cat(" ")

  return(res)
}

print.name.test <- function(num, name) {
  cat(paste("#", num, sep=""),
      paste("Testing", name))
}

am.l2.test01 <- function() {


  real <- 0.5057497
  res <- sigmoid(0.023)

  v <- print.result(real, res)
  print.name.test(01, "the Sigmoid function")
  cat("\n")

  v
}

am.l2.test02 <- function() {
  data <- c(1.2, 2.4, 1.7723, 2.72)

  real <- 0.3113189
  res <- softmax(data, data[2])


  v <- print.result(real, res)
  print.name.test(02, "the Softmax function")
  cat("\n")

  v
}

am.l2.test03 <- function() {
  real <- 0.220787
  res <- dx.sigmoid(0.71234)

  v <- print.result(real, res)
  print.name.test(03, "derivate of the Sigmoid function")
  cat("\n")

  v
}

am.l2.test04 <- function() {
  
  data <- c(1.2, 2.4, 1.7723, 2.72)

  real <- 0.2143994
  res <- dx.softmax(data, data[2])

  v <- print.result(real, res)
  print.name.test(04, "derivate of the Softmax function")
  cat("\n")

  v
}

am.l2.test05<- function()
{
  data <- rbind(c(2,3,4),c(1,2,3),c(7,8,9))
  
  real <- 2
  
  res <- true.positives(data, 1)
  
  v <- print.result(real, res)
  print.name.test(05, "true positives")
  cat("\n")
  
  v
  
}
am.l2.test06<- function()
{
  data <- rbind(c(2,3,4),c(1,2,3),c(7,8,9))
  
  real <- 22
  
  res <- true.negatives(data, 1)
  
  v <- print.result(real, res)
  print.name.test(06, "true negatives")
  cat("\n")
  
  v
  
}

am.l2.test07<- function()
{
  data <- rbind(c(2,3,4),c(1,2,3),c(7,8,9))
  
  real <- 7
  
  res <- false.negatives(data, 1)
  
  v <- print.result(real, res)
  print.name.test(07, "false negatives")
  cat("\n")
  
  v
  
}

am.l2.test08<- function()
{
  data <- rbind(c(2,3,4),c(1,2,3),c(7,8,9))
  
  real <- 8
  
  res <- false.positives(data, 1)
  
  v <- print.result(real, res)
  print.name.test(08, "false positives")
  cat("\n")
  
  v
  
}

am.l2.test09<- function()
{
  real <- 0.3
  
  res <- accuracy(1,2,3,4)
  
  v <- print.result(real, res)
  print.name.test(09, "accuracy")
  cat("\n")
  
  v
  
}

am.l2.test10<- function()
{
  real <- 0.3333333
  
  res <- precision(1,2)
  
  v <- print.result(real, res)
  print.name.test(10, "precision")
  cat("\n")
  
  v
  
}

am.l2.test11<- function()
{
  real <- 0.4285714
  
  res <- recall(3,4)
  
  v <- print.result(real, res)
  print.name.test(11, "recall")
  cat("\n")
  
  v
  
}

am.l2.test12<- function()
{
  real <- 0.2222222
  
  res <- f.measure(1,2,3,4)
  
  v <- print.result(real, res)
  print.name.test(12, "f.measure")
  cat("\n")
  
  v
  
}

run.private.tests <- function() {
  # clear screen
  # cat("\033[2J")

  cat("Set of private tests of laboratory 2", "\n\n")
  # get tests
  tests <- apropos("am.l2.test.*")

  passed <- 0

  # execute the tests
  for (i in 1:length(tests)) {
    if (do.call(tests[i], list())) passed <- passed + 1
  }

  ratio <- passed / length(tests)

  cat("\n\n\n")
  if (ratio >= 0.6)
    cat("Congratulation! You has passed this level. \n")
  else
    cat("Opps. You don't have passed this level...\n")

  cat("Your score is",
      paste("[",passed,"/",length(tests),"]",sep=""),
      "\n")
}
