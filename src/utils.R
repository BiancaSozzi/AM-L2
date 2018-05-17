#load(".vars.RData")

dependencies.loader <- function(deps) {
  for (i in 1:length(deps)) {
    file.i <- path.expand(deps[i])

    if (!file.exists(file.i))
      stop(paste("The file",
                 file.i,
                 "should be into the directory",
                 getwd(),
                 "to run this script."))
    else source(file.i)
  }
}

read.dataset <- function(filename, type="CSV") {
  file <- path.expand(filename)

  if (!file.exists(file))
    stop(paste("The file", filename, "does not exist."))

  if (type == "CSV") {
    data <- read.csv(file)
  }
  else {
    stop(paste("The type", type, "is unknown"))
  }

  return(data)
}

# Method to split the set data into two subset where one of subsets has perc
# percent of elements of data
#
split.data <- function(data, perc=1) {
  index <- sample(1:nrow(data), round(nrow(data) * perc))
  
  train <- data[-index,]
  test <- data[index,]
  
  return(list(train=train, test=test))
}

# Method to build the confusion matrix
confusion.matrix <- function(observed, predicted, levels=NULL) {
  if (is.null(levels)) levels <- levels(observed)
  
  table(factor(observed, levels), factor(predicted, levels))
}
