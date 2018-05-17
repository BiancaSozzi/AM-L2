library(pixmap)

# IMPORTANT
#
# This file should be run at same place that directory faces_4 and the file
# images.txt
#


# Load images from list in images.txt
#
#   file: should use the value "images.txt"
#
read.data <- function(file="images.txt") {
	data <- NULL
	v.class <- NULL
	image.files <- read.table(file)

	for (file in 1:nrow(image.files)) {
		filename <- as.character(image.files[file,])
		image <- read.pnm(filename)

		class <- (strsplit(filename, "_"))[[1]][3]
		v.class <- c(v.class, class)

		example <- as.vector(slot(image, "grey"))
		data <- rbind(data,example)
	}

	data <- as.data.frame(data, row.names=1:nrow(image.files), stringsAsFactors=TRUE)
	data$class <- factor(v.class)

	data
}

## Read dataset and write in a csv file
write.csv(read.data("images.txt"), "faces.csv")

# Later to read it you would do
#
# data <- read.csv("faces.csv")
