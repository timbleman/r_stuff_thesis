# draw boxplots for all similarities in a matrix for two configurations

setwd("C:/CS1_R-Intro/experiments-beamng-ai-wo-minlen-wo-infspeed-7-steering-4-len-20200821T084856Z-001")

library(ggplot2)
library(hrbrthemes)

metric1 = "jaccard.csv"
metric2 = "jaccard_11ang.csv"

similarity_matrix_1 <- read.csv(metric1, check.names=FALSE, row.names=1)
similarity_matrix_2 <- read.csv(metric2, check.names=FALSE, row.names=1)


if (length(similarity_matrix_1) != length(similarity_matrix_2)){
	print("Jo, matrices are not of same size, this will break everything!")
}
get_matrix_as_list <- function(matrix){
	metric_list = rep(0, length(matrix))
	i <- 1
	for (col in matrix){
		for (el in col){
			metric_list[i] <- el
			i <- i + 1
		}
	}
	return(metric_list)
}

metric_1_list <- get_matrix_as_list(similarity_matrix_1)
metric_2_list <- get_matrix_as_list(similarity_matrix_2)

names <- c(metric1, metric2)
dframe_2_bxplt <- data.frame(metric_1_list, metric_2_list)
dframe_2_bxplt.names = c(metric1, metric2)

boxplot(metric_1_list, metric_2_list, names=names)
#ggplot(dframe_2_bxplt) +
#	geom_boxplot(aes(y=names[1]))
