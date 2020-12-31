# Draw box plots for all similarity values in a matrix for two configurations
# Adjust the set, bng with or without OBEs
# Selecet two metrics (metric1, metric2) to plot
# W_YLIM: With or without ylim

# non OBE, use these only if the output metrics are not computed for with OBEs
setwd("C:/CS1_R-Intro/experiments-beamng-ai-no-obe-wo-minlen-wo-infspeed")
# OBE
setwd("C:/CS1_R-Intro/experiments-beamng-ai-wo-minlen-wo-infspeed-7-steering-4-len-20200821T084856Z-001")

library(ggplot2)
library(hrbrthemes)

# turn off the ylim for box plots for LCS and derivatives
W_YLIM = FALSE

metric1 = "cursdl_lcs_7ang_nnorm.csv"
metric2 = "cursdl_lcstr_7ang_nnorm.csv"

#metric1 = "cur_sdl_1_lcsstr_dist_not_normalized.csv"
#metric2 = "cur_sdl_5_lcstr_dist_not_normalized.csv"

#metric1 = "steering_bin.csv"
#metric2 = "steering_sin.csv"
#metric1 = "steering_adj_bin.csv"
#metric2 = "steering_adj_sin.csv"

#metric1 = "speed_bin.csv"
#metric2 = "speed_sin.csv"

#metric1 = "steering_speed_bin.csv"
#metric2 = "steering_speed_sin.csv"
#metric1 = "steering_speed_adj_bin.csv"
#metric2 = "steering_speed_adj_sin.csv"

# define width and height of plots for a more consistent presentation
wid <- 633
hei <- 633

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
# strip the .csv ending in the names
# load the corresponding function from "rand_samp_subset_obe_and_cov.R", use chdir
# this is shit, look into "C:/CS1_R-Intro/boxplots_3_similarities.R" for more info
if (!exists("vec_remove_file_endings")){
  prev_names <- names
  source("C:/CS1_R-Intro/rand_samp_subset_obe_and_cov.R")
  names <- prev_names
}
names <- vec_remove_file_endings(names)
dframe_2_bxplt <- data.frame(metric_1_list, metric_2_list)
dframe_2_bxplt.names = c(metric1, metric2)

# changing the output size for the plot
dev.new(width = wid, height = hei, unit="px", noRStudioGD=TRUE)

par(mar=c(5,5,4,1)+.1) # bigger margins for axis titles
font_mult <- 1.5
if (W_YLIM){
  boxplot(metric_1_list, metric_2_list, names=names, ylab="Similarity value", ylim=c(0,1),
		      cex.axis=font_mult, cex.lab=(font_mult+0.2))
} else {
  boxplot(metric_1_list, metric_2_list, names=names, ylab="Similarity value", 
          cex.axis=font_mult, cex.lab=(font_mult+0.2))
}
title(xlab="Similarity metric", line=3, cex.lab=(font_mult+0.2))

#ggplot(dframe_2_bxplt) +
#	geom_boxplot(aes(y=names[1]))
