# draw boxplots for all similarities in a matrix for two configurations

setwd("C:/CS1_R-Intro/experiments-beamng-ai-wo-minlen-wo-infspeed-7-steering-4-len-20200821T084856Z-001")
setwd("C:/CS1_R-Intro/driver-ai-wo-minlen-wo-infspeed-7-steering-4-len-20200818T120651Z-001")


library(ggplot2)
library(hrbrthemes)

#metric1 = "jaccard_28alph.csv"
#metric2 = "jaccard_44alph.csv"
#metric3 = "jaccard_60alph.csv"
#metric1 = "jaccard_7ang_4len.csv" 
#metric2 = "jaccard_11ang_4len.csv"
#metric3 = "jaccard_15ang_4len.csv"
# uncomment for sliding window 1d alphabet size
#metric1 = "cursdl_sw_7ang_4len.csv"
#metric2 = "cursdl_sw_11ang_4len.csv"
#metric3 = "cursdl_sw_15ang_4len.csv"
# uncomment for sliding window 2d alphabet size
#metric1 = "sdl_2d_28alph.csv"
#metric2 = "sdl_2d_44alph.csv"
#metric3 = "sdl_2d_60alph.csv"
metric1 = "sdl2d_sw_7ang_4len.csv" 
metric2 = "sdl2d_sw_11ang_4len.csv"
metric3 = "sdl2d_sw_15ang_4len.csv"

#metric1 = "cur_sdl_lcs_dist_not_normalized.csv"
#metric2 = "cur_sdl_lcstr_dist_not_normalized.csv"

#metric1 = "cur_sdl_1_lcsstr_dist_not_normalized.csv"
#metric2 = "cur_sdl_5_lcstr_dist_not_normalized.csv"

similarity_matrix_1 <- read.csv(metric1, check.names=FALSE, row.names=1)
similarity_matrix_2 <- read.csv(metric2, check.names=FALSE, row.names=1)
similarity_matrix_3 <- read.csv(metric3, check.names=FALSE, row.names=1)

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
metric_3_list <- get_matrix_as_list(similarity_matrix_3)

# \n to move labels up
names <- c(paste(metric2, "\n"), metric2, paste(metric3, "\n"))
dframe_2_bxplt <- data.frame(metric_1_list, metric_2_list, metric_3_list)
dframe_2_bxplt.names = c(metric1, metric2, metric3)

#par(mar=c(6,4,4,1)+.1) # bigger margins are not needed
font_mult <- 1.5
boxplot(metric_1_list, metric_2_list, metric_3_list, names=names, ylim=c(0,1),
		cex.axis=font_mult, xaxt="n", mgp=c(4.5,3,3))
# stupid hacky thing to have 2 axis
axis(side=1, at=seq(1,3,2), labels=names[seq(1,3,2)], cex.axis=font_mult,
	 tick=FALSE, line=3.5)
axis(side=1, at=seq(2,3,2), labels=names[seq(2,3,2)], cex.axis=font_mult,
	tick=FALSE)
#ggplot(dframe_2_bxplt) +
#	geom_boxplot(aes(y=names[1]))
