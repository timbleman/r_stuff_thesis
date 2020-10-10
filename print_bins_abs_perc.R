# print bins absolute or as percentage for a test
library(fields)

setwd("C:/CS1_R-Intro/driver-ai-wo-minlen-wo-infspeed-7-steering-4-len-20200818T120651Z-001")

dimensions <- 2
st_or_sp <- "sp"
if(dimensions == 2){ 
	# these have to be adjusted by the user
	bin_metric <- "speed_steering_2d_bins.csv"
	bin_metric <- "speed_steering_2d_bins_adjusted.csv"
}
if(dimensions == 1){ 
	if(st_or_sp == "st") {
		bin_metric <- "steering_bins.csv"
	}
	if(st_or_sp == "sp") {
		bin_metric <- "speed_bins.csv"
	}
}
as_percentage <- FALSE
# These have to be correctly entered!!!
x_dim = 16
if(dimensions == 1){ 
	y_dim = 1
} else {
	y_dim = 16
}
NUM_COL <- 64
selected_test <- "one-plus-o1011" #"random"
# TODO non equal distr for drvr/bngai
EQUAL_X_AXS <- TRUE
if(EQUAL_X_AXS){
	predef_col_labels <- round(seq(-1, 1, 2/(x_dim-1)), digits=2)
} else {
	predef_col_labels <- rep(0, x_dim)
}
predef_speed_labels <- round(seq(0, 85, 85/(y_dim-1)), digits=2)

all_bins <- read.csv(bin_metric, check.names=FALSE, row.names=1)

# set.seed(1234)
if (!selected_test %in% rownames(all_bins)){
	ran_num <- sample(1:length(all_bins), 1)
	selected_test <- rownames(all_bins)[ran_num]
}

print(paste("Selected test", selected_test))

to_percentage <- function(bins){
	total_sum <- sum(bins)
	bins <- bins / total_sum
	return(bins)
}

if (as_percentage){
	perc_bins <- to_percentage(as.numeric(as.vector(all_bins[selected_test,])))
	all_bins[selected_test,] <- perc_bins
}
#all_bins[selected_test,] <- format(round(all_bins[selected_test,], 2), nsmall = 2)
print_bins <- as.numeric(as.vector(all_bins[selected_test,]))
# x strangely is representing rows not columns
image_bins <- matrix(print_bins, nrow=y_dim, ncol=x_dim)
print_bins <- format(round(print_bins, 2), nsmall = 2)
print_bins <- matrix(print_bins, nrow=y_dim, ncol=x_dim)

#to_percentage(as.numeric(as.vector(all_bins["one-plus-o1011",])))
horiz_vec <- rep('_', x_dim * 6)
horiz_line <- paste(horiz_vec, collapse="")
horiz_line
for (i in 1:y_dim){
	print(i)
	vec_str <- paste(as.vector(print_bins[i,]), collapse=", ")
	print(paste('|', vec_str, '|', collapse=""))
}
horiz_line

# stupid image function needs rotation
rotate <- function(x) t(apply(x, 2, rev))
rotate_1d <- function(x){
	return(matrix(x[1,], nrow=x_dim, ncol=1))
}


dev.off()
if (y_dim == 1){
	dev.new(width=x_dim/2, height=2.3)
	rotated <- rotate_1d(image_bins)
	#
	rownames(rotated) <- predef_col_labels
	#axis( 2, at=seq(0,1,length.out=ncol(rotated) ), labels= colnames(rotated), las= 2 )
	if(st_or_sp == "st"){
		fields::image.plot(rotated, xlab="steering bins", yaxt='n', xaxt='n', col=heat.colors(NUM_COL))
		#axis( 2, at=seq(0,1,length.out=ncol(rotated) ), labels= colnames(rotated), las= 2 )
		axis( 1, at=seq(0,1,length.out=nrow(rotated) ), labels= rownames(rotated), las= 2)
	} else if(st_or_sp == "sp"){
		fields::image.plot(rotated, xlab="speed bins", yaxt='n', xaxt='n', col=heat.colors(NUM_COL))
		#axis( 2, at=seq(0,1,length.out=ncol(rotated) ), labels= colnames(rotated), las= 2 )
		axis( 1, at=seq(0,1,length.out=nrow(rotated) ), labels= rownames(rotated), las= 2)
	}
} else {
	dev.new(width=x_dim, height=y_dim)
	rotated <- rotate(image_bins)
	rownames(rotated) <- predef_col_labels
	colnames(rotated) <- predef_speed_labels
	fields::image.plot(rotated, xlab="speed bins", ylab="steering bins", axes=FALSE, col=heat.colors(NUM_COL))
	axis( 2, at=seq(0,1,length.out=nrow(rotated) ), labels= rownames(rotated), las= 2)
	axis( 1, at=seq(0,1,length.out=ncol(rotated) ), labels= colnames(rotated), las= 2 )
}
title(main=paste(bin_metric))