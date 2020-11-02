# print bins absolute or as percentage for a test
# should be run in an environment, that supports window resizing (not RStudio)
# too much of this is hardcoded and requires adjustments for small little changes
# reimplementing using ggplot may have been much easier
library(fields)

# select a set
setwd("C:/CS1_R-Intro/driver-ai-wo-minlen-wo-infspeed-7-steering-4-len-20200818T120651Z-001")
setwd("C:/CS1_R-Intro/experiments-beamng-ai-wo-minlen-wo-infspeed-7-steering-4-len-20200821T084856Z-001")

# obe or two dimensions
dimensions <- 1
# steering (st) or speed (sp)
st_or_sp <- "sp"
# select a test to plot bins, if not found a random one is taken
selected_test <- "random--la711" #"one-plus-o1011" #"random"
# Equal borders for the steering bins
EQUAL_X_AXS <- FALSE
if(dimensions == 2){ 
	# these have to be adjusted by the user
	if (EQUAL_X_AXS){
	  bin_metric <- "speed_steering_2d_bins.csv"
	} else {
	  bin_metric <- "speed_steering_2d_bins_adjusted.csv"
	}
}
if(dimensions == 1){ 
	if(st_or_sp == "st") {
		if (EQUAL_X_AXS){
			bin_metric <- "steering_bins.csv"
		} else {
			bin_metric <- "steering_bins_non_uniform_percentile.csv"
		}
	}
	if(st_or_sp == "sp") {
		bin_metric <- "speed_bins.csv"
	}
}
# whether percentage will be plotted instead of absolute values
as_percentage <- FALSE
# These have to be correctly entered!!!
x_dim = 16
if(dimensions == 1){ 
	y_dim = 1
} else {
	y_dim = 16
}
NUM_COL <- 64
# borders for fillin bins 
# FIXME the usage of x_dim will be problematic for non-square bins
# calculate labels for equal width
predef_speed_labels <- round(seq(0, 85, 85/(x_dim)), digits=1)
predef_steer_labels <- round(seq(-1, 1, 2/(x_dim)), digits=2)
# percentile based borders
adjusted_steer_labels <- c(-1.0, -0.09, -0.06, -0.05, -0.04, -0.03, -0.03, -0.02, 
					-0.00, 0.02, 0.03, 0.03, 0.04, 0.05, 0.06, 0.08, 1.0)
if (st_or_sp == "sp") {
	predef_col_labels <- predef_speed_labels
} else if (st_or_sp == "st"){
	if(EQUAL_X_AXS){
		predef_col_labels <- predef_steer_labels
	} else {
		predef_col_labels <- adjusted_steer_labels
	}
}

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


# font size multiplier, requires tweaking of borders
font_mult <- 2
dev.off()
if (y_dim == 1){
	dev.new(width=x_dim/2, height=2.65)
	rotated <- rotate_1d(image_bins)
	par(mar=c(7,4,4,1)+.1)  # margins bottom, left, top, right

	# TODO maybe use the upper, not lower border
	rownames(rotated) <- predef_col_labels[1:length(predef_col_labels)-1]
	#axis( 2, at=seq(0,1,length.out=ncol(rotated) ), labels= colnames(rotated), las= 2 )
	if(st_or_sp == "st"){
		fields::image.plot(rotated, xlab="steering bins", yaxt='n', xaxt='n', 
					 col=heat.colors(NUM_COL, rev=TRUE), cex.lab = font_mult, 
					 legend.cex = font_mult, axis.args=list(cex.axis=font_mult),
					 mgp=c(5.5,1,1))  # position of axis labels
		axis( 1, at=seq(0,1,length.out=nrow(rotated) ), labels= rownames(rotated), 
			las= 2, cex.axis = font_mult)
	} else if(st_or_sp == "sp"){
		fields::image.plot(rotated, xlab="speed bins", yaxt='n', xaxt='n', 
					 col=heat.colors(NUM_COL, rev=TRUE), cex.lab = font_mult, 
					 legend.cex = font_mult, axis.args=list(cex.axis=font_mult),
					 mgp=c(5.5,1,1)) # position of axis labels
		axis( 1, at=seq(0,1,length.out=nrow(rotated) ), labels= rownames(rotated), 
			las= 2, cex.axis = font_mult)
	}
} else {
	dev.new(width=x_dim+0.5, height=y_dim)
	rotated <- rotate(image_bins)
	# TODO maybe use the upper, not lower border
	rownames(rotated) <- predef_col_labels[1:length(predef_col_labels)-1]
	colnames(rotated) <- predef_speed_labels[1:length(predef_speed_labels)-1]

	par(mar=c(7,7,4,1)+.1)  # margins bottom, left, top, right
	# cex increases fonts, legend.cex is not working
	fields::image.plot(rotated, xlab="steering bins", ylab="speed bins", axes=FALSE, 
				 col=heat.colors(NUM_COL, rev=TRUE),
				 cex.lab = font_mult, legend.cex = font_mult, 
				 mgp=c(5.5,1,1), # position of axis labels
				 axis.args=list(cex.axis=font_mult))
	axis( 1, at=seq(0,1,length.out=nrow(rotated) ), labels= rownames(rotated), 
		las= 2, cex.axis = font_mult)
	axis( 2, at=seq(0,1,length.out=ncol(rotated) ), labels= colnames(rotated), 
		las= 2, cex.axis = font_mult)
}
title(main=paste(bin_metric), cex.main=font_mult)