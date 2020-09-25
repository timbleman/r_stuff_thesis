# print bins absolute or as percentage for a test

setwd("C:/CS1_R-Intro/driver-ai-wo-minlen-wo-infspeed-7-steering-4-len-20200818T120651Z-001")

# these have to be adjusted by the user
bin_metric <- "speed_steering_2d_bins.csv"
#bin_metric <- "steering_bins.csv"
#bin_metric <- "speed_bins.csv"
as_percentage <- FALSE
# These have to be correctly entered!!!
x_dim = 16
y_dim = 16
selected_test <- "random"#"one-plus-o1011" #"random"

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

if (y_dim == 1){
	image(image_bins, ylab=paste(bin_metric), axes=FALSE)
} else {
	image(image_bins, ylab=paste(bin_metric), axes=FALSE)
}
