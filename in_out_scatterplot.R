# scatterplot of input and output similarities for one test

# without OBEs
setwd("C:/CS1_R-Intro/experiments-driver-ai-no-obe-wo-minlen-wo-infspeed")
#setwd("C:/CS1_R-Intro/experiments-beamng-ai-no-obe-wo-minlen-wo-infspeed")

input_name <- "jaccard_44alph.csv"
output_name <- "steering_speed_dist_binary.csv"

# select a centerpoint, if it is not in the matrix, a random one is chose
centerpoint <- "random--la1021"

inm <- read.csv(input_name, check.names=FALSE, row.names=1)
outm <- read.csv(output_name, check.names=FALSE, row.names=1)

stopifnot(names(inm) == names(outm))

# set.seed(1234)
if (!centerpoint %in% names(inm)){
	ran_num <- sample(1:length(inm), 1)
	centerpoint <- names(inm)[ran_num]
}

print(paste("Selected", centerpoint, "as centerpoint."))

input_sim <- inm[,centerpoint]  # without comma to retrieve 1d-dframe with names
output_sim <- outm[,centerpoint]
plot(input_sim, output_sim, xlim=c(1,0), ylim=c(1,0))
