# scatterplot of input and output similarities for one test

setwd("C:/CS1_R-Intro/experiments-beamng-ai-wo-minlen-wo-infspeed-7-steering-4-len-20200821T084856Z-001")

input_name <- "jaccard_60alph.csv"
output_name <- "steering_speed_dist.csv"

centerpoint <- "no"

inm <- read.csv(input_name, check.names=FALSE, row.names=1)
outm <- read.csv(output_name, check.names=FALSE, row.names=1)