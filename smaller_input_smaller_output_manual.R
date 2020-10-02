# looks wheter closer input tests are also closer in output

# without OBEs
setwd("C:/CS1_R-Intro/experiments-driver-ai-no-obe-wo-minlen-wo-infspeed")
setwd("C:/CS1_R-Intro/experiments-beamng-ai-no-obe-wo-minlen-wo-infspeed")

input_name = "jaccard_44alph.csv"
output_name = "steering_speed_dist_single.csv"

start_point <- "random--la1021"
compare_point_sim <- 0.8 
compare_point <- ""

inm <- read.csv(input_name, check.names=FALSE, row.names=1)
outm <- read.csv(output_name, check.names=FALSE, row.names=1)

if (!start_point %in% names(inm)){
	ran_num <- sample(1:length(inm), 1)
	start_point <- names(inm)[ran_num]
}

input_sim_df <- inm[start_point]  # without comma to retrieve 1d-dframe with names
input_sim_df <- input_sim_df[order(input_sim_df[, start_point], decreasing=TRUE),, drop=FALSE]
output_sim_df <- outm[start_point]
output_sim_df <- output_sim_df[order(output_sim_df[, start_point], decreasing=TRUE),, drop=FALSE]


get_name_of_best_match <-function(target_sim, sim_df){
	# todo binary search instead of this linear one
	i <- 1
	while(sim_df[i,] > target_sim){
		i <- i + 1
	}
	return(rownames(sim_df)[i])
}

compare_point <- get_name_of_best_match(compare_point_sim, input_sim_df)
print(paste("Start point", start_point, "comparing tests closer and farther apart than", compare_point))

compare_point_in_sim <- input_sim_df[compare_point,]
compare_point_out_sim <- output_sim_df[compare_point,]

# TODO not only greater and 
tests_more_sim_than_comp <- rownames(input_sim_df)[input_sim_df[,start_point] > compare_point_in_sim]
tests_less_sim_than_comp <- rownames(input_sim_df)[input_sim_df[,start_point] < compare_point_in_sim]
out_sims_more_in_sim <- output_sim_df[tests_more_sim_than_comp, ]
out_sims_less_in_sim <- output_sim_df[tests_less_sim_than_comp, ]

more_in_more_out <- sum(out_sims_more_in_sim > compare_point_out_sim)
more_in_less_out <- length(out_sims_more_in_sim) - more_in_more_out
less_in_more_out <- sum(out_sims_less_in_sim > compare_point_out_sim)
less_in_less_out <- length(out_sims_less_in_sim) - less_in_more_out

print(paste("Ration more in sim --> more out sim", more_in_more_out/length(out_sims_more_in_sim)))
print(paste("Ration less in sim --> less out sim", less_in_less_out/length(out_sims_less_in_sim)))