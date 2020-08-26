library(ggplot2)  

setwd("C:/CS1_R-Intro/driver-ai-wo-minlen-wo-infspeed-7-steering-4-len-20200818T120651Z-001")
#setwd("C:/CS1_R-Intro/experiments-beamng-ai-wo-minlen-wo-infspeed-7-steering-4-len-20200821T084856Z-001")

# get obe count for all tests and extract the ones that fail
for_each_num_obes <- read.csv("for_each_num_obes.csv" , row.names=1)
tests_that_fail <- row.names(for_each_num_obes)[for_each_num_obes$num_obes == 1]

# do not get filled, remaining from previous single metric plotting
vals_of_interest <- c("0.98" = 0.0,
				"0.95" = 0.0, 
				"0.9" = 0.0,
				"0.85" = 0.0,
				"0.8" = 0.0,
				"0.6" = 0.0,
				"0.0" = 0.0)

metric1 = "jaccard.csv"
metric2 = "sdl_2d_dist.csv"

# bool to control what neighbors are taken
GREATER_THAN = TRUE

len_observations = 2*length(vals_of_interest)*length(tests_that_fail)
# sample neighborhood for both distances
sampled_avg_nb <- rep(0, len_observations)

# vector to gather all ratio vals, for both
all_ratios_vec <- rep(0, len_observations)

# metric used, for the dataframe
metric_used <- rep(NA, len_observations)

fill_ratio_nb <- function(start_index, metric_name){
	similarity_matrix <- read.csv(metric_name, check.names=FALSE, row.names=1)

	for (val_str in names(vals_of_interest)){
		threshold = as.numeric(val_str)
		sum_num_nb <- 0

		# needed for repeating the avg neighborhood
		last_index = start_index
		for (test_name in tests_that_fail){
			all_neighbors <- similarity_matrix[test_name]
			if (GREATER_THAN){
				#print("using >")
				close_neighbors <- row.names(all_neighbors)[all_neighbors[,test_name] >= threshold]
			} else { 
				#print("using <")
				close_neighbors <- row.names(all_neighbors)[all_neighbors[,test_name] <= threshold]
			}

			# look how many neighbors have OBEs
			for (nb in close_neighbors){
				if (nb %in% tests_that_fail){
					num_obes <- num_obes + 1
				}
			}
			sum_num_nb <- sum_num_nb + length(close_neighbors)

			obes_ratio <- num_obes / length(close_neighbors)

			# fill the all ratios vector
			all_ratios_vec[start_index] <<- obes_ratio
			start_index <- start_index + 1

			# fill in which metric has been used
			metric_used <<- metric_name
		}
		
		# calculate the average number of neighbors for current threshold
		avg_num_nb <- sum_num_nb / length(tests_that_fail)
		print(paste("Threshold", threshold, "avg neighbors:", avg_num_nb))

		# repeat the number of neighbors value for the dataframe
		for (i in last_index:start_index-1){
			sampled_avg_nb[i] <<- avg_num_nb
		}
	}
}

fill_ratio_nb(1, metric1)
new_index 

