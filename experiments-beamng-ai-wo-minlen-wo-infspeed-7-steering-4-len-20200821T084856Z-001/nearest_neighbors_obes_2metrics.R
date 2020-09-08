#############################################################
#############################################################
############This code is deprecated, do not use##############
#############################################################
#############################################################


library(ggplot2)  
library(dplyr)  

#setwd("C:/CS1_R-Intro/driver-ai-wo-minlen-wo-infspeed-7-steering-4-len-20200818T120651Z-001")
setwd("C:/CS1_R-Intro/experiments-beamng-ai-wo-minlen-wo-infspeed-7-steering-4-len-20200821T084856Z-001")
#setwd("C:/CS1_R-Intro/drvr-jacc-5050")

# get obe count for all tests and extract the ones that fail
for_each_num_obes <- read.csv("for_each_num_obes.csv" , row.names=1)
tests_that_fail <- row.names(for_each_num_obes)[for_each_num_obes$num_obes == 1]

# do not get filled, remaining from previous single metric plotting
vals_of_interest <- c("0.98" = 0.0,
				"0.95" = 0.0, 
				"0.9" = 0.0,
				"0.85" = 0.0,
				"0.8" = 0.0,
				"0.7" = 0.0,
				"0.6" = 0.0,
				"0.5" = 0.0,
				"0.0" = 0.0)
# uncomment for jaccard
#metric1 = "jaccard.csv"
#metric2 = "jaccard_11ang.csv"
# uncomment for jaccard 11 vs 15
#metric1 = "jaccard_11ang.csv"
#metric2 = "jaccard_15ang.csv"
# uncommment for jaccard different length weight
#metric1 = "jaccard_60alph.csv"
#metric2 = "jaccard_60alph_5050.csv"
# uncomment for sliding window 1d vs 2d
metric1 = "curve_sdl_dist_11ang.csv"
metric2 = "sdl_2d_dist_44alph.csv"
# uncomment for sliding window 1d alphabet size
#metric1 = "curve_sdl_dist.csv"
#metric2 = "curve_sdl_dist_11ang.csv"
# uncomment for sliding window 2d alphabet size
#metric1 = "sdl_2d_dist.csv"
#metric2 = "sdl_2d_dist_5050.csv"
# uncomment for sliding window 1d vs 2d 11ang
#metric1 = "curve_sdl_dist_11ang.csv"
#metric2 = "sdl_2d_dist_11ang.csv"
# uncomment for center dist distances
#metric1 = "center_dist_binary.csv"
#metric2 = "center_dist_single.csv"
# uncommment for steering only distances
#metric1 = "steering_dist_binary.csv"
#metric2 = "steering_dist_single.csv"


# bool to control what neighbors are taken
GREATER_THAN = TRUE


len_observations = 2*length(vals_of_interest)*length(tests_that_fail)
# all thresholds repeated
t_holds_repeated <- rep(0, len_observations)

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
			num_obes <- 0
			for (nb in close_neighbors){
				if (nb %in% tests_that_fail){
					num_obes <- num_obes + 1
				}
			}
			sum_num_nb <- sum_num_nb + length(close_neighbors)

			obes_ratio <- num_obes / length(close_neighbors)


			# fill the all ratios vector
			all_ratios_vec[start_index] <<- obes_ratio

			# fill in which metric has been used
			metric_used[start_index] <<- metric_name

			# fill in the threshold
			t_holds_repeated[start_index] <<- threshold
			start_index <- start_index + 1
		}
		
		# calculate the average number of neighbors for current threshold
		avg_num_nb <- sum_num_nb / length(tests_that_fail)
		avg_num_nb_normalized <- avg_num_nb/length(similarity_matrix)
		print(paste("Threshold", threshold, "avg neighbors:", avg_num_nb))

		# repeat the number of neighbors value for the dataframe
		for (i in last_index:start_index-1){
			sampled_avg_nb[i] <<- avg_num_nb_normalized
		}
	}
}

fill_ratio_nb(1, metric1)
new_index <- len_observations/2 + 1
fill_ratio_nb(new_index, metric2)

dframe_bxplt <- data.frame(
	Threshold = t_holds_repeated,
	OBE_Ratios = all_ratios_vec,
	Avg_NB = sampled_avg_nb,
	Metric = metric_used
)

# set colors for the plot
cols <- c("brown3", "cadetblue3")
names(cols) <- c(metric1, metric2)

pick <- function(condition){
  function(d) d %>% filter_(condition)
}

dframe_bxplt$Threshold <- as.factor(dframe_bxplt$Threshold) 
ggplot(dframe_bxplt, aes(x=Threshold, y=OBE_Ratios)) +
	geom_boxplot(aes(fill=Metric)) + 
	geom_line(data=pick(~Metric== metric1), aes(y=Avg_NB, group=1, color=cols[metric1]), size=2, show.legend=FALSE) + 
	geom_line(data=pick(~Metric== metric2), aes(y=Avg_NB, group=1, color=cols[metric2]), size=2, show.legend=FALSE) +
	scale_x_discrete(limits=rev(levels(as.factor(dframe_bxplt$Threshold)))) + 
	scale_fill_manual(values = cols)

for (i in 1:5){
	print("Do not use, this code is deprecated!")
}

