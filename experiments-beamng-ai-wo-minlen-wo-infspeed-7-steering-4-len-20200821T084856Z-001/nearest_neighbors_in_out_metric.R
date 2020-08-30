library(ggplot2)  

#setwd("C:/CS1_R-Intro/driver-ai-wo-minlen-wo-infspeed-7-steering-4-len-20200818T120651Z-001")
setwd("C:/CS1_R-Intro/experiments-beamng-ai-wo-minlen-wo-infspeed-7-steering-4-len-20200821T084856Z-001")

# do not get filled, remaining from previous single metric plotting
vals_of_interest <- c("0.95" = 0.0, 
				"0.9" = 0.0,
				"0.85" = 0.0,
				"0.8" = 0.0,
				"0.7" = 0.0,
				"0.5" = 0.0,
				"0.0" = 0.0)
# uncomment for jaccard
#metric_in = "jaccard.csv"
#metric_in = "jaccard_11ang.csv"
#metric_in = "jaccard_15ang.csv"
metric_in = "sdl_2d_dist_11ang.csv"
metric_out = "steering_speed_dist.csv"
#metric_out = "steering_dist_binary.csv"


# bool to control what neighbors are taken
GREATER_THAN = TRUE


# this is stupid, the matrix is needed just for its length
similarity_matrix <- read.csv(metric_in, check.names=FALSE, row.names=1)
len_observations = length(vals_of_interest)*length(similarity_matrix)
# all thresholds repeated
t_holds_repeated <- rep(0, len_observations)

# sample neighborhood for both distances
sampled_avg_nb <- rep(0, len_observations)

# vector to gather all ratio vals, for both
all_ratios_vec <- rep(0, len_observations)

# metric used, for the dataframe
metric_used <- rep(NA, len_observations)


fill_ratio_nb_io <- function(start_index, metric_in_name, metric_out_name){
	in_similarity_matrix <- read.csv(metric_in_name, check.names=FALSE, row.names=1)
	out_similarity_matrix <- read.csv(metric_out_name, check.names=FALSE, row.names=1)

	all_tests <- names(in_similarity_matrix)
	for (val_str in names(vals_of_interest)){
		threshold = as.numeric(val_str)
		sum_num_nb <- 0

		# needed for repeating the avg neighborhood
		last_index = start_index
		for (test_name in all_tests){
			all_neighbors_in <- in_similarity_matrix[test_name]
			if (GREATER_THAN){
				#print("using >")
				close_neighbors <- row.names(all_neighbors_in)[all_neighbors_in[,test_name] >= threshold]
			} else { 
				#print("using <")
				close_neighbors <- row.names(all_neighbors_in)[all_neighbors_in[,test_name] <= threshold]
			}
			length_of_neighborhood <- length(close_neighbors)
			sum_num_nb <- sum_num_nb + length(close_neighbors)

			# extract all the closest neighbors from the outlist
			best_selected = rep(NA, length_of_neighborhood)	
			i_best_sel <- 1
			all_neighbors_out <- out_similarity_matrix[test_name]	
			all_nbs_names = row.names(all_neighbors_out)
			#print(head(all_neighbors_out))
			# find all closest ones
			for (i in 1:length_of_neighborhood){
				current_max = -1   # TODO change
				current_best = ""
				# select the best test that is not yet in the list
				for (nb_out in all_nbs_names){
					# is this right? rowname then colname=
					sim  = all_neighbors_out[nb_out, test_name]
					#print(typeof(current_max)  )
					# jo, this might backfire, only using >
					if (sim > current_max){
						if (!nb_out %in% best_selected){
							current_max = sim
							current_best = nb_out
						}
					}
				}
				# check if there has been one added, remove if working
				if (current_best == ""){print("Jo this does not work")}
				best_selected[i_best_sel] = current_best
				i_best_sel <- i_best_sel + 1
			}

			# check how big the union is
			union_size <- 0
			for (name in close_neighbors){
				if (name %in% best_selected){
					union_size <- union_size + 1
				}
			}

			# TODO maybe normalize by sample size or something?
			union_ratio <- union_size/length(close_neighbors)
			# add to all ratios vector
			all_ratios_vec[start_index] <<- union_ratio

			# fill in the threshold
			t_holds_repeated[start_index] <<- threshold
			start_index <- start_index + 1
		}
		# calc avg_num_nb
		avg_num_nb <- sum_num_nb / length(all_tests)
		avg_num_nb_normalized <- avg_num_nb/length(in_similarity_matrix)

		print(paste("Threshold", threshold, "avg neighbors:", avg_num_nb))

		# repeat the number of neighbors value for the dataframe
		for (i in last_index:start_index-1){
			sampled_avg_nb[i] <<- avg_num_nb_normalized
		}
	}
}

fill_ratio_nb_io(1, metric_in, metric_out)

dframe_bxplt <- data.frame(
	Threshold = t_holds_repeated,
	Union_Ratios = all_ratios_vec,
	Avg_NB = sampled_avg_nb
	)

dframe_bxplt$Threshold <- as.factor(dframe_bxplt$Threshold) 
ggplot(dframe_bxplt, aes(x=Threshold, y=Union_Ratios)) +
	geom_boxplot() + 
	geom_line(aes(y=Avg_NB, group=1), size=2) + 
	scale_x_discrete(limits=rev(levels(as.factor(dframe_bxplt$Threshold))))
