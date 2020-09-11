library(ggplot2)  
library(egg)

# with OBEs
#setwd("C:/CS1_R-Intro/driver-ai-wo-minlen-wo-infspeed-7-steering-4-len-20200818T120651Z-001")
#setwd("C:/CS1_R-Intro/experiments-beamng-ai-wo-minlen-wo-infspeed-7-steering-4-len-20200821T084856Z-001")

# without OBEs
setwd("C:/CS1_R-Intro/experiments-driver-ai-no-obe-wo-minlen-wo-infspeed")
#setwd("C:/CS1_R-Intro/experiments-beamng-ai-no-obe-wo-minlen-wo-infspeed")

LENGTH_INSTEAD_OF_UNION_SHARED = TRUE

# do not get filled, remaining from previous single metric plotting
vals_of_interest <- c("0.95" = 0.0, 
				"0.9" = 0.0,
				"0.85" = 0.0,
				"0.8" = 0.0,
				"0.7" = 0.0,
				"0.5" = 0.0,
				"0.0" = 0.0)
# uncomment for jaccard
#metric_in = "jaccard_28alph.csv"
#metric_in = "jaccard_44alph.csv"
metric_in = "jaccard_60alph.csv"
#metric_in = "sdl_2d_dist_44alph.csv"
#metric_in = "sdl_2d_dist_60alph.csv"
#metric_out = "steering_speed_dist.csv"
#metric_out = "steering_dist_binary.csv"
#metric_out = "steering_speed_dist_single.csv"
metric_out = "steering_speed_dist.csv"
#metric_out = "steering_speed_dist_single.csv"

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

time.A	<- Sys.time()   # benchmark implementation

get_shared_len <- function(arr1, arr2){
	shared_num <- 0
	shared_num <- length(arr1)
	for (el in arr2){
		if (!el %in% arr1){
			shared_num <- shared_num + 1
		}
	}
	return(shared_num)
}


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
			all_neighbors_out <- out_similarity_matrix[test_name]	
			all_nbs_names = row.names(all_neighbors_out)
			# find all closest ones
			all_nbs_out_sorted <- rownames(all_neighbors_out)[order(all_neighbors_out[test_name], decreasing=TRUE)]
			best_selected <- all_nbs_out_sorted[1:length_of_neighborhood]


			# check how big the union is
			shared_size <- 0
			for (name in close_neighbors){
				if (name %in% best_selected){
					shared_size <- shared_size + 1
				}
			}

			if (LENGTH_INSTEAD_OF_UNION_SHARED){
				divid <- length(close_neighbors)
			} else {
				divid <- get_shared_len(close_neighbors, best_selected)
			}

			# TODO maybe normalize by sample size or something?
			shared_ratio <- shared_size/divid
			# add to all ratios vector
			all_ratios_vec[start_index] <<- shared_ratio

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

bx_plots <- ggplot(dframe_bxplt, aes(x=Threshold, y=Union_Ratios)) +
	geom_boxplot(fill="#F8766D") + 
	scale_x_discrete(limits=rev(levels(as.factor(dframe_bxplt$Threshold))))

# does also not work
#sampled_avg_nb_non_rep <- c(unique(sampled_avg_nb), sampled_avg_nb[length(sampled_avg_nb)])
#names(sampled_avg_nb_non_rep) <- c(names(vals_of_interest), "0.0")
# this is absolutely horrible, but it workes for now
sampled_avg_nb_non_rep <- unique(sampled_avg_nb)
names(sampled_avg_nb_non_rep) <- names(vals_of_interest)
names(sampled_avg_nb_non_rep)[length(sampled_avg_nb_non_rep)] = "0"
dframe_lnplt <- data.frame(
	Threshold = names(sampled_avg_nb_non_rep),
	Avg_NB = sampled_avg_nb_non_rep
)
dframe_lnplt$Threshold <- as.factor(dframe_lnplt$Threshold)

ln_plots <- ggplot(dframe_lnplt, aes(x=Threshold)) +
	geom_path(aes(y=Avg_NB, group=1), color="#F8766D", size=2) +
	scale_x_discrete(limits=rev(levels(as.factor(dframe_bxplt$Threshold))))

egg::ggarrange(bx_plots, ln_plots)

# old plotting
#ggplot(dframe_bxplt, aes(x=Threshold, y=Union_Ratios)) +
#	geom_boxplot() + 
#	geom_line(aes(y=Avg_NB, group=1), size=2) + 
#	scale_x_discrete(limits=rev(levels(as.factor(dframe_bxplt$Threshold))))


time.B	<- Sys.time()
difftime(time.B, time.A, unit = "secs")