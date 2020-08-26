# library(ggplot2)   # is there something like requirements?

#setwd("C:/CS1_R-Intro/driver-ai-wo-minlen-wo-infspeed-7-steering-4-len-20200818T120651Z-001")
setwd("C:/CS1_R-Intro/experiments-beamng-ai-wo-minlen-wo-infspeed-7-steering-4-len-20200821T084856Z-001")


# get obe count for all tests and extract the ones that fail
for_each_num_obes <- read.csv("for_each_num_obes.csv" , row.names=1)
tests_that_fail <- row.names(for_each_num_obes)[for_each_num_obes$num_obes == 1]


# adjust these two
vals_of_interest <- c("0.98" = 0.0,
				"0.95" = 0.0, 
				"0.9" = 0.0,
				"0.85" = 0.0,
				"0.8" = 0.0,
				"0.6" = 0.0,
				"0.0" = 0.0)
#vals_of_interest <- c("0.9" = 0.0)
name = "jaccard.csv"
#name = "cur_sdl_5_lcstr_dist.csv"
#name = "sdl_2d_dist.csv"
similarity_matrix <- read.csv(name, check.names=FALSE, row.names=1)   # , row.names=1   # messes with row extraction

# write the mutual sampled neighborhood of all
sampled_neighborhood <- rep(0, length(vals_of_interest))
names(sampled_neighborhood) <- names(vals_of_interest)

# write the average sampled neighborhood of each
sampled_avg_nb <- rep(0, length(vals_of_interest))
names(sampled_avg_nb) <- names(vals_of_interest)

# bool to control what neighbors are taken
GREATER_THAN = TRUE

# list for boxplot
all_ratios_list <- vector(mode = "list", length = length(vals_of_interest))
names(all_ratios_list) <- names(vals_of_interest)
# vector to gather all ratio vals
all_ratios_vec <- rep(0, length(vals_of_interest)*length(tests_that_fail))
all_ratios_vec_i <- 1

for (val_str in names(vals_of_interest)){
	threshold = as.numeric(val_str)
	
	# for calculating the average of sampled neighbors and the obe ratio
	sum_obes_ratio_threshold <- 0
	sum_num_nb <- 0  

	# gather all ratios for box plots
	all_ratios = rep(NA, length(tests_that_fail))
	all_ratios_i <- 1
	
	# maybe count the unique neighbors?
	unique_nbs = rep(NA, length(similarity_matrix))
	unique_nb_i <- 1
	for (test_name in tests_that_fail){
		all_neighbors <- similarity_matrix[test_name]
		#print(paste("For test", test_name))
		#print(head(all_neighbors))
		if (GREATER_THAN){
			#print("using >")
			close_neighbors <- row.names(all_neighbors)[all_neighbors[,test_name] >= threshold]
		} else { 
			#print("using <")
			close_neighbors <- row.names(all_neighbors)[all_neighbors[,test_name] <= threshold]
		}	
		#print(paste("Threshold", threshold, "Number of close neighbors", length(close_neighbors)))

		# look how many neighbors have OBEs
		num_obes <- 0
		for (nb in close_neighbors){
			if (nb %in% tests_that_fail){
				num_obes <- num_obes + 1
			}
			if (!(nb %in% unique_nbs)){
				unique_nbs[i] <- nb
				i <- i + 1
			}
		}
		
		sum_num_nb <- sum_num_nb + length(close_neighbors)
		obes_ratio <- num_obes / length(close_neighbors)
		#print(paste("num closenb for", threshold, ":", length(close_neighbors))) # makes sense, each obe test gets added anyway
		sum_obes_ratio_threshold <- sum_obes_ratio_threshold + obes_ratio

		# add own ratio to list
		all_ratios[all_ratios_i] <- obes_ratio
		all_ratios_i <- all_ratios_i + 1
		# fill complete vector
		all_ratios_vec[all_ratios_vec_i] <- obes_ratio
		all_ratios_vec_i <- all_ratios_vec_i + 1
	}
	avg_obes_ratio_threshold <- sum_obes_ratio_threshold / length(tests_that_fail)
	avg_num_nb <- sum_num_nb / length(tests_that_fail)
	# calculate the number of unique neighbors
	num_unique_nbs = sum(!is.na(unique_nbs))
	print(paste("Avg OBEs ratio for", threshold, ":", avg_obes_ratio_threshold))
	print(paste("Avg neighbors:", avg_num_nb, "Unique nbs:", num_unique_nbs))
	
	#add average to thresholds array
	vals_of_interest[val_str] <- avg_obes_ratio_threshold
	#add average number of neighbors per road and normalize to fit into the chart
	sampled_avg_nb[val_str] <- avg_num_nb/length(similarity_matrix)
	#normalize unique sampled neighborhood to fit into chart
	sampled_neighborhood[val_str] <- num_unique_nbs/length(similarity_matrix)

	# add all ratios for the current threshold
	all_ratios_list[[val_str]] <- all_ratios
	print(paste("len allratios at ", val_str, "len all_ratios", length(all_ratios)))
}


# Lineplot
# Make a lineplot of 
t_holds <- as.numeric(names(vals_of_interest))
dframe_lnplt <- data.frame(
	Threshold = t_holds,
	OBE_Ratios = vals_of_interest,
	Sampled_NB = sampled_neighborhood,
	Avg_NB = sampled_avg_nb
)
ggplot(dframe_lnplt, aes(x=Threshold)) + 
		geom_line(aes(y=OBE_Ratios, colour="Avg OBE ratio"), size=2) + 
		geom_line(aes(y=Avg_NB, colour="Avg sampled area")) +
		scale_color_manual("", breaks = c("Avg OBE ratio", "Avg sampled area"), 
						values = c("red", "blue")) +
		ggtitle("OBE ratio to jaccard similarity") 
		

#Boxplot
# the dataframe must have many values, create a dataframe that can do that
# this might be more wise to do above
t_holds_repeated <- rep(0, length(all_ratios)*length(vals_of_interest))
for (i in 1:length(vals_of_interest)){
	offset <- (i-1)*length(all_ratios)
	for (j in 1:length(all_ratios)){
		t_holds_repeated[offset+j] <- as.numeric(names(vals_of_interest)[i])
	}
}
sampled_nb_repeated <- rep(0, length(all_ratios)*length(vals_of_interest))
for (i in 1:length(vals_of_interest)){
	offset <- (i-1)*length(all_ratios)
	for (j in 1:length(all_ratios)){
		sampled_nb_repeated[offset+j] <- sampled_avg_nb[i]
	}
}

dframe_bxplt <- data.frame(
	Threshold = t_holds_repeated,
	OBE_Ratios = all_ratios_vec,
	Avg_NB = sampled_nb_repeated
	)
dframe_bxplt$Threshold <- as.factor(dframe_bxplt$Threshold) 
ggplot(dframe_bxplt, aes(x=reorder(Threshold, desc(Threshold)), y=OBE_Ratios)) +
		geom_boxplot() + 
		geom_line(aes(y=Avg_NB, group=1), size=2)
		# scale_x_discrete(limits=rev(levels(Threshold)))
