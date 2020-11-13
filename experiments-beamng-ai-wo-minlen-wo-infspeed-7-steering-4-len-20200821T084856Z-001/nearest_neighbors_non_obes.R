library(ggplot2)   # is there something like requirements?
library(egg)

setwd("C:/CS1_R-Intro/driver-ai-wo-minlen-wo-infspeed-7-steering-4-len-20200818T120651Z-001")
#setwd("C:/CS1_R-Intro/experiments-beamng-ai-wo-minlen-wo-infspeed-7-steering-4-len-20200821T084856Z-001")


# get obe count for all tests and extract the ones that fail
for_each_num_obes <- read.csv("for_each_num_obes.csv" , row.names=1)
tests_that_fail <- row.names(for_each_num_obes)[for_each_num_obes$num_obes == 1]
tests_that_dont_fail <- row.names(for_each_num_obes)[for_each_num_obes$num_obes == 0]


BOXPLOT_INSTEAD_OF_LINEPLOT = TRUE

# adjust these two
vals_of_interest <- c("0.95" = 0.0,
                      "0.9" = 0.0,
        "0.8" = 0.0,
				"0.7" = 0.0,
				"0.0" = 0.0)
#vals_of_interest <- c("0.9" = 0.0)
#name = "jaccard_11ang_4len.csv"
#name = "cur_sdl_lcs_dist.csv"
name = "sdl2d_sw_11ang_4len.csv"
#name = "cursdl_sw_11ang.csv"
#name = "curve_sdl_dist_11ang.csv"
#name = "speering_speed_dist.csv"


# define width and height of plots for a more consistent presentation
# non OBE
wid <- 725
hei <- 530

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
	for (test_name in tests_that_dont_fail){
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
		i <- 0
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
	avg_obes_ratio_threshold <- sum_obes_ratio_threshold / length(tests_that_dont_fail)
	avg_num_nb <- sum_num_nb / length(tests_that_dont_fail)
	# calculate the number of unique neighbors
	num_unique_nbs = sum(!is.na(unique_nbs))
	print(paste("Avg OBEs ratio for", threshold, ":", avg_obes_ratio_threshold))
	print(paste("Avg neighbors:", avg_num_nb, "Unique nbs:", num_unique_nbs))
	cat("\n")
	
	#add average to thresholds array
	vals_of_interest[val_str] <- avg_obes_ratio_threshold
	#add average number of neighbors per road and normalize to fit into the chart
	sampled_avg_nb[val_str] <- avg_num_nb/length(similarity_matrix)
	#normalize unique sampled neighborhood to fit into chart
	sampled_neighborhood[val_str] <- num_unique_nbs/length(similarity_matrix)

	# add all ratios for the current threshold
	all_ratios_list[[val_str]] <- all_ratios
	#print(paste("len allratios at ", val_str, "len all_ratios", length(all_ratios)))
}


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
bx_plots <- ggplot(dframe_bxplt, aes(x=Threshold, y=OBE_Ratios)) +
	geom_boxplot(fill="#F8766D") + 
	scale_x_discrete(limits=rev(levels(as.factor(dframe_bxplt$Threshold))))

ln_plots <- ggplot(dframe_bxplt, aes(x=Threshold)) + 
		geom_line(aes(y=Avg_NB, group=1), color="#F8766D", size=2) + 
		scale_x_discrete(limits=rev(levels(as.factor(dframe_bxplt$Threshold))))


# changing the output size for the plot
# somehow this increases the resolution significantly, the ratio however is similar enough
dev.new(width = wid, height = hei, unit="px", noRStudioGD=TRUE)
# change to font size of the ggplots
font_size <- 26
ln_plots <- ln_plots + theme(text = element_text(size=font_size))
bx_plots <- bx_plots + theme(text = element_text(size=font_size))
egg::ggarrange(bx_plots, ln_plots)
