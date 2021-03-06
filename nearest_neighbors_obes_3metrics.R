# Used for all OBE ratio plots in RQ1 (OBE tests as start points)
# Things to adjust:
# Select a set (BeamNG.AI or DriverAI path)
# Select the number of metrics in a plot
# Add more thresholds to look at in vals_of_interest


(ggplot2)
library(dplyr)  # for the filter function
library(egg)  # for having plots stacked


# select a set
setwd("C:/CS1_R-Intro/driver-ai-wo-minlen-wo-infspeed-7-steering-4-len-20200818T120651Z-001")
setwd("C:/CS1_R-Intro/experiments-beamng-ai-wo-minlen-wo-infspeed-7-steering-4-len-20200821T084856Z-001")

# Number of metrics in a plot, from 1 to 3
NUMBER_OF_METRICS = 3

# Add more thresholds
# do not get filled, remaining from previous single metric plotting
vals_of_interest <- c("0.98" = 0.0,
				"0.9" = 0.0,
				"0.8" = 0.0,
				"0.7" = 0.0,
				"0.0" = 0.0)
# uncomment for jaccard, choose between old and new alphabet name
#metric1 = "jaccard_7ang_4len.csv"
#metric1 = "jaccard_11ang_8len.csv"
#metric2 = "jaccard_11ang_4len.csv"
#metric3 = "jaccard_15ang_4len.csv"
#metric1 = "jaccard_7ang_4len.csv"
metric1 = "jaccard_28alph.csv"
#metric1 = "jaccard_44alph.csv"
#metric2 = "jaccard_88alph.csv"
metric2 = "jaccard_44alph.csv"
metric3 = "jaccard_60alph.csv"


# uncomment for sliding window 1d alphabet size
#metric1 = "cursdl_sw_7ang.csv"
#metric2 = "cursdl_sw_11ang.csv"
#metric3 = "cursdl_sw_15ang.csv"
# uncomment for sliding window 2d alphabet size
#metric1 = "sdl2d_sw_7ang_4len.csv"
#metric1 = "sdl2d_sw_44alph.csv"
#metric2 = "sdl2d_sw_60alph.csv"
#metric3 = "sdl2d_sw_120alph.csv"
# uncomment for lcs
#metric1 = "cursdl_lcs_7ang.csv"   
# uncomment for lcstr 0, 1 and 5 mismatches
#metric1 = "cursdl_1_lcstr_7ang.csv"
#metric2 = "cursdl_5_lcstr_7ang.csv"
#metric3 = "cursdl_lcstr_7ang.csv"   # 
#metric1 = "cursdl_lcs_7ang.csv"
# uncomment for steering speed 2d output similarity
#metric1 = "steering_speed_dist_binary.csv"
#metric2 = "steering_speed_dist_single.csv"
# uncomment for steering dist
#metric1 = "steering_dist_binary.csv"
#metric2 = "steering_dist_single.csv"



# define width and height of plots for a more consistent presentation
# OBE
wid <- 830
hei <- 564

# get obe count for all tests and extract the ones that fail
for_each_num_obes <- read.csv("for_each_num_obes.csv" , row.names=1)
tests_that_fail <- row.names(for_each_num_obes)[for_each_num_obes$num_obes == 1]

# deprecated, leave as is
STEPLINE_INSTEAD_OF_LINEPLOT = FALSE

# whether to leave '.csv' in the plots
STRIP_FILE_ENDING <- TRUE

# bool to control what neighbors are taken
GREATER_THAN = TRUE


len_observations = NUMBER_OF_METRICS*length(vals_of_interest)*length(tests_that_fail)
# all thresholds repeated
t_holds_repeated <- rep(0, len_observations)

# sample neighborhood for both distances
sampled_avg_nb <- rep(0, len_observations)

# vector to gather all ratio vals, for both
all_ratios_vec <- rep(0, len_observations)

# metric used, for the dataframe
metric_used <- rep(NA, len_observations)

fill_ratio_nb <- function(start_index, metric_name){
	# for the interpolated lineplot of the avg neighborhood
	nb_vec <- rep(0, length(vals_of_interest))
	names(nb_vec) <- names(vals_of_interest)
	
	# load the corresponing matrix
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

		# put in the not repeated
		nb_vec[val_str] <- avg_num_nb_normalized
		# repeat the number of neighbors value for the dataframe
		for (i in last_index:start_index-1){
			sampled_avg_nb[i] <<- avg_num_nb_normalized
		}
	}
	return(nb_vec)
}

print(metric1)
nb_vec1 <- fill_ratio_nb(1, metric1)
if (NUMBER_OF_METRICS >= 2){
	print(metric2)
	new_index <- len_observations/NUMBER_OF_METRICS + 1
	nb_vec2 <- fill_ratio_nb(new_index, metric2)
}
if (NUMBER_OF_METRICS >= 3){
	print(metric3)
	new_index <- 2*(len_observations/NUMBER_OF_METRICS) + 1
	nb_vec3 <- fill_ratio_nb(new_index, metric3)
}

dframe_bxplt <- data.frame(
	Threshold = t_holds_repeated,
	OBE_Ratios = all_ratios_vec,
	Avg_NB = sampled_avg_nb,
	Metric = metric_used
)


# dataframe for continuos lineplot
dframe_lnplt <- data.frame(
	Threshold = names(vals_of_interest),
	Avg_NB_1 = nb_vec1
)

if (NUMBER_OF_METRICS == 1){
	# set colors for the plot
	cols <- c("#F8766D")
	names(cols) <- c(metric1)
}
if (NUMBER_OF_METRICS == 2){
	# dataframe for continuos lineplot
	dframe_lnplt$Avg_NB_2 = nb_vec2

	# set colors for the plot
	cols <- c("#F8766D", "#7AC5CD")
	names(cols) <- c(metric1, metric2)
}
if (NUMBER_OF_METRICS == 3){
	# dataframe for continuos lineplot
	dframe_lnplt$Avg_NB_2 = nb_vec2
	dframe_lnplt$Avg_NB_3 = nb_vec3
	
	# set colors for the plot
	cols <- c("#F8766D", "#7AC5CD", "#00BA38")
	names(cols) <- c(metric1, metric2, metric3)
}

pick <- function(condition){
  function(d) d %>% filter_(condition)
}

dframe_bxplt$Threshold <- as.factor(dframe_bxplt$Threshold) 


if (STRIP_FILE_ENDING){
  # strip the .csv ending in the names
  # load the corresponding function from "rand_samp_subset_obe_and_cov.R", use chdir
  # this is shit, look into "C:/CS1_R-Intro/boxplots_3_similarities.R" for more info
  if (!exists("vec_remove_file_endings")){
    source("C:/CS1_R-Intro/rand_samp_subset_obe_and_cov.R")
  }
  names(cols) <- vec_remove_file_endings(names(cols))
  dframe_bxplt$Metric <- vec_remove_file_endings(dframe_bxplt$Metric)
  metric1_strpd <- vec_remove_file_endings(metric1)
  metric2_strpd <- vec_remove_file_endings(metric2)
  metric3_strpd <- vec_remove_file_endings(metric3)
}

# make the Metric a factor to be able to reorder the legend
dframe_bxplt$Metric <- factor(dframe_bxplt$Metric, levels=names(cols))

bx_plots <- ggplot(dframe_bxplt, aes(x=Threshold, y=OBE_Ratios)) +
	scale_x_discrete(limits=rev(levels(as.factor(dframe_bxplt$Threshold)))) +
	geom_boxplot(aes(fill=Metric)) +
	scale_fill_manual(values=cols) + 
	theme(axis.text.x=element_blank(), axis.title.x=element_blank())  # remove x axis for upper plot

base_stp_line <- ggplot(dframe_bxplt, aes(x=Threshold, y=OBE_Ratios)) +
			geom_line(data=pick(~Metric==metric1_strpd), aes(y=Avg_NB, group=1), color=cols[metric1_strpd], size=2, show.legend=FALSE) + 
			scale_x_discrete(limits=rev(levels(as.factor(dframe_bxplt$Threshold))))

base_connected_line <- ln_plots <- ggplot(dframe_lnplt, aes(x=Threshold)) +
			geom_line(aes(y=Avg_NB_1, group=1), color=cols[metric1_strpd], size=2, show.legend=FALSE) +
			scale_x_discrete(limits=rev(levels(as.factor(dframe_lnplt$Threshold)))) +
			labs(y="Avg_Neighborhood")

if (STEPLINE_INSTEAD_OF_LINEPLOT){
	if (NUMBER_OF_METRICS == 1){
		ln_plots <- base_stp_line
	}
	if (NUMBER_OF_METRICS == 2){
		ln_plots <- base_stp_line +
			geom_line(data=pick(~Metric== metric2_strpd), aes(y=Avg_NB, group=1), color=cols[metric2_strpd], size=2, show.legend=FALSE)
	}
	if (NUMBER_OF_METRICS == 3){
		ln_plots <- base_stp_line +
			geom_line(data=pick(~Metric== metric2_strpd), aes(y=Avg_NB, group=1), color=cols[metric2_strpd], size=2, show.legend=FALSE) +
			geom_line(data=pick(~Metric== metric3_strpd), aes(y=Avg_NB, group=1), color=cols[metric3_strpd], size=2, show.legend=FALSE)
	}
} else {
	if (NUMBER_OF_METRICS == 1){
		ln_plots <- base_connected_line
	}
	if (NUMBER_OF_METRICS == 2){
		ln_plots <- base_connected_line +
			geom_line(aes(y=Avg_NB_2, group=1), color=cols[metric2_strpd], size=2, show.legend=FALSE)
	}
	if (NUMBER_OF_METRICS == 3){
		ln_plots <- base_connected_line +
			geom_line(aes(y=Avg_NB_2, group=1), color=cols[metric2_strpd], size=2, show.legend=FALSE) +
			geom_line(aes(y=Avg_NB_3, group=1), color=cols[metric3_strpd], size=2, show.legend=FALSE)
	}
}

# changing the output size for the plot
# somehow this increases the resolution significantly, the ratio however is similar enough
dev.new(width = wid, height = hei, unit="px", noRStudioGD=TRUE)
# change to font size of the ggplots
font_size <- 20
ln_plots <- ln_plots + theme(text = element_text(size=font_size))
bx_plots <- bx_plots + theme(text = element_text(size=font_size)) + ylim(0, 1)
egg::ggarrange(bx_plots, ln_plots)

# TODO add library(egg) and library(cowplot)
#Note: As of version 1.0.0, cowplot does not change the
#  default ggplot2 theme anymore. To recover the previous
#  behavior, execute:
#  theme_set(theme_cowplot())
