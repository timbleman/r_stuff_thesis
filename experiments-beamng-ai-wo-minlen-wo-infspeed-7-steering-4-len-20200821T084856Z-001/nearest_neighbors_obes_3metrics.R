library(ggplot)
library(dplyr)  # for the filter function
library(egg)  # for having plots stacked


setwd("C:/CS1_R-Intro/driver-ai-wo-minlen-wo-infspeed-7-steering-4-len-20200818T120651Z-001")
setwd("C:/CS1_R-Intro/experiments-beamng-ai-wo-minlen-wo-infspeed-7-steering-4-len-20200821T084856Z-001")


BOXPLOT_INSTEAD_OF_LINEPLOT = FALSE


# get obe count for all tests and extract the ones that fail
for_each_num_obes <- read.csv("for_each_num_obes.csv" , row.names=1)
tests_that_fail <- row.names(for_each_num_obes)[for_each_num_obes$num_obes == 1]

# do not get filled, remaining from previous single metric plotting
vals_of_interest <- c("0.95" = 0.0, 
				"0.9" = 0.0,
				"0.85" = 0.0,
				"0.8" = 0.0,
				"0.7" = 0.0,
				"0.6" = 0.0,
				"0.0" = 0.0)
# uncomment for jaccard
metric1 = "jaccard_28alph.csv"
metric2 = "jaccard_44alph.csv"
metric3 = "jaccard_60alph.csv"
# uncomment for sliding window 1d alphabet size
#metric1 = "curve_sdl_dist_7ang.csv"
#metric2 = "curve_sdl_dist_11ang.csv"
#metric3 = "curve_sdl_dist_15ang.csv"
# uncomment for sliding window 2d alphabet size
#metric1 = "sdl_2d_dist_28alph.csv"
#metric2 = "sdl_2d_dist_44alph.csv"
#metric3 = "sdl_2d_dist_60alph.csv"

# bool to control what neighbors are taken
GREATER_THAN = TRUE


len_observations = 3*length(vals_of_interest)*length(tests_that_fail)
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

nb_vec1 <- fill_ratio_nb(1, metric1)
new_index <- len_observations/3 + 1
nb_vec2 <- fill_ratio_nb(new_index, metric2)
new_index <- 2*(len_observations/3) + 1
nb_vec3 <- fill_ratio_nb(new_index, metric3)

# dataframe for continuos lineplot
dframe_lnplt <- data.frame(
	Threshold = names(vals_of_interest),
	Avg_NB_1 = nb_vec1, 
	Avg_NB_2 = nb_vec2, 
	Avg_NB_3 = nb_vec3 
)

dframe_bxplt <- data.frame(
	Threshold = t_holds_repeated,
	OBE_Ratios = all_ratios_vec,
	Avg_NB = sampled_avg_nb,
	Metric = metric_used
)

# set colors for the plot
cols <- c("brown3", "cadetblue3", "green")
names(cols) <- c(metric1, metric2, metric3)

pick <- function(condition){
  function(d) d %>% filter_(condition)
}

dframe_bxplt$Threshold <- as.factor(dframe_bxplt$Threshold) 

bx_plots <- ggplot(dframe_bxplt, aes(x=Threshold, y=OBE_Ratios)) +
	scale_x_discrete(limits=rev(levels(as.factor(dframe_bxplt$Threshold)))) +
	geom_boxplot(aes(fill=Metric)) +
	scale_color_manual(values=cols) + 
	theme(axis.text.x=element_blank(), axis.title.x=element_blank())  # remove x axis for upper plot

if (BOXPLOT_INSTEAD_OF_LINEPLOT){
	ln_plots <- ggplot(dframe_bxplt, aes(x=Threshold, y=OBE_Ratios)) +
		geom_line(data=pick(~Metric== metric1), aes(y=Avg_NB, group=1, color=cols[metric1]), size=2, show.legend=FALSE) + 
		geom_line(data=pick(~Metric== metric2), aes(y=Avg_NB, group=1, color=cols[metric2]), size=2, show.legend=FALSE) +
		geom_line(data=pick(~Metric== metric3), aes(y=Avg_NB, group=1, color=cols[metric3]), size=2, show.legend=FALSE) +
		scale_x_discrete(limits=rev(levels(as.factor(dframe_bxplt$Threshold))))
} else {
	ln_plots <- ggplot(dframe_lnplt, aes(x=Threshold)) +
		geom_line(aes(y=Avg_NB_1, group=1, color=cols[metric1]), size=2, show.legend=FALSE) +
		geom_line(aes(y=Avg_NB_2, group=1, color=cols[metric2]), size=2, show.legend=FALSE) +
		geom_line(aes(y=Avg_NB_3, group=1, color=cols[metric3]), size=2, show.legend=FALSE) +
		scale_x_discrete(limits=rev(levels(as.factor(dframe_lnplt$Threshold))))
}

ln_plots
#cowplot::plot_grid(bx_plots, ln_plots, align="v", ncol=1)
egg::ggarrange(bx_plots, ln_plots)

# TODO add library(egg) and library(cowplot)
#Note: As of version 1.0.0, cowplot does not change the
#  default ggplot2 theme anymore. To recover the previous
#  behavior, execute:
#  theme_set(theme_cowplot())
