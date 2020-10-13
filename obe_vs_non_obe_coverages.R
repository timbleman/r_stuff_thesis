# plot the coverage development for OBE/non-OBE
library(ggplot2)
library(reshape2)

setwd("C:/CS1_R-Intro/driver-ai-wo-minlen-wo-infspeed-7-steering-4-len-20200818T120651Z-001")
setwd("C:/CS1_R-Intro/experiments-beamng-ai-wo-minlen-wo-infspeed-7-steering-4-len-20200821T084856Z-001")


# has to be user adjusted
coverage_metric <- "steering_bins.csv"
coverage_metric <- "steering_bins_non_uniform_percentile.csv"
coverage_metric <- "speed_bins.csv"
#coverage_metric <- "obe_2d.csv"
#coverage_metric <- "speed_steering_2d_bins.csv"
min_sample_size <- 1
max_sample_size <- 18
step_size <- 2
# number of repetitions, to average out irregularitites
NUM_REP <- 3
SEEED <- 123

# this has also to be adjusted below for the others
# deftermines whether sparsely populated bins should be eliminated and at what percentage
cleanup_covs <- FALSE
# FALSE is better, is not impacted by short execution
cleanup_perc_instead_of_abs <- FALSE
cov_perc_threshold <- 0.05
cov_abs_threshold <- 10


covs <- read.csv(coverage_metric, check.names=FALSE, row.names=1)

for_each_num_obes <- read.csv("for_each_num_obes.csv" , row.names=1)
tests_that_fail <- row.names(for_each_num_obes)[for_each_num_obes$num_obes == 1]
tests_that_do_not_fail <- row.names(for_each_num_obes)[for_each_num_obes$num_obes == 0]

stopifnot(length(tests_that_fail) >= max_sample_size &&
		length(tests_that_do_not_fail) >= max_sample_size )

print("jo")
# missing: cleanup
# to cleanup all covered bins under a certain threshold
cleanup_single_bins <- function(bins){
	total_num <- sum(bins)
	stopifnot(cov_perc_threshold < 1 && cov_perc_threshold>= 0)
	if (cleanup_perc_instead_of_abs){
		thres <- total_num * cov_perc_threshold
	} else {
		thres <- cov_abs_threshold
	}
	for (j in 1:length(bins)){
		if (bins[j] < thres){
			bins[j] <- 0
		}  
	}
	return(bins)
}

steps = seq(from=min_sample_size, to=max_sample_size, by=step_size)
steps = c(steps, max_sample_size) # step may be smaller or bigger

get_coverage <- function(names_vec){
	num_bins <- length(covs[0,])
	summed_covs <- rep(0, num_bins)
	for (test in names_vec){
		summed_covs <- summed_covs + covs[test,]
	}
	# calculate the coverage
	coverage <- sum(summed_covs != 0)/num_bins
	return(coverage)
}

sample_covs_and_average <- function(num_repetitions){
	num_steps <- length(steps)
	obe_cov_i <- rep(0, num_steps)
	non_obe_cov_i <- rep(0, num_steps)
	
	for (k in 1:num_repetitions){
		max_sample_obe = sample(tests_that_fail, max_sample_size)
		max_sample_non_obe = sample(tests_that_do_not_fail, max_sample_size)
		# perform cleanup for all samples, seems to work
		if (cleanup_covs){
			all_tests_of_interest <- c(max_sample_obe, max_sample_non_obe)
			for (test in all_tests_of_interest){
				covs[test,] <<- cleanup_single_bins(covs[test,])
			}
		}

		for (i in 1:num_steps) {
			sample_size <- steps[i]
			obe_sub_set <- max_sample_obe[1:sample_size]
			non_obe_sub_set <- max_sample_non_obe[1:sample_size]
			obe_sub_cov <- get_coverage(obe_sub_set)
			non_obe_sub_cov <- get_coverage(non_obe_sub_set)
			obe_cov_i[i] <- obe_cov_i[i] + obe_sub_cov
			non_obe_cov_i[i] <- non_obe_cov_i[i] + non_obe_sub_cov
		}
	}
	print(paste("obe_cov_i", obe_cov_i))

	obe_cov_i =  obe_cov_i / num_repetitions
	non_obe_cov_i =  non_obe_cov_i / num_repetitions
	L1 <- list(obe_cov_i, non_obe_cov_i)
	return(L1)
}


# This is the important part, dont mess this up!
# sample coverages without cleanup
print("sample coverages without cleanup")
set.seed(SEEED)
L_NOCL <- sample_covs_and_average(num_repetitions = NUM_REP)
obe_cov_no_cl <- L_NOCL[[1]]
non_obe_cov_no_cl <- L_NOCL[[2]]

# sample coverages with percentage cleanup
print("sample coverages with percentage cleanup")
# deftermines whether sparsely populated bins should be eliminated and at what percentage
cleanup_covs <- TRUE
# FALSE is better, is not impacted by short execution
cleanup_perc_instead_of_abs <- TRUE
cov_perc_threshold <- 0.05
set.seed(SEEED)
L_PERCL <- sample_covs_and_average(num_repetitions = NUM_REP)
obe_cov_perc_cl <- L_PERCL[[1]]
non_obe_cov_perc_cl <- L_PERCL[[2]]

# sample coverages with absolute cleanup
print("sample coverages with absolute cleanup")
# deftermines whether sparsely populated bins should be eliminated and at what percentage
cleanup_covs <- TRUE
# FALSE is better, is not impacted by short execution
cleanup_perc_instead_of_abs <- FALSE
cov_abs_threshold <- 10
set.seed(SEEED)
L_ABSCL <- sample_covs_and_average(num_repetitions = NUM_REP)
obe_cov_abs_cl <- L_ABSCL[[1]]
non_obe_cov_abs_cl <- L_ABSCL[[2]]



print("Obe ratios at various sample sizes")
steps
obe_cov
non_obe_cov

dframe_obe_cov <- data.frame(
	Sample_size <- steps,
	OBE_cov_no_cl <- obe_cov_no_cl,
	non_OBE_cov_no_cl <- non_obe_cov_no_cl,
	OBE_cov_perc_cl <- obe_cov_perc_cl,
	non_OBE_cov_perc_cl <- non_obe_cov_perc_cl,
	OBE_cov_abs_cl <- obe_cov_abs_cl,
	non_OBE_cov_abs_cl <- non_obe_cov_abs_cl
)

# new dataframe for different plotting (point lines)
all_sample_sizes <- c(steps, steps, steps, steps, steps, steps)
all_cov_confs <- c(rep("OBE_cov_no_cl", length(steps)), rep("non_OBE_cov_no_cl", length(steps)),
			rep("OBE_cov_perc_cl", length(steps)), rep("non_OBE_cov_perc_cl", length(steps)),
			rep("OBE_cov_abs_cl", length(steps)), rep("non_OBE_cov_abs_cl", length(steps)))
all_covs <- c(obe_cov_no_cl,
		non_obe_cov_no_cl,
		obe_cov_perc_cl,
		non_obe_cov_perc_cl,
		obe_cov_abs_cl,
		non_obe_cov_abs_cl
)
dframe_obe_cov_reord <- data.frame(
	configuration <- all_cof_confs,
	sample_size <- all_sample_sizes,
	covs <- all_covs
)
colnames(dframe_obe_cov_reord) <- c("configuration", "sample_size", "covs")

cols <- c("#FF0000", "#0000FF", "#F8766D", "#7AC5CD", "#F4CCCC", "#C9DAF8")
names(cols) <- c("OBE_cov_no_cl", "non_OBE_cov_no_cl",
			"OBE_cov_perc_cl", "non_OBE_cov_perc_cl",
			"OBE_cov_abs_cl", "non_OBE_cov_abs_cl")

desc_name <- paste(coverage_metric, "coverages", "at", NUM_REP, "repetitions,")

ln_plots <- ggplot(dframe_obe_cov, aes(x=Sample_size)) +
		ylim(0,1) +
		geom_line(aes(y=OBE_cov_no_cl, group=1, color=cols["OBE_cov_no_cl"]), size=1, show.legend=TRUE) +
		geom_line(aes(y=non_OBE_cov_no_cl, group=1, color=cols["non_OBE_cov_no_cl"]), size=1, show.legend=TRUE) +
		geom_line(aes(y=OBE_cov_perc_cl, group=1, color=cols["OBE_cov_perc_cl"]), size=1, show.legend=TRUE) +
		geom_line(aes(y=non_OBE_cov_perc_cl, group=1, color=cols["non_OBE_cov_perc_cl"]), size=1, show.legend=TRUE) +
		geom_line(aes(y=OBE_cov_abs_cl, group=1, color=cols["OBE_cov_abs_cl"]), size=1, show.legend=TRUE, alpha=0.5) +
		geom_line(aes(y=non_OBE_cov_abs_cl, group=1, color=cols["non_OBE_cov_abs_cl"]), size=1, show.legend=TRUE, alpha=0.5) +
		#geom_point(aes(y=OBE_cov_no_cl), size=2) + 
		ggtitle(desc_name) +
		scale_color_identity(name = "Coverages",
						breaks = cols,
						labels = names(cols),
						guide = "legend")
ln_plots <- ggplot(dframe_obe_cov_reord, aes(x=sample_size, y=covs, group=configuration)) +
  		geom_line(aes(color=configuration), size=1.2)+
  		geom_point(aes(shape=configuration, color=configuration), size=2.2)
ln_plots <- ln_plots + scale_color_manual(values=cols)
ln_plots