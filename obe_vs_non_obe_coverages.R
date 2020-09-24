# plot the coverage development for OBE/non-OBE
library(ggplot2)

setwd("C:/CS1_R-Intro/driver-ai-wo-minlen-wo-infspeed-7-steering-4-len-20200818T120651Z-001")
setwd("C:/CS1_R-Intro/experiments-beamng-ai-wo-minlen-wo-infspeed-7-steering-4-len-20200821T084856Z-001")


# has to be user adjusted
coverage_metric = "steering_bins.csv"
#coverage_metric = "obe_2d.csv"
min_sample_size = 1
max_sample_size = 18
step_size = 2
# deftermines whether sparsely populated bins should be eliminated and at what percentage
cleanup_covs = TRUE
cov_perc_threshold = 0.02


covs <- read.csv(coverage_metric, check.names=FALSE, row.names=1)

for_each_num_obes <- read.csv("for_each_num_obes.csv" , row.names=1)
tests_that_fail <- row.names(for_each_num_obes)[for_each_num_obes$num_obes == 1]
tests_that_do_not_fail <- row.names(for_each_num_obes)[for_each_num_obes$num_obes == 0]

stopifnot(length(tests_that_fail) >= max_sample_size &&
		length(tests_that_do_not_fail) >= max_sample_size )

print("jo")
# missing: cleanup
# to cleanup all covered bins under a certain threshold
cleanup_single_bins <- function(bins, threshold=cov_perc_threshold){
	total_num <- sum(bins)
	stopifnot(threshold < 1 && threshold >= 0)
	thres <- total_num * threshold
	for (j in 1:length(bins)){
		if (bins[j] < thres){
			bins[j] <- 0
		}  
	}
	return(bins)
}

# TODO remove
set.seed(1234)
max_sample_obe = sample(tests_that_fail, max_sample_size)
max_sample_non_obe = sample(tests_that_do_not_fail, max_sample_size)
# perform cleanup for all samples, seems to work
if (cleanup_covs){
	all_tests_of_interest <- c(max_sample_obe, max_sample_non_obe)
	for (test in all_tests_of_interest){
		covs[test,] <- cleanup_single_bins(covs[test,])
	}
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

num_steps <- length(steps)
obe_cov <- rep(0, num_steps)
non_obe_cov <- rep(0, num_steps)
for (i in 1:num_steps) {
	sample_size <- steps[i]
	print(sample_size)
	obe_sub_set <- max_sample_obe[1:sample_size]
	non_obe_sub_set <- max_sample_non_obe[1:sample_size]
	obe_sub_cov <- get_coverage(obe_sub_set)
	non_obe_sub_cov <- get_coverage(non_obe_sub_set)
	obe_cov[i] <- obe_sub_cov
	non_obe_cov[i] <- non_obe_sub_cov
}

print("Obe ratios at various sample sizes")
steps
obe_cov
non_obe_cov

dframe_obe_cov <- data.frame(
	Sample_size <- steps,
	OBE_coverage <- obe_cov,
	non_OBE_coverage <- non_obe_cov
)

cols <- c("brown3", "cadetblue3")
names(cols) <- c("OBE_coverage", "non_OBE_coverage")

ln_plots <- ggplot(dframe_obe_cov, aes(x=Sample_size)) +
		geom_line(aes(y=OBE_coverage, group=1), color=cols["OBE_coverage"], size=2, show.legend=TRUE) +
		geom_line(aes(y=non_OBE_coverage, group=1), color=cols["non_OBE_coverage"], size=2, show.legend=TRUE)
ln_plots