# plot the coverage development of multiple subsets
library(ggplot2)
library(reshape2)

# whether bins with under 10 entries are removed
cleanup_covs <- FALSE
# select a dataset
bng_or_drvr = "bng"
# select whether to plot obes or coverages
obes_or_covs = "obes "#"covs"
# configure the steos at which neighborhood size sampling takes place
min_sample_size <- 1
max_sample_size <- 30
step_size <- 2

# coverages of interest
covs_of_interest <- c("steering_bins.csv", "speed_steering_2d_bins_adjusted.csv") 

# add paths to the subsets
if(bng_or_drvr == "bng"){
  paths_lowdiv_obe <- c("C:/CS1_R-Intro/dummy_adaptive_random_sampling")
  paths_hidiv_obe <- c("C:/CS1_R-Intro/dummy_adaptive_random_sampling",
                       "C:/CS1_R-Intro/dummy_adaptive_random_sampling")
  paths_lowdiv_nonobe <- c("C:/CS1_R-Intro/dummy_adaptive_random_sampling")
  paths_hidiv_nonobe <- c("C:/CS1_R-Intro/dummy_adaptive_random_sampling")
} else if (bng_or_drvr == "drvr"){
  # dummy stuff, remove
  paths_lowdiv_obe <- c("C:/CS1_R-Intro/dummy_adaptive_random_sampling")
  paths_hidiv_obe <- c("C:/CS1_R-Intro/dummy_adaptive_random_sampling",
                       "C:/CS1_R-Intro/dummy_adaptive_random_sampling")
  paths_lowdiv_nonobe <- c("C:/CS1_R-Intro/dummy_adaptive_random_sampling")
  paths_hidiv_nonobe <- c("C:/CS1_R-Intro/dummy_adaptive_random_sampling")
}


steps = seq(from=min_sample_size, to=max_sample_size, by=step_size)
steps = c(steps, max_sample_size) # step may be smaller or bigger

# simply adds bins to get coverage
get_coverage <- function(names_vec, cov_dframe){
  num_bins <- length(cov_dframe[0,])
  summed_covs <- rep(0, num_bins)
  for (test in names_vec){
    summed_covs <- summed_covs + cov_dframe[test,]
  }
  # calculate the coverage
  coverage <- sum(summed_covs != 0)/num_bins
  return(coverage)
}

# replace bins that are below a threshold
# taken and simplified from obe_vs_non_obe_coverages.R
cleanup_single_bins_abs <- function(bins, cov_abs_threshold=10, 
                                    cleanup_perc_instead_of_abs=FALSE){
  total_num <- sum(bins)
  if (cleanup_perc_instead_of_abs){
    thres <- total_num * cov_perc_threshold
  } else {
    thres <- cov_abs_threshold
  }
  for (j in 1:length(bins)){
    if (bins[j] < cov_abs_threshold){
      bins[j] <- 0
    }  
  }
  return(bins)
}

# extracts an ordered list of the subset population
get_order <- function(set_path){
  # seems to be working as it should
  setwd(set_path)
  all_tests <- read.csv("for_each_sampling_index.csv", row.names=1)
  all_selected <- subset(all_tests, sampling_index != -1)
  ordered_selected <- all_selected[order(all_selected$sampling_index),,drop=FALSE]
  ordered_names <- rownames(ordered_selected)
  return(ordered_names)
}

# get the development of a single coverage metric for a single set
# uses global steps
get_single_coverage_development <- function(set_path, cov_name, pop_ordered){
  setwd(set_path)
  covs <- read.csv(cov_name, row.names=1)

  # cleanup if needed, modify original dataframe
  if(cleanup_covs){
    for(test in pop_ordered){
      covs[test,] <- cleanup_single_bins(covs[test,])
    } 
  }
  
  # sample the coverage at each step
  num_steps <- length(steps)
  # vector that includes coverages at each step
  vec <- rep(0, num_steps)
  for (i in 1:num_steps) {
    sample_size <- steps[i]
    ordered_subset <- pop_ordered[1:sample_size]
    cov <- get_coverage(names_vec = ordered_subset, cov_dframe = covs)
    vec[i] <- cov
  }
  return(vec)
}

# Get the obe development while sampling
get_single_obe_development <- function(set_path, pop_ordered){
  setwd(set_path)
  for_each_num_obes <- read.csv("for_each_num_obes.csv", row.names=1)
  tests_that_fail <- rownames(for_each_num_obes)[for_each_num_obes$num_obes == 1]
  # sample the coverage at each step
  num_steps <- length(steps)
  # vector that includes coverages at each step
  vec <- rep(0, num_steps)
  for (i in 1:num_steps) {
    sample_size <- steps[i]
    ordered_subset <- pop_ordered[1:sample_size]
    num_obes <- sum(ordered_subset %in% tests_that_fail)
    vec[i] <- num_obes
  }
  return(vec)
}

# get dframe of coverages for single set
# two formats of dataframes, add melting via reshape
get_set_dframe <- function(set_path){
  setwd(set_path)
  pop_ordered <- get_order(set_path)
  # grows in a loop, not optimal
  dframe <- data.frame(steps)
  if (obes_or_covs == "covs") {
    # this is just a dummy, remove in favor of loop
    for(cov_name in covs_of_interest){
      print(cov_name)
      dframe[toString(cov_name)] <- get_single_coverage_development(set_path,
                                                                  cov_name,
                                                                  pop_ordered)
    }
  } else {
    dframe["obes"] <- get_single_obe_development(set_path, pop_ordered)
  }
  return(dframe)
}

# this should be included in a method that runs multiple and is able to average them out
df <- get_set_dframe(paths_lowdiv_obe[1])
print("dataframe of dummy covs")
df
# melting all coverages into one, in order to be able to plot
# for future averaging: add names representing each dset
mdata <- melt(df, id=c("steps"))
ln_plots <- ggplot(mdata, aes(x=steps, y=value, group=variable)) +
  geom_line(aes(color=variable), size=1.5)
ln_plots
