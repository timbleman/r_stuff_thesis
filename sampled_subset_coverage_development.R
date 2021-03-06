# Plots the coverage development of multiple subsets, uses cumulative plots
# high diversity, low diversity and random subsets used 
# Used in RQ4
# Writes plots to the path specified in png_save_path
# Takes a fair amount of time, be patient
# Averages out the coverage values of subsets of the same class
# What to adjust:
# bng_or_drvr: Select a group of paths to subsets
# relative_instead_of_absolute_coverages: Coverage values relative to the suite the subsets were extracted from
# min_sample_size, max_sample_size, step_size: The range in which coverage values are examined
# covs_of_interest: coverage metrics that should be plotted, add their csv name ("speed_bins.csv") and/or "OBE"
# Note: If you deviate from my folder structure, all the paths have to be adjusted manually!
# The paths should be shared between this script and rand_samp_subset_obe_and_cov.R, for now you have to copy and paste.
library(ggplot2)
library(reshape2)
library(cowplot)

# Select a dataset ("bng" or "drvr")
bng_or_drvr = "drvr"
# Relative to parent suite
relative_instead_of_absolute_coverages <- TRUE
# Print without legend
without_legend <- TRUE
# Configure the steos at which neighborhood size sampling takes place
min_sample_size <- 1
max_sample_size <- 30
step_size <- 2
# Whether bins with under 10 entries are removed
cleanup_covs <- FALSE
# Select whether to plot obes or coverages, leave it at "cov"
obes_or_covs = "covs" #"obes" #"covs"

# Coverages of interest
covs_of_interest <- c("OBE", "steering_bins.csv", "speed_bins.csv", 
                      "speed_steering_2d_bins_adjusted.csv",
                      "obe_2d.csv") 
# This is the only metric I use with cleanup in the thesis
if (cleanup_covs){
  covs_of_interest <- c("speed_steering_2d_bins.csv")
}

# Add paths to the subsets
# All these have to adjusted manually if you deviate from my file location or naming!
# Mind the groups of the paths.
if(bng_or_drvr == "bng"){
  higher_level_folder <- "C:/CS1_R-Intro/div_bng5_only_results/"
  paths_lowdiv_obe <- c("experiments-beamng-ai-wo-minlen-wo-infspeed-lowdiv_random--la111",
                        "experiments-beamng-ai-wo-minlen-wo-infspeed-lowdiv_random--la111",
                        "experiments-beamng-ai-wo-minlen-wo-infspeed-lowdiv_random--la617",
                        "experiments-beamng-ai-wo-minlen-wo-infspeed-lowdiv_random--la219",
                        "experiments-beamng-ai-wo-minlen-wo-infspeed-lowdiv_random--la811"
                        )
  paths_hidiv_obe <- c("experiments-beamng-ai-wo-minlen-wo-infspeed-highdiv_random--la111",
                       "experiments-beamng-ai-wo-minlen-wo-infspeed-highdiv_random--la111",
                       "experiments-beamng-ai-wo-minlen-wo-infspeed-highdiv_random--la617",
                       "experiments-beamng-ai-wo-minlen-wo-infspeed-highdiv_random--la219",
                       "experiments-beamng-ai-wo-minlen-wo-infspeed-highdiv_random--la811"
                       )
  paths_lowdiv_nonobe <- c("experiments-beamng-ai-wo-minlen-wo-infspeed-lowdiv_random--la311",
                           "experiments-beamng-ai-wo-minlen-wo-infspeed-lowdiv_random--la222",
                           "experiments-beamng-ai-wo-minlen-wo-infspeed-lowdiv_random--la711",
                           "experiments-beamng-ai-wo-minlen-wo-infspeed-lowdiv_random--la84",
                           "experiments-beamng-ai-wo-minlen-wo-infspeed-lowdiv_random--la918"
                           )
  paths_hidiv_nonobe <- c("experiments-beamng-ai-wo-minlen-wo-infspeed-highdiv_random--la311",
                          "experiments-beamng-ai-wo-minlen-wo-infspeed-highdiv_random--la222",
                          "experiments-beamng-ai-wo-minlen-wo-infspeed-highdiv_random--la711",
                          "experiments-beamng-ai-wo-minlen-wo-infspeed-highdiv_random--la84",
                          "experiments-beamng-ai-wo-minlen-wo-infspeed-highdiv_random--la918"
                          )
  random_sampling <- c("experiments-beamng-ai-wo-minlen-wo-infspeed--random0",
                       "experiments-beamng-ai-wo-minlen-wo-infspeed--random1",
                       "experiments-beamng-ai-wo-minlen-wo-infspeed--random2",
                       "experiments-beamng-ai-wo-minlen-wo-infspeed--random3",
                       "experiments-beamng-ai-wo-minlen-wo-infspeed--random4")
  parent_suite <- "C:/CS1_R-Intro/experiments-beamng-ai-wo-minlen-wo-infspeed-7-steering-4-len-20200821T084856Z-001"
  png_save_path <- "C:/CS1_R-Intro/RQ4/bng_5_subsets"
} else if (bng_or_drvr == "drvr"){
  higher_level_folder <- "C:/CS1_R-Intro/div_drvr5_only_results/"
  paths_lowdiv_obe <- c("experiments-driver-ai-wo-minlen-wo-infspeed-lowdiv_random--la219",
                        "experiments-driver-ai-wo-minlen-wo-infspeed-lowdiv_random--la318",
                        "experiments-driver-ai-wo-minlen-wo-infspeed-lowdiv_random--la520",
                        "experiments-driver-ai-wo-minlen-wo-infspeed-lowdiv_random--la438",
                        "experiments-driver-ai-wo-minlen-wo-infspeed-lowdiv_random--la712"
                        )
  paths_hidiv_obe <- c("experiments-driver-ai-wo-minlen-wo-infspeed-highdiv_random--la219",
                       "experiments-driver-ai-wo-minlen-wo-infspeed-highdiv_random--la318",
                       "experiments-driver-ai-wo-minlen-wo-infspeed-highdiv_random--la520",
                       "experiments-driver-ai-wo-minlen-wo-infspeed-highdiv_random--la438",
                       "experiments-driver-ai-wo-minlen-wo-infspeed-highdiv_random--la712"
                       )
  paths_lowdiv_nonobe <- c("experiments-driver-ai-wo-minlen-wo-infspeed-lowdiv_one-plus-o212",
                           "experiments-driver-ai-wo-minlen-wo-infspeed-lowdiv_random--la42",
                           "experiments-driver-ai-wo-minlen-wo-infspeed-lowdiv_random--la68",
                           "experiments-driver-ai-wo-minlen-wo-infspeed-lowdiv_random--la94",
                           "experiments-driver-ai-wo-minlen-wo-infspeed-lowdiv_random--la1010"
                           )
  paths_hidiv_nonobe <- c("experiments-driver-ai-wo-minlen-wo-infspeed-highdiv_one-plus-o212",
                          "experiments-driver-ai-wo-minlen-wo-infspeed-highdiv_random--la42",
                          "experiments-driver-ai-wo-minlen-wo-infspeed-highdiv_random--la68",
                          "experiments-driver-ai-wo-minlen-wo-infspeed-highdiv_random--la94",
                          "experiments-driver-ai-wo-minlen-wo-infspeed-highdiv_random--la1010"
                          )
  random_sampling <- c("experiments-driver-ai-wo-minlen-wo-infspeed--random0",
                       "experiments-driver-ai-wo-minlen-wo-infspeed--random1",
                       "experiments-driver-ai-wo-minlen-wo-infspeed--random2",
                       "experiments-driver-ai-wo-minlen-wo-infspeed--random3",
                       "experiments-driver-ai-wo-minlen-wo-infspeed--random4"
                       )
  parent_suite <- "C:/CS1_R-Intro/driver-ai-wo-minlen-wo-infspeed-7-steering-4-len-20200818T120651Z-001"
  png_save_path <- "C:/CS1_R-Intro/RQ4/drvr_5_subsets"
}
# create list of paths for iterating
listed_set_paths <- list("paths_lowdiv_obe" = paths_lowdiv_obe, 
                         "paths_hidiv_obe" = paths_hidiv_obe, 
                         "paths_lowdiv_nonobe" = paths_lowdiv_nonobe, 
                         "paths_hidiv_nonobe" = paths_hidiv_nonobe,
                         "random_sampling" = random_sampling)
# add the parent path to each folder
for (i in 1:length(listed_set_paths)){
  for (j in 1:length(listed_set_paths[[i]])){
    listed_set_paths[[i]][j] <- paste(higher_level_folder, 
                                      listed_set_paths[[i]][j], sep="")
  }
}

steps = seq(from=min_sample_size, to=max_sample_size, by=step_size)
steps = c(steps, max_sample_size) # step may be smaller or bigger

# get all absolute coverages of the parent suite
get_parent_coverages <- function(parent_path=parent_suite){
  prevwd <- getwd()
  setwd(parent_path)
  p_covs <- read.csv("whole_suite_coverages.csv", row.names=1)
  setwd(prevwd)
  return(p_covs)
}

parent_covs <- get_parent_coverages()

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
  target_file <- "for_each_sampling_index.csv"
  if (file.exists(target_file)){
    all_tests <- read.csv(target_file, row.names=1)
    all_selected <- subset(all_tests, sampling_index != -1)
    ordered_selected <- all_selected[order(all_selected$sampling_index),,drop=FALSE]
    ordered_names <- rownames(ordered_selected)
  } else {
    print(paste("Warning", target_file, "has not been found in", set_path))
    print("Now using a random order.")
    ordered_names <- get_random_order(set_path)
  }
  
  return(ordered_names)
}

# if no order of sampling is defined 
# use the names found in a dummy file and return a random order
get_random_order <- function(set_path){
  setwd(set_path)
  # use for_each_num_obes.csv as dummy file
  for_each_num_obes <- read.csv("for_each_num_obes.csv", check.names=FALSE, row.names=1)
  shuffeled_names <- sample(rownames(for_each_num_obes))
  return(shuffeled_names)
}

# get the development of a single coverage metric for a single set
# uses global steps
get_single_coverage_development <- function(set_path, cov_name, pop_ordered){
  setwd(set_path)
  covs <- read.csv(cov_name, row.names=1)

  # cleanup if needed, modify original dataframe
  if(cleanup_covs){
    for(test in pop_ordered){
      covs[test,] <- cleanup_single_bins_abs(covs[test,])
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
  if(relative_instead_of_absolute_coverages){
    # strip the ".csv"
    corrected_cov_name <- substr(cov_name, 0, nchar(cov_name)-4)
    # add cleanup to the name if this is enabled
    if (cleanup_covs){
      corrected_cov_name <- paste(corrected_cov_name, "cleanup", sep = "_")
    }
    print(paste("corr cov name:", corrected_cov_name))
    p_cov <- parent_covs[corrected_cov_name,]
    print(paste("parent_cov:", p_cov))
    vec <- vec / p_cov
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
  # if relative is selected divide subset num obes by parent suite num obes
  if (relative_instead_of_absolute_coverages){
    prevwd <- getwd()
    setwd(parent_suite)
    for_each_num_obes <- read.csv("for_each_num_obes.csv" , row.names=1)
    num_obes <- sum(for_each_num_obes)
    vec <- vec / num_obes
    setwd(prevwd)
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

# calculates hav many values there are for all subsets
num_dframe_entries_per_set <- function(){
  num_sets <- 0
  for (set in listed_set_paths){
    num_sets <- num_sets + length(set)
  }
  return(num_sets * length(steps))
}

# get the dataframe for multiple sets with a fixed coverage metric
# set cov name to "OBE" to get the obe development
get_metric_dframe <- function(cov_name){
  # determine length of vectors, for all the values
  total_len <- num_dframe_entries_per_set()
  step_len <- length(steps)
  # there has to be a better way to get all the steps, maybe rep(names(steps), n)?
  all_steps <- rep(0, total_len)
  all_configs <- rep("", total_len)
  all_covs <- rep(0, total_len)
  all_names <- rep("", total_len)
  i <- 1
  
  # loop over set coinfiguration paths
  #for (set_conf in listed_set_paths){
  for (set_conf_name in names(listed_set_paths)){
    # get the path lists, strange implicit conversion to lists
    set_conf <- unlist(listed_set_paths[set_conf_name], use.names=FALSE)
    # simple name for subsets for each configuration
    nme <- 1
    # loop over subsets for each configuration
    for (s_path in set_conf){
      pop_ordered <- get_order(s_path)
      if (cov_name == "OBE"){
        cov <- get_single_obe_development(s_path, pop_ordered)  
      } else {
        cov <- get_single_coverage_development(s_path, cov_name, pop_ordered)
      }
      
      # add data to lists
      # list of indices
      seq_is <- i: (i + step_len - 1)
      # fill vectors
      all_steps[seq_is] <- steps
      all_configs[seq_is] <- set_conf_name
      all_names[seq_is] <- toString(nme)
      nme <- nme + 1
      all_covs[seq_is] <- cov
      
      # increment by the index of seq_is + 1
      i <- seq_is[length(seq_is)] + 1
    }
  }
  
  # create a dataframe of vectors
  df <- data.frame(all_steps, all_configs, all_names, all_covs)
  #print(df)
  return(df)
}

dummy_plot_one_set <- function(){
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
}


# creates a filename for saving pngs
# has to be passed the full name of a coverage, including ".csv"!
get_cov_png_filename <- function(full_cov){
  # save the file with appropriate filename
  if (relative_instead_of_absolute_coverages){
    rel <- "relative"
  } else {
    rel <- "absolute"
  }
  
  if (full_cov == "OBE"){
    file_name <-  paste(rel, "_obe_for_each_sampling_configs", 
                        "_", bng_or_drvr, ".png", sep="")
  } else {
    cov_name <- substr(full_cov, 1, nchar(full_cov)-4)
    if (cleanup_covs){
      cln <- "w_cleanup"
    } else {
      cln <- "wo_cleanup"
    }
    file_name <- paste(cov_name, "_", rel, "_coverage_for_each_sampling_configs_", 
                       cln, "_", bng_or_drvr, ".png", sep="")
  }
  return(file_name)
}

# calculate the coverages of multiple sets for each coverage metric and save the plots
plot_and_save_singlecov_multiset <- function(){
  draw_legend = TRUE
  # for all coverages
  for (cov in covs_of_interest){
    cov_name <- substr(cov, 1, nchar(cov)-4)
    print(paste("Creating plot for",  cov_name))
    # create the plot
    df <- get_metric_dframe(cov)
    theme_set(theme_classic(base_size=36))
    if (!without_legend){
      ln_plots <- ggplot(df, aes(x=all_steps, y=all_covs, group=all_configs)) +
        geom_line(aes(color=all_configs), size=1.5, stat="summary", fun="mean")
      ln_plots
    } else {
      ln_plots <- ggplot(df, aes(x=all_steps, y=all_covs, group=all_configs)) +
        geom_line(aes(color=all_configs), size=1.5, stat="summary", fun="mean")
      # draw extra legend
      if (draw_legend){
        library(cowplot)
        leg <- cowplot::get_legend(ln_plots)
        leg
        save_img(png_save_path, "legend.png")
        draw_legend <- FALSE
      }
      ln_plots <- ln_plots +
        theme(legend.position = "none")
      ln_plots
    }
    file_name = get_cov_png_filename(cov)
    print(file_name)
    save_img(png_save_path, file_name)
  }
}

save_img <- function(ipath, file_name){
  prevwd <- getwd()
  setwd(ipath)
  ggsave(filename=file_name)
  setwd(prevwd)
}


plot_and_save_singlecov_multiset()
