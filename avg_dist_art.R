# calculates the average similarity for a number of sambled subsets

# if larger than subset, or <= 0, all are taken
step = 31
# metric to compute the average score on
similarity_metric = "jaccard_11ang_4len.csv"

# select a dataset
bng_or_drvr = "bng"

# TODO move in a extra file
# add paths to the subsets
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
  # dummy stuff, remove
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
listed_set_paths <- list("paths_lowdiv" = c(paths_lowdiv_obe, paths_hidiv_obe), 
                         "paths_hidiv" = c(paths_lowdiv_nonobe, paths_hidiv_nonobe), 
                         "random_sampling" = random_sampling)
# add the parent path to each folder
for (i in 1:length(listed_set_paths)){
  for (j in 1:length(listed_set_paths[[i]])){
    listed_set_paths[[i]][j] <- paste(higher_level_folder, 
                                      listed_set_paths[[i]][j], sep="")
  }
}

# copied from sampled_subset_coverage_development.R
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

# cacluates the average distance a a certain number of neighbors
get_partial_avg_dist <- function(set_path, metr_name, step){
  setwd(set_path)
  ord <- get_order(set_path)
  sim_matr <- read.csv(metr_name, check.names=FALSE, row.names=1)
  # take only the distances of the first n tests
  if (step < nrow(sim_matr) & step > 0){
    first_tests <- ord[1:step]
    sim_matr <- sim_matr[first_tests, first_tests]
  }
  return (mean(data.matrix(sim_matr)))
}
  
# get the average similarity at a number of neighbors for multiple subsets
get_avg_vec <- function(paths_vec, similarity_metric, step){
  vals_vec <- rep(0, length(paths_vec))
  i <- 1
  for (p in paths_vec){
    avg_sim <- get_partial_avg_dist(p, similarity_metric, step)
    vals_vec[i] <- avg_sim
    i <- i + 1
  }
  return(vals_vec)
}

# compares two different configurations for multiple subsets (e.g. high vs low diversity)
# use names to make plotting more readable
compare_two_conf_avgs <- function(paths_vec_1, paths_vec_2, names, 
                                  similarity_metric,
                                  step){
  vec1 <- get_avg_vec(paths_vec_1, similarity_metric, step)
  vec2 <- get_avg_vec(paths_vec_2, similarity_metric, step)
  if (is.na(names)){
    names <- c("config 1", "config 2")
  }
  print(paste("Comparing", names[1], "to", names[2], "at step", step))
  print(paste("Avg", names[1], ":", mean(vec1), ";", names[2], ":", mean(vec2)))
  print(paste("Wilcox p-value", wilcox.test(vec1, vec2)["p.value"]))
}

compare_two_conf_avgs(listed_set_paths[[1]], listed_set_paths[[2]], c("lowdiv", "hidiv"), similarity_metric, 27)
