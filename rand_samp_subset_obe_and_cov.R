# plots coverages and obes of adaptive random sampling subsets
library(ggplot2)
library(reshape2)
library(xtable)

bng_or_drvr = "bng"
obes_or_covs = "covs"

# relative to parent suite
relative_instead_of_absolute_coverages <- TRUE
# validity checks, keep off for dummy suites
CHECKS = FALSE

# coverages of interest
covs_of_interest <- c("steering_bins.csv", "speed_bins.csv", 
                      "speed_steering_2d_bins_adjusted.csv")
covs_of_interest <- c("steering_bins_cleanup.csv", "speed_bins_cleanup.csv", 
                      "speed_steering_2d_bins_cleanup.csv")


if(bng_or_drvr == "bng"){
  paths_lowdiv_obe <- c("C:/CS1_R-Intro/div_bng5_only_results/experiments-beamng-ai-wo-minlen-wo-infspeed-lowdiv_random--la111",
                        "C:/CS1_R-Intro/div_bng5_only_results/experiments-beamng-ai-wo-minlen-wo-infspeed-lowdiv_random--la111",
                        "C:/CS1_R-Intro/div_bng5_only_results/experiments-beamng-ai-wo-minlen-wo-infspeed-lowdiv_random--la617",
                        "C:/CS1_R-Intro/div_bng5_only_results/experiments-beamng-ai-wo-minlen-wo-infspeed-lowdiv_random--la219",
                        "C:/CS1_R-Intro/div_bng5_only_results/experiments-beamng-ai-wo-minlen-wo-infspeed-lowdiv_random--la811"
  )
  paths_hidiv_obe <- c("C:/CS1_R-Intro/div_bng5_only_results/experiments-beamng-ai-wo-minlen-wo-infspeed-highdiv_random--la111",
                       "C:/CS1_R-Intro/div_bng5_only_results/experiments-beamng-ai-wo-minlen-wo-infspeed-highdiv_random--la111",
                       "C:/CS1_R-Intro/div_bng5_only_results/experiments-beamng-ai-wo-minlen-wo-infspeed-highdiv_random--la617",
                       "C:/CS1_R-Intro/div_bng5_only_results/experiments-beamng-ai-wo-minlen-wo-infspeed-highdiv_random--la219",
                       "C:/CS1_R-Intro/div_bng5_only_results/experiments-beamng-ai-wo-minlen-wo-infspeed-highdiv_random--la811"
  )
  paths_lowdiv_nonobe <- c("C:/CS1_R-Intro/div_bng5_only_results/experiments-beamng-ai-wo-minlen-wo-infspeed-lowdiv_random--la311",
                           "C:/CS1_R-Intro/div_bng5_only_results/experiments-beamng-ai-wo-minlen-wo-infspeed-lowdiv_random--la222",
                           "C:/CS1_R-Intro/div_bng5_only_results/experiments-beamng-ai-wo-minlen-wo-infspeed-lowdiv_random--la711",
                           "C:/CS1_R-Intro/div_bng5_only_results/experiments-beamng-ai-wo-minlen-wo-infspeed-lowdiv_random--la84",
                           "C:/CS1_R-Intro/div_bng5_only_results/experiments-beamng-ai-wo-minlen-wo-infspeed-lowdiv_random--la918"
  )
  paths_hidiv_nonobe <- c("C:/CS1_R-Intro/div_bng5_only_results/experiments-beamng-ai-wo-minlen-wo-infspeed-highdiv_random--la311",
                          "C:/CS1_R-Intro/div_bng5_only_results/experiments-beamng-ai-wo-minlen-wo-infspeed-highdiv_random--la222",
                          "C:/CS1_R-Intro/div_bng5_only_results/experiments-beamng-ai-wo-minlen-wo-infspeed-highdiv_random--la711",
                          "C:/CS1_R-Intro/div_bng5_only_results/experiments-beamng-ai-wo-minlen-wo-infspeed-highdiv_random--la84",
                          "C:/CS1_R-Intro/div_bng5_only_results/experiments-beamng-ai-wo-minlen-wo-infspeed-highdiv_random--la918"
  )
  random_suite <- c("C:/CS1_R-Intro/div_bng5_only_results/experiments-beamng-ai-wo-ml-wo-is-rand0",
                       "C:/CS1_R-Intro/div_bng5_only_results/experiments-beamng-ai-wo-ml-wo-is-rand1",
                       "C:/CS1_R-Intro/div_bng5_only_results/experiments-beamng-ai-wo-ml-wo-is-rand2",
                       "C:/CS1_R-Intro/div_bng5_only_results/experiments-beamng-ai-wo-minlen-wo-infspeed--random3",
                       "C:/CS1_R-Intro/div_bng5_only_results/experiments-beamng-ai-wo-minlen-wo-infspeed--random4")
	parent_suite <- "C:/CS1_R-Intro/experiments-beamng-ai-wo-minlen-wo-infspeed-7-steering-4-len-20200821T084856Z-001"
} else if (bng_or_drvr == "drvr"){
  # dummy stuff, remove
  paths_lowdiv_obe <- c("C:/CS1_R-Intro/dummy_adaptive_random_sampling")
  paths_hidiv_obe <- c("C:/CS1_R-Intro/dummy_adaptive_random_sampling",
                       "C:/CS1_R-Intro/dummy_adaptive_random_sampling")
  paths_lowdiv_nonobe <- c("C:/CS1_R-Intro/dummy_adaptive_random_sampling")
  paths_hidiv_nonobe <- c("C:/CS1_R-Intro/dummy_adaptive_random_sampling")
  random_suite <- c("C:/CS1_R-Intro/dummy_adaptive_random_sampling_2")
  parent_suite <- "C:/CS1_R-Intro/driver-ai-wo-minlen-wo-infspeed-7-steering-4-len-20200818T120651Z-001"
}
listed_set_paths <- list("paths_hidiv_obe" = paths_hidiv_obe, 
                         "paths_hidiv_nonobe" = paths_hidiv_nonobe,
                         "paths_lowdiv_obe" = paths_lowdiv_obe, 
                         "paths_lowdiv_nonobe" = paths_lowdiv_nonobe, 
                         "random_sampling" = random_suite)


total_len <- sum(length(paths_lowdiv_obe ), length(paths_lowdiv_nonobe ),
			length(paths_hidiv_obe ), length(paths_hidiv_nonobe))
classes <- rep("", total_len)
names <- rep("", total_len)
# for obe plots
obes <- rep(0, total_len)
obe_covs <- rep(0, total_len)
# for coverage plots
st_cov_cl <- rep(0, total_len)
sp_cov_cl <- rep(0, total_len)
stsp_cov_cl <- rep(0, total_len)
index <- 1

# load the obe data from a single path
get_obe_data <- function(subset_path){
  setwd(subset_path)
  # extract the number of subset obes
  for_each_num_obes <- read.csv("for_each_num_obes.csv" , row.names=1)
  num_obes <- sum(for_each_num_obes)
  # extract the obe coverage
  all_whole_suite_covs <- read.csv("whole_suite_coverages.csv", row.names=1)
  obe_2d_cov <- all_whole_suite_covs["obe_2d",]
  # relative obes and coverage to parent suite
  if (relative_instead_of_absolute_coverages){
    prevwd <- getwd()
    setwd(parent_suite)
    parent_for_each_num_obes <- read.csv("for_each_num_obes.csv" , row.names=1)
    total_num_obes <- sum(parent_for_each_num_obes)
    num_obes <- num_obes/total_num_obes
    parent_all_whole_suite_covs <- read.csv("whole_suite_coverages.csv", row.names=1)
    total_obe_2d_cov <- parent_all_whole_suite_covs["obe_2d",]
    print(total_obe_2d_cov)
    obe_2d_cov <- obe_2d_cov / total_obe_2d_cov
    setwd(prevwd)
  }
  # put in a vector for returning
  vec <- c(num_obes, obe_2d_cov)
  names(vec) <- c("num_obes", "obe_2d_cov")
  return(vec)
}

# execute all paths in a list and fill the vectors
fill_obe_data <- function(path_list, class){
  name_appendix = 1
  for(path in path_list){
    classes[index] <<- class
    names[index] <<- name_appendix  # Todo: maybe replace this with class + appendix
    name_appendix <- name_appendix + 1
    # get obe data
    obe_d <- get_obe_data(path)
    obes[index] <<- obe_d["num_obes"]
    obe_covs[index] <<- obe_d["obe_2d_cov"]
    index <<- index + 1
  }
}

# load all the coverage data from a single path
get_cov_data <- function(subset_path){
  setwd(subset_path)
  all_whole_suite_covs <- read.csv("whole_suite_coverages.csv", row.names=1)
  vec <- all_whole_suite_covs[,]
  names(vec) <- rownames(all_whole_suite_covs)
  # normalize using the parent suite
  if (relative_instead_of_absolute_coverages){
    prevwd <- getwd()
    setwd(parent_suite)
    parent_all_whole_suite_covs <- read.csv("whole_suite_coverages.csv", row.names=1)
    # i do not trust that the order is always the same, therefore not using vectors for division
    for (nme in names(vec)){
      if (parent_all_whole_suite_covs[nme,] != 0){
        vec[nme] <- vec[nme] / parent_all_whole_suite_covs[nme,]
      } else {
        vec[nme] <- 0
      }
      if (vec[nme] > 1 & CHECKS){
        print("There is a problem with the coverages in the sets. Are the right paths selected?")
      }
    }
    setwd(prevwd)
  }
  return(vec)
}

# TODO remove
fill_cov_data <- function(path_list, class){
  name_appendix = 1
  for(path in path_list){
    classes[index] <<- class
    names[index] <<- name_appendix  # Todo: maybe replace this with class + appendix
    name_appendix <- name_appendix + 1
    cov_d <- get_cov_data(path)
    st_cov_cl[index] <<- cov_d["steering_bins_cleanup"]
    sp_cov_cl[index] <<- cov_d["speed_bins_cleanup"]
    stsp_cov_cl[index] <<- cov_d["speed_steering_2d_bins_cleanup"]
    index <<- index + 1
  }
}

get_cov_dframe <- function(covs_vec=covs_of_interest){
  # strip ".csv" if needed
  covs_of_interest <- vec_remove_file_endings(covs_of_interest)
  print(covs_of_interest)
  # determine length of vectors, for all the values
  total_len <- num_dframe_entries()
  # create vectors to fill
  # this may be faster than rep("", len)
  classes <- character(total_len)
  names <- character(total_len)
  variable <- character(total_len)
  value <- numeric(total_len)
  i <- 1
  
  # three loops is not good but who cares
  for (set_conf_name in names(listed_set_paths)){
    # get the path lists, strange implicit conversion to lists
    set_conf <- unlist(listed_set_paths[set_conf_name], use.names=FALSE)
    # simple name for subsets for each configuration
    nme <- 1
    for (s_path in set_conf){
      cov_data <- get_cov_data(s_path)
      for (cov in covs_of_interest){
        classes[i] <- set_conf_name
        names[i] <- nme
        variable[i] <- cov
        value[i] <- cov_data[cov]
        i <- i + 1
      }
      nme <- nme + 1
    }
  }
  
  # create a dataframe of vectors
  df <- data.frame(classes, names, variable, value)
  #print(df)
  return(df)
}


# calculates hav many values there are for all subsets
num_dframe_entries <- function(){
  num_sets <- 0
  for (set in listed_set_paths){
    num_sets <- num_sets + length(set)
  }
  return(num_sets * length(covs_of_interest))
}

# remove the file ending from a vector of names
# by default used to strip ".csv"
vec_remove_file_endings <- function(names_vec, file_ending=".csv"){
  new_vec <- rep("", length(names_vec))
  ending_len <- nchar(file_ending)
  i <- 1
  for (nm in names_vec){
    ending_start <- max(1, (nchar(nm)-ending_len+1))
    len_nm <- nchar(nm)
    pos <- ending_start:len_nm
    if (substr(nm, ending_start, len_nm) == file_ending){
      new_vec[i] <- substring(nm, 1, ending_start-1)
    } else {
      new_vec[i] <- nm
    }
    
    i <- i + 1
  }
  return(new_vec)
}

fill_cov_data_legacy <- function(path_list, class){
  name_appendix = 1
  for(path in path_list){
    classes[index] <<- class
    names[index] <<- name_appendix  # Todo: maybe replace this with class + appendix
    name_appendix <- name_appendix + 1
    cov_d <- get_cov_data(path)
    st_cov_cl[index] <<- cov_d["steering_bins_cleanup"]
    sp_cov_cl[index] <<- cov_d["speed_bins_cleanup"]
    stsp_cov_cl[index] <<- cov_d["steering_bins_cleanup"]
    index <<- index + 1
  }
}

# reorders dataframe, gets a classes-names-variable-values-dataframe
# metrics columns, configurations rows
# a lot of stupid and ugly stuff in this one but I guess it works
reorder_dframe_for_table <- function(cnvv_dframe){
  df1 <- data.frame()
  
  # create two initial columns, one for type of sampling, one for obe or nonobe, needed for latex table
  type_of_sampling <- character(length(listed_set_paths))
  obe_or_not <- character(length(listed_set_paths))
  j <- 1
  for (covi in names(listed_set_paths)){
    # grepl is contains for strings
    if (grepl("nonobe", covi, fixed=TRUE)){
      # check if prev is the same, not a clean solution, but works for now
      typeos <- substr(covi, 1, nchar(covi)-7)
      if (type_of_sampling[j-1] == typeos){
        type_of_sampling[j] <- ""
      } else {
        type_of_sampling[j] <- typeos
      }
      obe_or_not[j] <- "non_OBE"
    } else if (grepl("obe", covi, fixed=TRUE)){
      type_of_sampling[j] <- substr(covi, 1, nchar(covi)-4)
      obe_or_not[j] <- "OBE"
    } else {
      type_of_sampling[j] <- covi 
      obe_or_not[j] <- ""
    }
    j <- j + 1
  }
  df1 <- data.frame(type_of_sampling, obe_or_not)
  # maybe stupid stuff that should be removed
  colnames(df1) <- c("", " ")
  
  # create values for avg and std div for each metric and set
  covs_of_interest <- vec_remove_file_endings(covs_of_interest)  
  for (covi in covs_of_interest){
    # for saving one metric all sets as str "avg (std div)"
    vec <- character(length(listed_set_paths))
    i <- 1
    # subset using only one metric
    subs_onemetr <- cnvv_dframe[cnvv_dframe$variable == covi, ]
    for (conf_name in names(listed_set_paths)){
      vals <- subs_onemetr$value[subs_onemetr$classes == conf_name]
      avgi <- round(mean(vals), 3)
      sdi <- round(sd(vals), 3)
      table_entry <- paste(toString(avgi), " (", toString(sdi), ")", sep="")
      vec[i] <- table_entry
      i <- i + 1
    }
    if (ncol(df1) > 0){
      df1[covi] <- vec
    } else {
      df1 <- data.frame(vec)
      colnames(df1) <- covi
    }
  }
  rownames(df1) <- names(listed_set_paths)
  return(df1)
}

if (obes_or_covs == "obes"){
	fill_obe_data(paths_lowdiv_obe, "lowdiv_obe")
  fill_obe_data(paths_hidiv_obe, "highdiv_obe")
  fill_obe_data(paths_lowdiv_nonobe, "lowdiv_nonobe")
  fill_obe_data(paths_hidiv_nonobe, "highdiv_nonobe")
  
  dframe_obe <- data.frame(
    classes,
    names,
    obes,
    obe_covs
  )
  # melt obe and obe covs into one
  mdata <- melt(dframe_obe, id=c("classes", "names"))
  br_plt <- ggplot(data=dframe_obe, aes(x=classes, y=obes, fill=names)) +
    geom_bar(stat="identity", color="black", position=position_dodge())+
    theme_minimal()
  #br_plt <- ggplot(data=mdata, aes(x=variable, y=value, group=classes, color=classes, alpha=names)) +
  #  geom_col(stat="identity", color="black", position=position_dodge())+
  #  theme_minimal()
  #br_plt <- ggplot(data=mdata, aes(x=variable, y=value, fill=classes)) +
  #  barplot(stat="identity", color="black", position="stack")+
  #  theme_minimal()
  # is taking the mean a viable approach?
  # or should there be one subbplot for each subset?
  br_plt <- ggplot(data=mdata, aes(x=variable, y=value, fill=classes)) +
    geom_bar(stat="summary", color="black", position=position_dodge(), fun="mean")+
    theme_minimal()
  br_plt
} else if (obes_or_covs == "covs"){
  # TODO refactor this 
  #fill_cov_data(paths_lowdiv_obe, "lowdiv_obe")
  #fill_cov_data(paths_hidiv_obe, "highdiv_obe")
  #fill_cov_data(paths_lowdiv_nonobe, "lowdiv_nonobe")
  #fill_cov_data(paths_hidiv_nonobe, "highdiv_nonobe")
  #
  #dframe_cov <- data.frame(
  #  classes,
  #  names,
  #  st_cov_cl,
  #  sp_cov_cl,
  #  stsp_cov_cl
  #)
  
  df <- get_cov_dframe()
  df_table <- reorder_dframe_for_table(df)
  #mdata <- melt(dframe_cov, id=c("classes", "names"))
  #mdata <- df
  br_plt <- ggplot(data=df, aes(x=variable, y=value, fill=classes)) +
    geom_bar(stat="summary", color="black", position=position_dodge(), fun="mean")+
    theme_minimal()
  br_plt
  
  hlines <- c(-1, 0, 2, 4, nrow(df_table))
  print(xtable(df_table), booktabs=TRUE, include.rownames=FALSE, hline.after=hlines)
}
