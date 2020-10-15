# plots coverages and obes of adaptive random sampling subsets
library(ggplot2)
library(reshape2)

bng_or_drvr = "bng"
obes_or_covs = "covs"

if(bng_or_drvr == "bng"){
	paths_lowdiv_obe <- c("C:/CS1_R-Intro/dummy_adaptive_random_sampling")
	paths_hidiv_obe <- c("C:/CS1_R-Intro/dummy_adaptive_random_sampling",
	                     "C:/CS1_R-Intro/dummy_adaptive_random_sampling")
	paths_lowdiv_nonobe <- c("C:/CS1_R-Intro/dummy_adaptive_random_sampling")
	paths_hidiv_nonobe <- c("C:/CS1_R-Intro/dummy_adaptive_random_sampling")
} else if (bng_or_drvr == "drvr"){
}
# dummy stuff, remove
paths_lowdiv_obe <- c("C:/CS1_R-Intro/dummy_adaptive_random_sampling")
paths_hidiv_obe <- c("C:/CS1_R-Intro/dummy_adaptive_random_sampling",
                     "C:/CS1_R-Intro/dummy_adaptive_random_sampling")
paths_lowdiv_nonobe <- c("C:/CS1_R-Intro/dummy_adaptive_random_sampling")
paths_hidiv_nonobe <- c("C:/CS1_R-Intro/dummy_adaptive_random_sampling")



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
  # extract the number of subset obes
  for_each_num_obes <- read.csv("for_each_num_obes.csv" , row.names=1)
  num_obes <- sum(for_each_num_obes)
  # extract the obe coverage
  all_whole_suite_covs <- read.csv("whole_suite_coverages.csv", row.names=1)
  obe_2d_cov <- all_whole_suite_covs["obe_2d",]
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
  all_whole_suite_covs <- read.csv("whole_suite_coverages.csv", row.names=1)
  vec <- all_whole_suite_covs[,]
  names(vec) <- rownames(all_whole_suite_covs)
  return(vec)
}

fill_cov_data <- function(path_list, class){
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
  fill_cov_data(paths_lowdiv_obe, "lowdiv_obe")
  fill_cov_data(paths_hidiv_obe, "highdiv_obe")
  fill_cov_data(paths_lowdiv_nonobe, "lowdiv_nonobe")
  fill_cov_data(paths_hidiv_nonobe, "highdiv_nonobe")
  
  dframe_cov <- data.frame(
    classes,
    names,
    st_cov_cl,
    sp_cov_cl,
    stsp_cov_cl
  )
  mdata <- melt(dframe_cov, id=c("classes", "names"))
  br_plt <- ggplot(data=mdata, aes(x=variable, y=value, fill=classes)) +
    geom_bar(stat="summary", color="black", position=position_dodge(), fun="mean")+
    theme_minimal()
  br_plt
}
