# compute mantel distances for various input and output
library(vegan)

# path to tests
#setwd("C:/CS1_R-Intro/driver-ai-wo-minlen-wo-infspeed-7-steering-4-len-20200818T120651Z-001")
# without OBEs
setwd("C:/CS1_R-Intro/experiments-driver-ai-no-obe-wo-minlen-wo-infspeed")
setwd("C:/CS1_R-Intro/experiments-beamng-ai-no-obe-wo-minlen-wo-infspeed")

input_names_to_load <- list("jaccard_11ang_4len.csv" = NA,
					"sdl2d_sw_11ang_4len.csv" = NA)

for (name in names(input_names_to_load)){	
	c <- read.csv(name)
	print(name)	
	input_names_to_load[[name]] = c
}

#input_names_to_load


output_names_to_load <- list("steering_speed_adj_bin.csv" = NA,
                             "steering_speed_adj_sin.csv" = NA,
                             "steering_speed_bin.csv" = NA,
                             "steering_speed_sin.csv" = NA, 
                             "steering_adj_bin.csv" = NA,
                             "steering_adj_sin.csv" = NA) 

for (name in names(output_names_to_load)){	
	c <- read.csv(name)
	print(name)	
	output_names_to_load[[name]] = c
}

#output_names_to_load

# all to all mantel dists and saving in lists
mantel_vals <- list(rep(NA, length(output_names_to_load)))
# setNames(mantel_vals, list(names(output_names_to_load)))  # does also not work
#names(mantel_vals) <- list(names(output_names_to_load))
#mantel_vals[["speering_speed_dist.csv"]]
j <- 1
for (out_dist_name in names(output_names_to_load)){
	out_dist = output_names_to_load[[out_dist_name]]
	compare_list <- list(rep(NA, length(input_names_to_load)))

	i <- 1
	for (in_dist_name in names(input_names_to_load)){
		in_dist = input_names_to_load[[in_dist_name]]
		man <- mantel(in_dist[,2:length(in_dist)], out_dist[,2:length(in_dist)])

		name_of_tup <- paste(in_dist_name, " vs ", in_dist_name)
		tup <- c("signif" = man[["signif"]], "statistic" = man[["statistic"]])

		compare_list[[i]] <- tup
		i <- i + 1 
	}

	# TODO check das
	names(compare_list) <- list(names(input_names_to_load))[[1]]

	mantel_vals[[j]] = compare_list
	j <- j + 1
}

# TODO check das
names(mantel_vals) <- list(names(output_names_to_load))[[1]]

mantel_vals




