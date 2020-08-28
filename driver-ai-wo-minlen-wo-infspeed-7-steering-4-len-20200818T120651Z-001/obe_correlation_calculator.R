# compute correlation between test obe behavior to similarity

# path to tests
#setwd("C:/CS1_R-Intro/experiments-beamng-ai-wo-minlen-wo-infspeed-7-steering-4-len-20200821T084856Z-001")
# setwd("C:/CS1_R-Intro/driver-ai-wo-minlen-wo-infspeed-7-steering-4-len-20200818T120651Z-001")
setwd("C:/CS1_R-Intro/driver-ai-wo-minlen-wo-infspeed-7-steering-4-len-20200818T120651Z-001")

for_each_num_obes <- read.csv("for_each_num_obes.csv")

tests_that_fail <- for_each_num_obes[for_each_num_obes$num_obes == 1, ]

#input_names_to_load <- list("jaccard.csv" = NA, "sdl_2d_dist.csv" = NA,
#					"sdl_2d_k_lcstr_dist.csv" = NA,
#					"curve_sdl_dist.csv" = NA,
#					"cur_sdl_k_lcstr_dist.csv" = NA,
#					"cur_sdl_lcs_dist.csv" = NA)
input_names_to_load <- list ("cur_sdl_lcs_dist.csv" = NA,
					"cur_sdl_lcstr_dist.csv" = NA,
					"cur_sdl_1_lcstr_dist.csv" = NA,
					"cur_sdl_2_lcstr_dist.csv" = NA,
					"cur_sdl_3_lcstr_dist.csv" = NA,
					"cur_sdl_5_lcstr_dist.csv" = NA,
					"sdl_2d_lcs_dist.csv" = NA,
					"sdl_2d_lcstr_dist.csv" = NA,
					"sdl_2d_1_lcstr_dist.csv" = NA,
					"sdl_2d_2_lcstr_dist.csv" = NA,
					"sdl_2d_3_lcstr_dist.csv" = NA,
					"sdl_2d_5_lcstr_dist.csv" = NA)


# Load all the csvs for the names 
for (name in names(input_names_to_load)){	
	c <- read.csv(name, check.names=FALSE)
	print(name)	
	input_names_to_load[[name]] = c
}

# create an vector for the box plots
avgs <- rep(0, length(input_names_to_load))
names(avgs) <- names(input_names_to_load)
new_names <- names(input_names_to_load)

i <- 1
for (in_dist_name in names(input_names_to_load)){
	print(in_dist_name)
	in_dist = input_names_to_load[[in_dist_name]]
	



	#jaccard_matrix = read.csv("jaccard.csv", check.names=FALSE)

	names_of_tests_that_fail = tests_that_fail$test
	sum_cors <- 0
	sum_p_vals <- 0
	for (name in names_of_tests_that_fail){
		distances <- in_dist[[name]]
		#print(class(jaccard_matrix[[name]]))
		num_obes <- for_each_num_obes[["num_obes"]]
		cor_output <- cor.test(distances, num_obes, method="spearman")
		
		estimated_rho = as.numeric(cor_output[["estimate"]])
		sum_cors <- sum_cors + estimated_rho
		#print(cor_output)
		p_val = cor_output[["p.value"]]
		#print(p_val)
		sum_p_vals <- sum_p_vals + p_val
	}
	# TODO does the - make sense?
	avg_current <- -sum_cors/as.numeric(length(names_of_tests_that_fail))
	avgs[in_dist_name] <- avg_current
	print(paste("Average obe correlation for ", in_dist_name, avg_current))
	avg_p_val <-  sum_p_vals/as.numeric(length(names_of_tests_that_fail))
	print(paste("Average p value for ", in_dist_name, avg_p_val))
	new_names[i] <- paste(new_names[i], " ", round(avg_p_val, 3))
	i <- i + 1
}

names(avgs) <- new_names
avgs
# adjusts margin to fit the whole names
op <- par(mar=c(13,4,4,2))
barplot(avgs, las=2)   # , main="BeamNGAI OBE correlation")   # add title
remove(op)                  

# stupid way of commenting, but presented in the course
if(FALSE){
jaccard_matrix = read.csv("jaccard.csv", check.names=FALSE)

names_of_tests_that_fail = tests_that_fail$test
sum_cors <- 0
for (name in names_of_tests_that_fail){
	distances <- jaccard_matrix[[name]]
	print(class(jaccard_matrix[[name]]))
	num_obes <- for_each_num_obes[["num_obes"]]
	cor_output <- cor.test(distances, num_obes, method="spearman")
	
	estimated_rho = as.numeric(cor_output[["estimate"]])
	sum_cors <- sum_cors + estimated_rho
	print(cor_output)

}
print(paste("Average correlation ", 
		sum_cors/as.numeric(length(names_of_tests_that_fail))))
}