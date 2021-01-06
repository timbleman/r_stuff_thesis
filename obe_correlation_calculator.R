# Compute correlation between test obe behavior to similarity
# Things to adjust:
# setwd() to the right suite, uncomment BeamNG.AI or DriverAI
# Add (input) metrics to input_names_to_load, comment and uncomment ones

# path to tests
# BeamNG.AI
setwd("C:/CS1_R-Intro/experiments-beamng-ai-wo-minlen-wo-infspeed-7-steering-4-len-20200821T084856Z-001")
# DriverAI
#setwd("C:/CS1_R-Intro/driver-ai-wo-minlen-wo-infspeed-7-steering-4-len-20200818T120651Z-001")

# comment or uncomment these for desired metrics
input_names_to_load <- list("jaccard_28alph.csv" = NA, 
					"jaccard_44alph.csv" = NA,
					"jaccard_88alph.csv" = NA,
					"jaccard_60alph.csv" = NA)
input_names_to_load <- list("sdl2d_sw_28alph.csv" = NA, 
                            "sdl2d_sw_44alph.csv" = NA,
                            "sdl2d_sw_88alph.csv" = NA,
                            "sdl2d_sw_60alph.csv" = NA,
                            "sdl2d_sw_120alph.csv" = NA)
input_names_to_load <- list("cursdl_sw_7ang.csv" = NA, 
                            "cursdl_sw_11ang.csv" = NA,
                            "cursdl_sw_15ang.csv" = NA)
input_names_to_load <- list ("cursdl_lcs_7ang.csv" = NA,
					"cursdl_lcstr_7ang.csv" = NA,
					"cursdl_1_lcstr_7ang.csv" = NA,
					"cursdl_3_lcstr_7ang.csv" = NA,
					"cursdl_5_lcstr_7ang.csv" = NA)

PLOTTING = TRUE

# Get tests that fail
for_each_num_obes <- read.csv("for_each_num_obes.csv")
tests_that_fail <- for_each_num_obes[for_each_num_obes$num_obes == 1, ]

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
	
	names_of_tests_that_fail = tests_that_fail$test
	sum_cors <- 0
	sum_p_vals <- 0
	for (name in names_of_tests_that_fail){
		distances <- in_dist[[name]]
		num_obes <- for_each_num_obes[["num_obes"]]
		cor_output <- cor.test(distances, num_obes, method="spearman")
		
		estimated_rho = as.numeric(cor_output[["estimate"]])
		sum_cors <- sum_cors + estimated_rho
		p_val = cor_output[["p.value"]]
		sum_p_vals <- sum_p_vals + p_val
	}
	avg_current <- sum_cors/as.numeric(length(names_of_tests_that_fail))
	avgs[in_dist_name] <- avg_current
	print(paste("Average obe correlation for ", in_dist_name, avg_current))
	avg_p_val <-  sum_p_vals/as.numeric(length(names_of_tests_that_fail))
	print(paste("Average p value for ", in_dist_name, avg_p_val))
	new_names[i] <- paste(new_names[i], " ", round(avg_p_val, 3))
	i <- i + 1
}

# jitter makes everthing a bit more readable, many dont have near tests
# plot(jitter(distances, factor=0.1), jitter(for_each_num_obes[,"num_obes"], factor=0.1))

names(avgs) <- new_names
avgs
if (PLOTTING){
  # adjusts margin to fit the whole names
  op <- par(mar=c(13,4,4,2))
  barplot(avgs, las=2)   # , main="BeamNGAI OBE correlation")   # add title
  remove(op)                  
}

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