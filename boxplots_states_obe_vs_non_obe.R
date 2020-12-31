# Box plots of num states for obe and non obe test cases
# choose a test suite
setwd("C:/CS1_R-Intro/driver-ai-wo-minlen-wo-infspeed-7-steering-4-len-20200818T120651Z-001")
setwd("C:/CS1_R-Intro/experiments-beamng-ai-wo-minlen-wo-infspeed-7-steering-4-len-20200821T084856Z-001")

# Sample to speed up the process, not needed
sample_instead_of_all = FALSE
sample_size <- 18

for_each_num_obes <- read.csv("for_each_num_obes.csv" , row.names=1)
tests_that_fail <- row.names(for_each_num_obes)[for_each_num_obes$num_obes == 1]
tests_that_do_not_fail <- row.names(for_each_num_obes)[for_each_num_obes$num_obes == 0]

max_sample_obe <- sample(tests_that_fail, sample_size)
max_sample_non_obe <- sample(tests_that_do_not_fail, sample_size)

all_num_states <- read.csv("for_each_num_states.csv", check.names=FALSE, row.names=1)

if (sample_instead_of_all){
	num_obe_states <- all_num_states[max_sample_obe,]
	num_non_obe_states <- all_num_states[max_sample_non_obe,]
} else {
	num_obe_states <- all_num_states[tests_that_fail,]
	num_non_obe_states <- all_num_states[tests_that_do_not_fail,]
}
names <- c("OBE tests", "non OBE tests")

font_mult = 1.5
boxplot(num_obe_states, num_non_obe_states, names=names, xlab="Test outcome", 
        ylab="Number of states", cex.axis=font_mult, cex.lab=font_mult)
