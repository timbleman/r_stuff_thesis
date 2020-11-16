# plot for the broken tests in both datasets

setwd("C:/CS1_R-Intro/")

library(ggplot2)
library(hrbrthemes)


BROKENSPEED = "broken_speed"
MINLEN = "under_minimum_length"
VALID = "valid_execution"
OTHER = "other"

d_sets <- c(rep("BeamNG", 169), rep("DriverAI", 466))
bng_tests <- c(rep(VALID, 115), rep(MINLEN, 54))
drvr_tests <- c(rep(VALID, 291), rep(MINLEN, 131), rep(BROKENSPEED, 41), rep(OTHER, 3))
class = c(bng_tests, drvr_tests)
count <- rep(1, 635)

dframe_broken_execs <- data.frame(d_sets, class, count)

cols <- c(valid_execution="#66CC00", broken_speed="brown3", 
		other="cadetblue3", under_minimum_length="#FFFF33")

font_size <- 18
ggplot(dframe_broken_execs, aes(fill=class, x=d_sets, y=count)) +
	geom_bar(position="stack", stat="identity") + 
	ggtitle("Classes of executions") + 
	theme_ipsum(axis_text_size=font_size, axis_title_size=font_size) + 
	xlab("Datasets") + 
	ylab("Test executions") + 
	scale_fill_manual(values = cols) + 
	theme(text = element_text(size=font_size)) # scale the legend