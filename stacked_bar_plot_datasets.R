setwd("C:/CS1_R-Intro/)

library(ggplot2)
library(hrbrthemes)

# plot for the number of OBEs in BeamNG/DriverAI Datasets

OBE = "OBE"
NONOBE = "non_OBE"

d_sets <- c(rep("BeamNG full", 169), rep("BeamNG trimmed", 115), 
		rep("DriverAI full", 466), rep("DriverAI trimmed", 291))
obe_vs_non_BNG_full <- c(rep(OBE, 83), rep(NONOBE, 86))
obe_vs_non_BNG_trimmed <- c(rep(OBE, 56), rep(NONOBE, 59))
obe_vs_non_DRVR_full <- c(rep(OBE, 28), rep(NONOBE, 438))
obe_vs_non_DRVR_trimmed <- c(rep(OBE, 18), rep(NONOBE, 273))
obe <- c(obe_vs_non_BNG_full, obe_vs_non_BNG_trimmed, obe_vs_non_DRVR_full,
		obe_vs_non_DRVR_trimmed)

count <- rep(1, 1041)
dframe_trimmed_vs_not <- data.frame(d_sets, obe, count)

cols <- c(OBE="brown3", non_OBE="cadetblue3")

ggplot(dframe_trimmed_vs_not, aes(fill=obe, x=d_sets, y=count)) +
	geom_bar(position="stack", stat="identity") + 
	ggtitle("OBE distribution in the datasets") + 
	theme_ipsum(axis_text_size=10, axis_title_size = 11) + 
	xlab("datasets") + 
	ylab("test executions") + 
	scale_fill_manual(values = cols)