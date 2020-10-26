# bar plots for alphabet length compare times

library(ggplot2)
library(hrbrthemes)

Jaccard_comp_times <- c("28" = 4.79, "44" = 5.34, "60" = 6.07, " "=0)
SDL_2D_comp_times <- c("28" = 55.34, "44" = 57.25, "60" = 62.59, " "=0)
CUR_SDL_comp_times <- c("7" = 26.93, "11" = 28.71, "15" =31.04)

#df <- data.frame(Jaccard_comp_times, SDL_2D_comp_times, CUR_SDL_comp_times)
#names(df) <- c("jaccard_28", "jaccard_44", "jaccard_60")

vec <- c(Jaccard_comp_times, SDL_2D_comp_times, CUR_SDL_comp_times)
#vec <- c("jaccard_28" = Jaccard_comp_times,
#		"jaccard_44" = SDL_2D_comp_times, 
#		"jaccard_60"=CUR_SDL_comp_times)

# ugly and hacky, lol
first_col <- "#FEEB65"
second_col <- "#E4521B"
third_col <- "#4D342F"
cols <- c(first_col, second_col, third_col, "#00FF99", 
          first_col, second_col, third_col, "#00FF99",
          first_col, second_col, third_col, "#00FF99")
# ugly and hacky, lol
names = "Jaccard                     Sliding window SDL_2D        Sliding window CUR_SDL"

barplot(vec, ylab="time in seconds", col=cols, xlab=names, border=par(lty=0))