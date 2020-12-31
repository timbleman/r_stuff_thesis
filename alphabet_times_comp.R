# Bar plots for alphabet length compare times
# Values collected outside R, nothing to adjust

library(ggplot2)
library(hrbrthemes)

Jaccard_comp_times <- c("28" = 4.79, "44" = 5.34, "60" = 6.07, " "=0)
SDL_2D_comp_times <- c("28" = 55.34, "44" = 57.25, "60" = 62.59, " "=0)
CUR_SDL_comp_times <- c("7" = 26.93, "11" = 28.71, "15" =31.04)


vec <- c(Jaccard_comp_times, SDL_2D_comp_times, CUR_SDL_comp_times)

# ugly and hacky
first_col <- "#FEEB65"
second_col <- "#E4521B"
third_col <- "#4D342F"
cols <- c(first_col, second_col, third_col, "#00FF99", 
          first_col, second_col, third_col, "#00FF99",
          first_col, second_col, third_col, "#00FF99")
# ugly and hacky
names = "Jaccard                  sw_sdl_2d               sw_cur_sdl"

par(mar=c(5,5,4,1)+.1)  # margins bottom, left, top, right

font_mult <- 2

# what does this one do: "border=par(lty=0)," ? Produces only a warning...
barplot(vec, ylab="Time in seconds", col=cols, xlab=names, 
		cex.axis = font_mult, cex.lab = font_mult, cex.main = font_mult,
		cex.names = font_mult)
