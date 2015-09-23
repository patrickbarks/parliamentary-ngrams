
# load libraries
library(readr)
library(plyr)
library(ggplot2)
library(grid)
library(scales)


# read data
dat <- read_csv("../data/parl-41-processed.txt")


# format dates
dat$date <- as.Date(dat$date, format="%d-%m-%Y")


# function to get the percent occurence of a given search term
# returns the percentage of turns-of-talk containing the search term,
#  and the search term itself
PercentOccur <- function(x, search_term) {
  pct <- length(grep(search_term, x$speech)) / length(x$speech) * 100
  return(data.frame(pct, search_term))
}


# function to smooth relationship between percent occurrence and time, via loess
SmoothFn <- function(x, span = 0.15) {
  x$date_num <- as.numeric(factor(x$date))
  x$smooth <- predict(loess(pct ~ date_num, data = x, span = span))
  x$smooth[x$smooth < 0] <- 0
  return(x)
}


# apply PercentOccur by date, for three different search terms
out1 <- ddply(dat, .(date), .fun = PercentOccur, search_term = "F-35")
out2 <- ddply(dat, .(date), .fun = PercentOccur, search_term = "veteran")
out3 <- ddply(dat, .(date), .fun = PercentOccur, search_term = "Duffy")


# apply SmoothFn
out1_s <- SmoothFn(out1)
out2_s <- SmoothFn(out2)
out3_s <- SmoothFn(out3)


# combine data for all three search terms into single df
out <- rbind(out1_s, out2_s, out3_s)


# x-labels and indices
yrs <- c("2012", "2013", "2014", "2015")
indices_yrs <- sapply(yrs, function (x) min(grep(x, format(out$date, "%Y"))))


# plot
png("../figures/n-grams.png", height = 5, width = 9, units = "in", res = 300)
ggplot(out, aes(x = date_num, y = smooth)) +
  geom_line(aes(color = search_term), size = 2) +
  scale_x_continuous(breaks = indices_yrs, labels = yrs) +
  scale_y_continuous(labels = function (x) paste(x, "%", sep = "")) +
  expand_limits(y=0) +
  xlab(NULL) +
  ylab("Frequency of mentions") +
  theme(plot.title = element_text(size = 14, vjust = 1.2, face = "bold"),
        axis.title.x = element_text(size = 17, vjust = -0.3),
        axis.title.y = element_text(size = 17, vjust = 2.3),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 11),
        plot.margin = unit(c(2, 2, 5, 5), "mm"))
dev.off()
