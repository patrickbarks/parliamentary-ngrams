
# load libraries
library(readr)
library(plyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(scales)


# read data
dat <- read_csv("../data/parl-41-processed.txt")


# convert common spelled out dollar-values to $-suffix notation
dat$speech <- gsub("quarter of a million dollars", "$0.25 million", dat$speech)
dat$speech <- gsub("half a million dollars", "$0.5 million", dat$speech)
dat$speech <- gsub("three-quarters of a million dollars", "$0.75 million", dat$speech)
dat$speech <- gsub(" a million dollars", " $1 million", dat$speech)

dat$speech <- gsub("quarter of a billion dollars", "$0.25 billion", dat$speech)
dat$speech <- gsub("half a billion dollars", "$0.5 billion", dat$speech)
dat$speech <- gsub("three-quarters of a billion dollars", "$0.75 billion", dat$speech)
dat$speech <- gsub(" a billion dollars", " $1 billion", dat$speech)

dat$speech <- gsub("quarter of a trillion dollars", "$0.25 trillion", dat$speech)
dat$speech <- gsub("half a trillion dollars", "$0.5 trillion", dat$speech)
dat$speech <- gsub("three-quarters of a trillion dollars", "$0.75 trillion", dat$speech)
dat$speech <- gsub(" a trillion dollars", " $1 trillion", dat$speech)


## functions to exract dollar-value mentions
Tokenize <- function(string) {
# tokenize a string of text (split into vector of individual 'words')
  unlist(strsplit(string, "\\s+|\\–|\\—|\\.\\.+|\\-\\-+|\\…|\\-"), use.names=F)
}

GetDollarVals <- function(i, token_vec) {
# find dollar-values within vector of tokens
# dollar values may or may not have a spelled-out suffix (e.g. '$1.5 million')
# arguments are a dollar-value index (i) and a vector of tokens (token_vec)
  if (i == length(token_vec)) return(token_vec[i])
  next_term <- token_vec[i+1]  	
  suffixes <- "hundred|thousand|million|billion|trillion"
  is_next_term_suffix <- as.logical(length(grep(suffixes, next_term)))
  dollar_val <- ifelse(is_next_term_suffix, paste(token_vec[i], next_term), token_vec[i])
  return(dollar_val)
}

GetDollarValsWrapper <- function(statement) {
# wrapper function to apply Tokenize and GetDollarVals to a given statement (speech by MP)
  token_vec <- unlist(sapply(statement, Tokenize, USE.NAMES=F))
  dollar_indices <- grep("\\$", token_vec)
  dollar_vals_full <- sapply(dollar_indices, GetDollarVals, token_vec = token_vec)
  return(dollar_vals_full)
}


# get indices of dat corresponding to the three main parties
indices_cpc <- which(dat$party == "CPC")
indices_ndp <- which(dat$party == "NDP")
indices_lib <- which(dat$party == "Lib.")


# exract dollar-value mentions by party
dollar_vals_cpc <- unlist(lapply(dat$speech[indices_cpc], GetDollarValsWrapper))
dollar_vals_ndp <- unlist(lapply(dat$speech[indices_ndp], GetDollarValsWrapper))
dollar_vals_lib <- unlist(lapply(dat$speech[indices_lib], GetDollarValsWrapper))


# combine dollar-value mentions into single df
df_cpc <- data.frame(party = "Conservative", vals = dollar_vals_cpc)
df_ndp <- data.frame(party = "NDP", vals = dollar_vals_ndp)
df_lib <- data.frame(party = "Liberal", vals = dollar_vals_lib)
df <- rbind(df_cpc, df_ndp, df_lib)


# clean up dollar-values
df$vals <- gsub("\\$\\.5", "\\$0\\.5", df$vals)                  # change '.5' to '0.5'
df$vals <- gsub("CAN\\$", "", df$vals)                           # remove 'CAN$' prefix
df$vals <- gsub("\\,|^[[:punct:]]+|[[:punct:]]+$", "", df$vals)  # remove punct except decimals
df$vals <- gsub("K", " K", df$vals)                              # add space before 'K' suffix


# function to convert dollar-values to numeric form
ToNumeric <- function(dollar_val) {
  terms <- unlist(strsplit(dollar_val, "\\s"))	
  dollar_val_numeric <- as.numeric(terms[1])
  if (length(terms) == 1) return(dollar_val_numeric)
  dollar_val_numeric <- switch(terms[2],
                               "hundred"  = dollar_val_numeric * 100,
                               "thousand" = dollar_val_numeric * 1000,
                               "K"        = dollar_val_numeric * 1000,
                               "million"  = dollar_val_numeric * 1000000,
                               "billion"  = dollar_val_numeric * 1000000000,
                               "trillion" = dollar_val_numeric * 1000000000000)
  return(as.numeric(dollar_val_numeric))
}


# convert dollar-values to numeric form
df$vals <- sapply(df$vals, ToNumeric, USE.NAMES = F)


# top-mentions tables
sort(table(subset(df, party=="Conservative")$vals), decreasing = T)[1:4]
sort(table(subset(df, party=="NDP")$vals), decreasing = T)[1:4]
sort(table(subset(df, party=="Liberal")$vals), decreasing = T)[1:4]
top_mentions <- data.frame(party = c("Conservative", "NDP", "Liberal"),
                           rank = c("1.\n2.\n3.\n4."),
                           vals = c("$21B\n$1B\n$1K\n$2.7M",
                                    "$90K\n$50M\n$1B\n$3.1B",
                                    "$90K\n$2B\n$1B\n$200M"),
                           mentions = c("(142)\n(113)\n(112)\n(110)",
                                        "(124)\n(72)\n(68)\n(66)",
                                        "(104)\n(47)\n(38)\n(29)"))


# plotting
labs <- c("1", "10", "100", "1K", "10K", "100K", "1M", "10M", "100M", "1B", "10B",
          "100B", "1Tr", "10Tr")

png("../figures/dollar-mentions.png", height = 6, width = 6.75, units = "in", res = 300)
ggplot(df, aes(x = vals, fill = party)) +
  geom_density(adjust = 0.2) +
  facet_wrap(~ party, nrow = 3) +
  scale_fill_manual(values = c("dodgerblue", "orange", "red"), guide = F) +
  xlab("Dollar-value ($)") +
  ylab("Density of mentions") +
  ggtitle("Dollar-value mentions during Question Period, 41st Parliament") +
  scale_x_log10(limits = c(1, 10e12), breaks = 10^(0:13), labels = labs,
                expand = c(0.03, 0.03)) +
  theme(plot.title = element_text(size = 14, vjust = 1.2, face = "bold"),
        axis.title.x = element_text(size = 14, vjust = -0.3),
        axis.title.y = element_text(size = 14, vjust = 1.4),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 10),
        strip.text.x = element_text(size = 11)) +
  geom_rect(aes(ymin = 0.12, ymax = 0.39, xmin = 1.15, xmax = 300), fill = "grey94",
            color = "grey82", size=0.2) +
  annotate("text", x = 1.37, y = 0.362, label = "Top mentions", size = 3.3, hjust = 0,
           fontface = "bold", color = "grey30") +
  geom_text(data = top_mentions, aes(x = 2.5, y = 0.23, label = rank), size = 3.2,
            hjust = 1, color = "grey30") +
  geom_text(data = top_mentions, aes(x = 30, y = 0.23, label = vals), size = 3.2,
            hjust = 1, color = "grey30") +
  geom_text(data = top_mentions, aes(x = 230, y = 0.23, label = mentions), size = 3.2,
            hjust = 1, color = "grey30")
dev.off()


# double-check top mentions
GregexprCheck <- function(indices, regex) {
  length(which(unlist(gregexpr(regex, dat$speech[indices], perl = T)) != -1))	
}

GregexprCheck(indices_cpc, "\\$21[-\\s]billion")
GregexprCheck(indices_cpc, "\\$1[-\\s]billion")
GregexprCheck(indices_cpc, "\\$1,000")
GregexprCheck(indices_cpc, "\\$2\\.7[-\\s]million")

GregexprCheck(indices_ndp, "\\$90,000")
GregexprCheck(indices_ndp, "\\$50[-\\s]million")
GregexprCheck(indices_ndp, "\\$1[-\\s]billion")
GregexprCheck(indices_ndp, "\\$3\\.1[-\\s]billion")

GregexprCheck(indices_lib, "\\$90,000")
GregexprCheck(indices_lib, "\\$2[-\\s]billion")
GregexprCheck(indices_lib, "\\$1[-\\s]billion")
GregexprCheck(indices_lib, "\\$200[-\\s]million")


# analogies for $90K
GregexprCheck(1:nrow(dat), "\\$90,000") * 0.05   # value of nickel
GregexprCheck(1:nrow(dat), "\\$90,000") * 3.95   # mass of Cdn. nickel (g)
GregexprCheck(1:nrow(dat), "\\$90,000") * 90000  # value of $90,000


# total value of all mentions
format(sum(df$vals), scientific=F)


# total number of dollar-value mentions
dollar_vals_tot <- unlist(lapply(dat$speech, GetDollarValsWrapper))
length(dollar_vals_tot)


# context for top-mentions
# 1. NDP's Proposed Carbon Tax
# 2. CBC Funding; Health Reseach Funding; Canada Post Deficit; Others
# 3. Liberal Payroll Tax; Child/Family Tax Credits; Others
# 4. NDP's Misspending on Satellite Offices
cpc1 <- dat$speech[intersect(indices_cpc, grep("\\$21[-\\s]billion", dat$speech, perl = T))]
cpc2 <- dat$speech[intersect(indices_cpc, grep("\\$1[-\\s]billion", dat$speech, perl = T))]
cpc3 <- dat$speech[intersect(indices_cpc, grep("\\$1\\,000", dat$speech, perl = T))]
cpc4 <- dat$speech[intersect(indices_cpc, grep("\\$2\\.7[-\\s]million", dat$speech, perl = T))]

# 1. Payment from Nigel Wright to Mike Duffy
# 2. G8 'Slush Fund'
# 3. Underspending at Veteran's Affairs; Funds Missing Under Liberal Gov. in 2002; Others
# 4. Missing Anti-terrorism Funds
ndp1 <- dat$speech[intersect(indices_ndp, grep("\\$90\\,000", dat$speech, perl = T))]
ndp2 <- dat$speech[intersect(indices_ndp, grep("\\$50[-\\s]million", dat$speech, perl = T))]
ndp3 <- dat$speech[intersect(indices_ndp, grep("\\$1[-\\s]billion", dat$speech, perl = T))]
ndp4 <- dat$speech[intersect(indices_ndp, grep("\\$3\\.1[-\\s]billion", dat$speech, perl = T))]

# 1. Payment from Nigel Wright to Mike Duffy
# 2. Family Tax Cut Income-Splitting; Infrastructure Spending Cuts; Others
# 3. Underspending at Veteran's Affairs; F-35 Program Costs; Others
# 4. Cuts to Veteran's Affairs; Infrastructure Spending Cuts; Others
lib1 <- dat$speech[intersect(indices_lib, grep("\\$90\\,000", dat$speech, perl = T))]
lib2 <- dat$speech[intersect(indices_lib, grep("\\$2[-\\s]billion", dat$speech, perl = T))]
lib3 <- dat$speech[intersect(indices_lib, grep("\\$1[-\\s]billion", dat$speech, perl = T))]
lib4 <- dat$speech[intersect(indices_lib, grep("\\$200[-\\s]million", dat$speech, perl = T))]


# for dollar-values with multiple contexts, merge statements into single df for manual check
out1 <- data.frame(rnk = "cpc2", val = "1B", speech = cpc2)
out2 <- data.frame(rnk = "cpc3", val = "1K", speech = cpc3)
out3 <- data.frame(rnk = "ndp3", val = "1B", speech = ndp3)
out4 <- data.frame(rnk = "lib2", val = "2B", speech = lib2)
out5 <- data.frame(rnk = "lib3", val = "1B", speech = lib3)
out6 <- data.frame(rnk = "lib4", val = "200M", speech = lib4)
context <- rbind(out1, out2, out3, out4, out5, out6)
#write.csv(context, "../data/context.txt", row.names = F)


# summarize most common contexts for dollar-value mentions with multiple contexts
context <- read.csv("../data/context-processed.txt", stringsAsFactors = F)
sort(table(context$context[context$rnk == "cpc2"]), decreasing = T)[1:5]
sort(table(context$context[context$rnk == "cpc3"]), decreasing = T)[1:5]
sort(table(context$context[context$rnk == "ndp3"]), decreasing = T)[1:5]
sort(table(context$context[context$rnk == "lib2"]), decreasing = T)[1:5]
sort(table(context$context[context$rnk == "lib3"]), decreasing = T)[1:5]
sort(table(context$context[context$rnk == "lib4"]), decreasing = T)[1:5]

