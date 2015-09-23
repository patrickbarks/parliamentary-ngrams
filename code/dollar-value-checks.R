
# load libraries
library(readr)


# read data
dat <- read_csv("../data/parl-41-processed.txt")


# check for spelled out dollar-values
dat$speech[grep("¢", dat$speech)]

dat$speech[grep("hundred dollars", dat$speech)]
dat$speech[grep("thousand dollars", dat$speech)]
dat$speech[grep("million dollars", dat$speech)]
dat$speech[grep("billion dollars", dat$speech)]
dat$speech[grep("trillion dollars", dat$speech)]

dat$speech[grep("hundred-dollar", dat$speech)]
dat$speech[grep("thousand-dollar", dat$speech)]
dat$speech[grep("million-dollar", dat$speech)]
dat$speech[grep("billion-dollar", dat$speech)]
dat$speech[grep("trillion-dollar", dat$speech)]

dat$speech[grep("-dollar", dat$speech)]
dat$speech[grep("-hundred-dollar", dat$speech)]
dat$speech[grep("-thousand-dollar", dat$speech)]
dat$speech[grep("-million-dollar", dat$speech)]
dat$speech[grep("-billion-dollar", dat$speech)]
dat$speech[grep("-trillion-dollar", dat$speech)]

dat$speech[grep("\\sdollar(?!s)", dat$speech, perl=T)]
dat$speech[grep("\\shundred[-\\s]dollar(?!s)", dat$speech, perl=T)]
dat$speech[grep("\\sthousand[-\\s]dollar(?!s)", dat$speech, perl=T)]
dat$speech[grep("\\smillion[-\\s]dollar(?!s)", dat$speech, perl=T)]
dat$speech[grep("\\sbillion[-\\s]dollar(?!s)", dat$speech, perl=T)]
dat$speech[grep("\\strillion[-\\s]dollar(?!s)", dat$speech, perl=T)]

dat$speech[grep("quarter[-\\s]million dollars", dat$speech, perl=T)]
dat$speech[grep("half[-\\s]million dollars", dat$speech, perl=T)]
dat$speech[grep("quarter[-\\s]billion dollars", dat$speech, perl=T)]
dat$speech[grep("half[-\\s]billion dollars", dat$speech, perl=T)]

dat$speech[grep("quarter of a million dollars", dat$speech)]
dat$speech[grep("half a million dollars", dat$speech)]
dat$speech[grep("three-quarters of a million dollars", dat$speech)]
dat$speech[grep("\\sa million dollars", dat$speech)]

dat$speech[grep("quarter of a billion dollars", dat$speech)]
dat$speech[grep("half a billion dollars", dat$speech)]
dat$speech[grep("three-quarters of a billion dollars", dat$speech)]
dat$speech[grep("\\sA billion dollars", dat$speech)]

dat$speech[grep("quarter of a trillion dollars", dat$speech)]
dat$speech[grep("half a trillion dollars", dat$speech)]
dat$speech[grep("three-quarters of a trillion dollars", dat$speech)]
dat$speech[grep("\\sa trillion dollars", dat$speech)]
