
# load library
library(readr)


# read data
dat <- read_csv("../data/parl-41-raw.txt")
str(dat)


# check dates
sort(unique(as.Date(dat$date, format="%d-%m-%Y")))
length(unique(as.Date(dat$date, format="%d-%m-%Y")))


# check mp names (363 unique)
# note non-targets: "Des voix"; "Hon. members"; "members"; "memebers"; "The Speaker"; etc.
as.data.frame(sort(unique(dat$name)))
length(unique(dat$name))


# remove statements by non-target members
non_targets <- "Speaker|member|memeber|Des voix"
non_target_indices <- grep(non_targets, dat$name)
unique(dat$name[non_target_indices])
dat <- dat[-non_target_indices,]


# check mp names again (351 unique)
as.data.frame(sort(unique(dat$name)))
length(unique(dat$name))


# remove mp titles (name always begins after period followed by space)
name_start <- as.numeric(regexpr("\\.\\s", dat$name)) + 2
name_end <- nchar(dat$name)
dat$name <- substr(dat$name, name_start, name_end)


# check mp names again (324 unique)
as.data.frame(sort(unique(dat$name)))
length(unique(dat$name))


# check statements corresponding to blank names
# 1st element: statement <p> began with "Some hon. members"; will omit for simplicity
# 2nd element: was spoken by 'An hon. member', so can be ommitted
blank_name_indices <- which(dat$name == "")
dat[blank_name_indices,]
dat <- dat[-blank_name_indices,]


# mp name corrections
# multiple entries for 'Kellie Leitch' and 'Paul Calandra'
dat$name <- gsub("\\s\\s+", " ", dat$name)
dat$name[grep("Kellie Leitch", dat$name)] <- "Kellie Leitch"


# check mp names again (320 unique)
sort(unique(dat$name))
length(unique(dat$name))


# find indices and names with missing party info
indices_na <- which(is.na(dat$party))
names_na <- dat$name[indices_na]


# fill in missing party values
for(i in 1:length(indices_na)) {
  indices <- which(dat$name == names_na[i])
  party <- unique(dat$party[indices])
  party <- party[which(!is.na(party))]
  if(length(party) == 0) party <- "unknown"
  if(length(party) > 1) party <- "multiple"
  dat$party[indices_na[i]] <- party
}


# any mps still missing party info?
which(is.na(dat$party))
which(dat$party == "multiple")


# manually enter party info for mps with multiple affiliations during 41st parliament
indices_mult_affil <- which(dat$party == "multiple")
dat[indices_mult_affil,]
dat$party[21720] <- "CPC"
dat$party[23046] <- "Lib."


# write to file
write.csv(dat, "../data/parl-41-processed.txt", row.names=F)


