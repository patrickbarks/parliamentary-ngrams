
### load required libraries
library(RCurl)
library(httr)
library(XML)



### function to parse speaker's name, party, and text of speech
# text_to_parse has consistent format "[title]. [name] ([position], [party]): [text-of-speech]"
# speakers_string is a regex string containing names of all possible speakers (i.e. MPs)
#   in same format as initial part of text_to_parse "[title]. [name] ([position], [party]):"
ParseSpeeches <- function(text_to_parse, speakers_string) {

  # hansard acronyms for federal parties in 41st parliament
  parties <- c("CPC", "Lib.", "NDP", "BQ", "GP", "FD", "Ind.")

  # get initial part of text containing speaker info (name, party)
  speaker_info_len <- attr(regexpr(speakers_string, text_to_parse), "match.length")
  speaker_info <- substr(text_to_parse, 1, speaker_info_len)

  # get next part of text containing text of speech
  speech_text <- substr(text_to_parse, speaker_info_len + 1, nchar(text_to_parse))
    
  # get party affiliation (NA if no match)
  party_index <- which(sapply(parties, function(x) length(grep(x, speaker_info))) > 0)
  party <- ifelse(length(party_index) > 0, parties[party_index], NA)

  # get speaker's name (ends prior to left parenthesis or colon)
  name_end <- as.numeric(regexpr("\\s\\(|\\:", speaker_info)) - 1
  name <- substr(speaker_info, 1, name_end)

  # return speaker's name, party, and text of speech
  return(c(name = name, party = party, speech = speech_text))
}



### function to get and parse hansard transcripts of daily parliamentary debates
ParseDailyDebates <- function(url) {

  # get webpage; replace <br> with whitespace; parse html
  page <- GET(url)
  page <- gsub("<br>", " ", page)
  page <- htmlParse(page)

  # get and format date
  date <- unique(xpathSApply(page, "//h2[@id='publicationDate']", xmlValue))
  date <- as.Date(date, format="%A, %B %d, %Y")
  date <- as.character(format(date, "%d-%m-%Y"))

  # get headers; find which header corresponds to question period
  headers <- xpathSApply(page, "//h2", xmlValue)
  qperiod_index <- grep("oral questions", headers, ignore.case=T)

  # if no headers correspond to question period, return NULL
  if(length(qperiod_index) == 0) return(NULL)

  # get speakers
  speakers <- unique(xpathSApply(page, "//span[@class='paratext']//b", xmlValue))
  speakers <- speakers[nchar(speakers) > 5]

  # collapse speakers into single regex string; escape parentheses with \\
  speakers_string <- paste("^", paste(speakers, collapse="|^"), sep="")
  speakers_string <- gsub("\\(", "\\\\\\(", speakers_string)
  speakers_string <- gsub("\\)", "\\\\\\)", speakers_string)

  # get lines of speech including headers
  lines <- xpathSApply(page, "//span[@class='paratext']|//h2", xmlValue)

  # get index corresponding to first line of question period
  lines_start <- grep(headers[qperiod_index], lines) + 1

  # get index corresponding to last line of question period
  if(qperiod_index == length(headers)) {
    lines_end <- length(lines)
  } else {
    lines_end <- grep(headers[qperiod_index + 1], lines)
    lines_end <- min(lines_end[lines_end > lines_start])
  }

  # extract subset of lines of speech corresponding to question period
  lines_qperiod <- lines[lines_start:lines_end] 

  # remove leading white space from lines of speech
  lines_qperiod <- gsub("^[[:space:]]+", "", lines_qperiod)

  # collapse lines_qperiod to reflect one line per speaker
  indices_new_speaker <- grep(speakers_string, lines_qperiod)
  n_speeches <- length(indices_new_speaker)
  speech_starts <- indices_new_speaker
  speech_ends <- c(indices_new_speaker[-1], length(lines_qperiod)) - 1
  speech_index <- mapply(seq, from = speech_starts, to = speech_ends)
  speeches <- sapply(speech_index, function(x) paste(lines_qperiod[x], collapse=" "))

  # parse speaker's name, party, and text of speech
  df <- sapply(speeches, ParseSpeeches, speakers = speakers_string, USE.NAMES = F)
  df <- as.data.frame(t(df), stringsAsFactors=F)

  # find indices and names with missing party info
  indices_na <- which(is.na(df$party))
  names_na <- df$name[indices_na]

  # fill in missing party values (if any)
  if (length(indices_na) > 0) {
    for(i in 1:length(indices_na)) {
      indices <- which(df$name == names_na[i])
      party <- unique(df$party[indices])
      party <- party[which(!is.na(party))]
      if(length(party) == 0) party <- NA
      if(length(party) > 1) party <- "multiple"
      df$party[indices_na[i]] <- party
    }
  }

  # remove extra white space
  df$speech <- gsub("[[:space:]][[:space:]]+", " ", df$speech)
  df$speech <- gsub("^[[:space:]]|[[:space:]]$", "", df$speech)

  # add date
  df <- cbind(date = date, df)

  # return
  return(df)
}



### get urls for each daily debate within the two sessions of the 41st parliament
sessionUrls <- c(
  p41s2 = "http://www.parl.gc.ca/HouseChamberBusiness/ChamberSittings.aspx?View=H&Parl=41&Ses=2&Language=E&Mode=2",
  p41s1 = "http://www.parl.gc.ca/HouseChamberBusiness/ChamberSittings.aspx?View=H&Parl=41&Ses=1&Language=E&Mode=2")

GetDailyUrls <- function(sessionUrl) {
  page <- GET(sessionUrl)
  page <- htmlParse(page)
  paths <- xpathSApply(page, "//span[@class='CalendarEventType159']//a/@href")
  urls <- paste("http://www.parl.gc.ca", paths, sep="")
  return(urls)
}

dailyUrls <- unlist(lapply(sessionUrls, GetDailyUrls), use.names=F)



### compile text from every question period in the 41st parliament
out <- list()

for (j in 1:length(dailyUrls)) {
  out[[j]] <- ParseDailyDebates(dailyUrls[j])
  print(j)
  Sys.sleep(1.1)  # be kind to the gc.ca servers!!
}

str(out)

### unlist and write to file
out <- do.call(rbind, out)
write.csv(out, "../data/parl-41-raw.txt", row.names=F)

