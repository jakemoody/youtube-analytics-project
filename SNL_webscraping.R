###################################################
##				  SNL Web Scraping
##				   DATA COLLECTION
##
## Scrape SNL Data for recent seasons from Wikipedia including Host, Musical Guest and Date 
##
###################################################

# Load packages
library(scrapeR)
library(stringr)

# Prepare URL
path <-"https://en.wikipedia.org/wiki/List_of_Saturday_Night_Live_episodes"
webpage <- getURL(path)
webpage <- readLines(tc <- textConnection(webpage)); close(tc)
pagetree <- htmlTreeParse(webpage, error=function(...){}, useInternalNodes = TRUE, encoding = "UTF-8")

# Extract table header and contents
tablehead <- xpathSApply(pagetree, "//*/table/tr", xmlValue)

# Get data only for season 42 
season_42 <- tablehead[896:909] # contains raw season 42 data
season_42 <- strsplit(season_42,"\n") # cleans up season 42 data

# Get data only for season 41
season_41 <- tablehead[874:894] # contains raw season 41 data
season_41 <- strsplit(season_41,"\n") # cleans up season 41 data

## For Loops ## 
# season 42
season_42_clean <- data.frame()
for(i in 1:length(season_42)){
  host <- season_42[[i]][[3]]
  musicalguest <- season_42[[i]][[4]]
  elim1 <- str_trim(unlist(strsplit(season_42[[i]][[5]], "\\("))[2])
  date <- str_trim(unlist(strsplit(elim1, "\\)")))
  new_row <- cbind(host, musicalguest,date)
  season_42_clean <- rbind(season_42_clean, new_row)
  season_42_clean$date <- as.Date(season_42_clean$date)
  i <- i + 1
} 

# season 41
season_41_clean <- data.frame()
for(i in 1:length(season_41)){
  host <- season_41[[i]][[3]]
  musicalguest <- season_41[[i]][[4]]
  elim1 <- str_trim(unlist(strsplit(season_41[[i]][[5]], "\\("))[2])
  date <- str_trim(unlist(strsplit(elim1, "\\)")))
  new_row <- cbind(host, musicalguest,date)
  season_41_clean <- rbind(season_41_clean, new_row)
  season_41_clean$date <- as.Date(season_41_clean$date)
  i <- i + 1
} 

## Merge the season into one data frame 
season_42_clean$season <- "Season 42"
season_41_clean$season <- "Season 41"
all_seasons <- rbind(season_42_clean, season_41_clean)

## Save as .csv
write.csv(all_seasons, "snl_seasons.csv")
