###################################################
##				YouTube Saturday Night Live
##				         ANALYSIS
##
## This script contains analyses and creates plots/tables of SNL YouTube Channel Data 
## It looks at characteristics like video length, Host, Musical Guest, and more. 
##
##
###################################################
# Load libraries
library(ggplot2)
library(lubridate)
library(chron)
library(dplyr)
library(stringr)
 
# Load data (see: YouTube_data_collection.R to see how to produce the file)
m <- read.csv("snl_youtube.csv", stringsAsFactors=FALSE)

# Parse Date
m$PublishedDate_clean <- as.POSIXct(strptime(m$PublishedDate, "%Y-%m-%dT%H:%M:%S"))

# Parse Video Length
time_length <- strptime(m$Duration, "PT%MM%SS") # Converts to POSIXt object
m$Duration_min <- difftime(time_length, "2017-02-18 00:00:00 EST")

# Parse Days and Years Online
m$DaysOnline <- difftime(Sys.Date(), m$PublishedDate_clean, units="days")

# Parse character length of description and title 
m$TitleLength <- nchar(m$Title)
m$DescriptionLength <- nchar(m$Description)

# Helper function to find season based on date
getSeason <- function(DATES) {
  WI <- as.Date("2012-12-01", format = "%Y-%m-%d") # Winter start date
  SP <- as.Date("2012-3-01",  format = "%Y-%m-%d") # Spring start date
  SU <- as.Date("2012-6-01",  format = "%Y-%m-%d") # Summer start date
  FA <- as.Date("2012-9-01",  format = "%Y-%m-%d") # Fall start date
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(DATES, format="2012-%m-%d"))
  
  ifelse (d >= WI | d <  SP, "Winter", 
          ifelse (d >=  SP & d < SU, "Spring",
                  ifelse (d >= SU & d < FA, "Summer", "Fall")))
}

## Apply getSeason to PublishedDate
for(i in 1:nrow(m)){ # for each row in the market_data data frame
  m$Season[i] <- getSeason(m$PublishedDate_clean[i]) #...we should consider it a season: Winter, Spring, Summer or Fall based on the date at the half way point of the time interval
  i <- i + 1 # keep the for loop going
}

# Create categories based on description and title 
m$TitlePlusDesc <- paste(m$Title, m$Description)
m$Category <- "Other Skit"
m$Category[ grepl("Weekend Update", m$TitlePlusDesc, ignore.case=TRUE)] <- "Weekend Update"
m$Category[ grepl("Cold Open", m$TitlePlusDesc, ignore.case=TRUE)] <- "Cold Open"
m$Category[ grepl("Monologue", m$TitlePlusDesc, ignore.case=TRUE)] <- "Monologue"
m$Category[ grepl("Musical guest", m$TitlePlusDesc, ignore.case=TRUE)] <- "Musical guest"

# Campaign Related?  
m$Campaign <- "Not Campign Related"
m$Campaign[ grepl("Donald Trump | Trump | Hillary Clinton | Clinton", m$TitlePlusDesc, ignore.case=TRUE)] <- "Campaign Related"

# Categorize by Host (see: SNL_wedbscraping.R for details on how to scrape this data)
snl <- read.csv("snl_seasons.csv", stringsAsFactors = FALSE) # Load Data for seasons 41 & 42

## Associate Host with video
m$Host <- "Other"
for(i in 1:length(snl$host)){
  m$Host[grepl(snl$host[i], m$TitlePlusDesc, ignore.case=TRUE)] <- snl$host[i]
  i <- i +1 
}

## Associate Musical Guest with Video
m$MusicGuest <- "Other"
for(i in 1:length(snl$musicalguest)){
  m$MusicGuest[grepl(snl$musicalguest[i], m$TitlePlusDesc, ignore.case=TRUE)] <- snl$musicalguest[i]
  i <- i + 1 
}

# Remove all YouTube Videos Posted before Season 41 
recent_vids <- m %>% filter(!PublishedDate_clean < min(as.Date(snl$date)) & Duration_min > 1.00) # Remove ads and promo videos

### Descriptive Stats & Exploratory Data Analysis

hist(recent_vids$ViewCount) # Long-tail distribution
hist(as.numeric(recent_vids$Duration_min)) # The length of the videos are positively skewed
summary(as.numeric(recent_vids$Duration_min)) # Videos are a minimum of Median of 4.1 min, Mean of 4.168 min; max of 12 min

# ViewCount by Category
means_category <- aggregate(ViewCount ~ Category, recent_vids, function(x) c(M = mean(x), SE = sd(x)/sqrt(length(x))))
means_category$Mean <- means_category$ViewCount[,1]
means_category$SE <- means_category$ViewCount[,2]
means_category$ViewCount <- NULL
# Plot
ggplot(means_category, aes(x = reorder(Category, -Mean), y = Mean)) + geom_bar(position=position_dodge(), stat="identity", fill="#FF9999", colour="black") + theme(axis.text.x =element_text(angle =90, hjust=1)) + ylab("Mean Views per Video") + xlab("Category") + geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.2, position=position_dodge(.9))

# ViewCount by Host
means_host <- aggregate(ViewCount ~ Host, recent_vids, function(x) c(M = mean(x), SE = sd(x)/sqrt(length(x))))
means_host$Mean <- means_host$ViewCount[,1]
means_host$SE <- means_host$ViewCount[,2]
means_host$ViewCount <- NULL
# Plot
ggplot(means_host, aes(x = reorder(Host, -Mean), y = Mean)) + geom_bar(position=position_dodge(), stat="identity", fill="#FF9999", colour="black") + theme(axis.text.x =element_text(angle =90, hjust=1)) + ylab("Mean Views per Video") + xlab("Category") + geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.2, position=position_dodge(.9))

# ViewCount by MusicGuest
means_musical <- aggregate(ViewCount ~ MusicGuest, recent_vids, function(x) c(M = mean(x), SE = sd(x)/sqrt(length(x))))
means_musical$Mean <- means_musical$ViewCount[,1]
means_musical$SE <- means_musical$ViewCount[,2]
means_musical$ViewCount <- NULL
# Plot
ggplot(means_musical, aes(x = reorder(MusicGuest, -Mean), y = Mean)) + geom_bar(position=position_dodge(), stat="identity", fill="#FF9999", colour="black") + theme(axis.text.x =element_text(angle =90, hjust=1)) + ylab("Mean Views per Video") + xlab("Category") + geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.2, position=position_dodge(.9))

# ViewCount by Campaign
means_campaign <- aggregate(ViewCount ~ Campaign, recent_vids, function(x) c(M = mean(x), SE = sd(x)/sqrt(length(x))))
means_campaign$Mean <- means_campaign$ViewCount[,1]
means_campaign$SE <- means_campaign$ViewCount[,2]
means_campaign$ViewCount <- NULL
# Plot
ggplot(means_campaign, aes(x = reorder(Campaign, -Mean), y = Mean)) + geom_bar(position=position_dodge(), stat="identity", fill="#FF9999", colour="black") + theme(axis.text.x =element_text(angle =90, hjust=1)) + ylab("Mean Views per Video") + xlab("Category") + geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.2, position=position_dodge(.9))


## Linear Regression showing the relationship between category, host, musical guest, etc. on ViewCount

library(texreg)
fit1 <- lm( log(ViewCount + 0.5 ) ~ Host + MusicGuest + Category + as.numeric(Duration_min), recent_vids)
fit2 <- lm( log(ViewCount + 0.5 ) ~ Host + MusicGuest + as.numeric(Duration_min), recent_vids)
fit3 <- lm( log(ViewCount + 0.5 ) ~ Host + as.numeric(Duration_min), recent_vids)
fit4 <- lm( log(ViewCount + 0.5 ) ~ MusicGuest + as.numeric(Duration_min), recent_vids)
fit5 <- lm( log(ViewCount + 0.5 ) ~ Campaign + as.numeric(Duration_min), recent_vids)
fit6 <- lm( log(ViewCount + 0.5 ) ~ Category + as.numeric(Duration_min), recent_vids)
fit7 <- lm(log(ViewCount + 0.5 ) ~ as.numeric(Duration_min), recent_vids)

screenreg( list(fit1, fit2, fit3, fit4, fit5, fit6, fit7))


