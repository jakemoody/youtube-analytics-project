###################################################
##				YouTube Saturday Night Live
##				         ANALYSIS
##
## This script contains analyses and creates plots/tables of SNL YouTube Channel Data 
##
##
###################################################
# Load libraries
library(ggplot2)
library(lubridate)
library(stringi)
library(plyr)
library(scales)
# Load data
m <- read.csv("snl_youtube.csv", stringsAsFactors=FALSE)

# Parse Date
m$PublishedDate <- strptime(m$PublishedDate, "%Y-%m-%dT%H:%M:%S")

# Parse Duration
m$Duration <- strptime(m$Duration, "PT%MM%SS")

# Create some helper variables
m$DaysOnline <- as.numeric( difftime(Sys.Date(), m$PublishedDate, units="days") )
m$TitleLength <- nchar(m$Title)
m$YearsOnline <- m$DaysOnline/365
m$Length <- as.numeric( difftime(m$Duration, "2017-02-16 00:00:00", units="mins"))
m$DescriptionLength <- nchar(m$Description)

# Categories
m$TitlePlusDesc <- paste(m$Title, m$Description)
m$Category <- "Other Skit"
m$Category[ grepl("Weekend Update", m$TitlePlusDesc, ignore.case=TRUE)] <- "Weekend Update"
m$Category[ grepl("Cold Open", m$TitlePlusDesc, ignore.case=TRUE)] <- "Cold Open"
m$Category[ grepl("Monologue", m$TitlePlusDesc, ignore.case=TRUE)] <- "Monologue"
m$Category[ grepl("Musical guest", m$TitlePlusDesc, ignore.case=TRUE)] <- "Musical guest"



# How many videos per category?
vidcount <- as.data.frame(table(m$Category)) 
ggplot(vidcount, aes(x = reorder(Var1, -Freq), y = Freq)) + 
          geom_bar(position=position_dodge(), stat="identity", fill="#FF9999", colour="black") + 
          xlab("Category") + ylab("Amount of Videos")



# Average ViewCount by Category
tapply(m$ViewCount, m$Category, mean)


### To Update: 

# ViewCount by Category
means <- aggregate(ViewCount ~ Category, m, function(x) c(M = mean(x), SE = sd(x)/sqrt(length(x))))
names(means)
means$Mean <- means$ViewCount[,1]
means$SE <- means$ViewCount[,2]
means$ViewCount <- NULL

## Graphs
p1 <- ggplot(means, aes(x = Category, y = Mean)) + geom_bar(position=position_dodge(), stat="identity", fill="#FF9999", colour="black") + theme(axis.text.x =element_text(angle =90, hjust=1)) + geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.2, position=position_dodge(.9))
p2 <- ggplot(means, aes(x = reorder(Category, -Mean), y = Mean)) + geom_bar(position=position_dodge(), stat="identity", fill="#FF9999", colour="black") + theme(axis.text.x =element_text(angle =90, hjust=1)) + ylab("Mean Viewers per Video") + xlab("Category") + geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.2, position=position_dodge(.9))

# Linear Regression
## Linear Regression showing the relationship between category and view count. 

summary( lm( log(ViewCount) ~ Category + DaysOnline + Length, m))

library(texreg)
fit1 <- lm(log(ViewCount + 0.5) ~ YearsOnline + Length, data = m)
fit2 <- lm(log(ViewCount + 0.5) ~ Category + YearsOnline + Length, data = m)
coefNames <- c("Intercept", "Online (in Years)", "Length (in minutes)", "Category:ColdOpen", "Category:OtherSkit", "Category:Musicalguest", "Category:Monologue")
screenreg( list(fit1, fit2), custom.coef.names=coefNames )

