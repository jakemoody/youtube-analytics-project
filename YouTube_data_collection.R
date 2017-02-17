###################################################
##				  YouTube Channel
##				  DATA COLLECTION
##
## This script collects data about YouTube videos posted by any user on their
## on their channel. This example looks up data from Saturday Night Live's YouTube Channel
##
###################################################

# First time you run this, you need to install some packages
# install.packages(c("rjson", "plyr"))

# Load libraries
library(rjson)
library(plyr)

# Google API key; you need to go and register your own; select "Key for server applications"
# https://console.developers.google.com/
# Make sure you also register your IP address as well
key <- "### Enter Your API Key ###"	

# How to get vidoes posted by a user?
# Get the playlistID for the "upload" channel of the user "SaturdayNightLive"
url1 <- sprintf( "https://www.googleapis.com/youtube/v3/channels?part=contentDetails&forUsername=%s&key=%s", "SaturdayNightLive", key)
res1 <- fromJSON(file=url1)
playlistID <- res1$items[[1]]$contentDetails$relatedPlaylists$uploads

###############################################
## VIDEO Metadata: Helper function to turn JSON format into a data.frame 
###############################################
parseSnippet <- function(x) {
	# Could extract other fields such as Description=x$snippet$description
	vid  <- x$snippet$resourceId$videoId
	date  <- x$snippet$publishedAt
	title <- x$snippet$title
	desc <- x$snippet$description
	row <-  data.frame(VideoID=vid, PublishedDate=date, Title=title, Description=desc, stringsAsFactors=FALSE)
	return(row)
}


###############################################
## VIDEO STATISTICS: Helper function to turn JSON format into a data.frame 
###############################################
parseStatistics <- function(x) {
	# Could extract other fields such as Description=x$snippet$description
	return( data.frame(VideoID=x$id, 
			ViewCount=as.numeric(x$statistics$viewCount), 
			LikeCount=as.numeric(x$statistics$likeCount), 
			DislikeCount=as.numeric(x$statistics$dislikeCount), 
			FavoriteCount=as.numeric(x$statistics$favoriteCount), 
			CommentCount=as.numeric(x$statistics$commentCount), stringsAsFactors=FALSE) ) 
}

###############################################
## VIDEO ContentDetails: Helper function to turn JSON format into a data.frame 
###############################################
parseContentDetails <- function(x) {
	# Could extract other fields such as Description=x$snippet$description
	return( data.frame(VideoID=x$id, 
			Duration=x$contentDetails$duration, stringsAsFactors=FALSE) ) 
}


###############################################
## Get all videos for playlistID
## Loop over all next pages
###############################################
full <- data.frame(stringsAsFactors=FALSE)
nextPageToken <- "FIRST"

while( ! is.null(nextPageToken) ) {
	if( nextPageToken=="FIRST" ) {
		print("Reading first page ...")
		url2 <- sprintf("https://www.googleapis.com/youtube/v3/playlistItems?part=snippet&playlistId=%s&maxResults=%i&key=%s", 
						playlistID, 50, key)
	} else {
		url2 <- sprintf("https://www.googleapis.com/youtube/v3/playlistItems?part=snippet&playlistId=%s&maxResults=%i&pageToken=%s&key=%s", 
						playlistID, 50, nextPageToken, key)
		print( sprintf( "Reading next page %s ...", nextPageToken) )
	}
	page <- fromJSON(file=url2)
	nextPageToken <- page$nextPageToken
	
	# Call the parseSnippet function for every video in the result set 
	videosPage <- ldply(page$items, parseSnippet)

	# Now get statistics on those videos from other api
	# Docu: https://developers.google.com/youtube/v3/docs/videos/list
	url3 <- sprintf("https://www.googleapis.com/youtube/v3/videos?part=statistics&id=%s&key=%s", 
					paste(videosPage$VideoID, collapse=","), key)		# Create comma-separated list of all videoIDs
	res3 <- fromJSON(file=url3)

	# Call the parseStatistics function for every video in the result set 
	statsPage <- ldply(res3$items, parseStatistics)

	# Now get statistics on those videos from other api
	# Docu: https://developers.google.com/youtube/v3/docs/videos/list
	url4 <- sprintf("https://www.googleapis.com/youtube/v3/videos?part=contentDetails&id=%s&key=%s", 
					paste(videosPage$VideoID, collapse=","), key)		# Create comma-separated list of all videoIDs
	res4 <- fromJSON(file=url4)

	# Call the parseStatistics function for every video in the result set 
	detailsPage <- ldply(res4$items, parseContentDetails)

	# Merge the video data.frame and the stats data.frame together
	page <- merge(videosPage, statsPage)
	page <- merge(page, detailsPage)

	full <- rbind(full, page)
}

# Write to file for later use
write.csv(full, "snl_youtube.csv", row.names=FALSE)


