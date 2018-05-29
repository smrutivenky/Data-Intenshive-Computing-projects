
#Name: Prachi Jayakumar Patil UBID:ppatil3 Person Number: 50247309
#Name: Smruti Venkatesh UBID: smrutive Person Number: 50247212

library(twitteR)
library(ggmap)

#consumer_key <- "KfssBIzTWULR3uyVuOrd8GzFR"
#consumer_secret <- "I8AOcpOfrtWv2bUCPusjt80oFxpFil7WvgOpTKeIIYaZGxY6Dk"
#access_token <- "868133989-zngCZdVzMdQ7tr4uHnioKUhOSpiAz7J02VKHorZa"
#access_secret <- "kvqeSwshr12ucE05MLXrYehRWCfkFhzmv1Pzp4o39OJaV"
#setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#searchTerm <- "#flu"
#searchResults <- searchTwitter(searchTerm, n = 1000)  # Gather Tweets 
#tweetFrame1 <- twListToDF(searchResults)

#userInfo1 <- lookupUsers(tweetFrame1$screenName)  # Batch lookup of user info
#userFrame <- twListToDF(userInfo1)  # Convert to a nice dF

userFrame1 <- read.csv("./data/myData.csv", header=T, as.is=TRUE, na.strings=c(NA, "NA", " NA"))
userFrame1<-userFrame1[complete.cases(userFrame1[,c(1,2)]), ]

locatedUsers1 <- !is.na(userFrame1$location)


locations <- geocode(userFrame1$location[locatedUsers1])


filteredLoc <- subset.data.frame(locations, lon >= -161.75583 & lon <= -68.01197 & lat >= 19.50139 & lat <= 64.85694)

#Reference: https://github.com/abresler/flowingdata_tutorials/blob/master/linemaps_in_r/library/latlong2state.R
library(sp)
library(maps)
library(mapdata)
library(maptools)


# The single argument to this function, pointsDF, is a data.frame in which:
#   - column 1 contains the longitude in degrees (negative in the US)
#   - column 2 contains the latitude in degrees

latlong2state <- function(pointsDF) {
    # Prepare SpatialPolygons object with one SpatialPolygon
    # per state (plus DC, minus HI & AK)
    states <- map('state', fill=TRUE, col="transparent", plot=FALSE)
    IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
    states_sp <- map2SpatialPolygons(states, IDs=IDs,
                     proj4string=CRS("+proj=longlat +datum=wgs84"))

    # Convert pointsDF to a SpatialPoints object 
    pointsSP <- SpatialPoints(pointsDF, 
                    proj4string=CRS("+proj=longlat +datum=wgs84"))

    # Use 'over' to get indices of the Polygons object containing each point 
    indices <- over(pointsSP, states_sp)

    # Return the state names of the Polygons object containing each point
    stateNames <- sapply(states_sp@polygons, function(x) x@ID)
    stateNames[indices]
}

df<-latlong2state(filteredLoc)
df<-as.data.frame(df)
colnames(df) <- c("states")
df$states

#df<- influenza1[,c(1,2,3)]
library(plyr)
dfr<- count(df,'states')

library(ggplot2)
library(fiftystater)

data("fifty_states")
p <- ggplot(dfr, aes(map_id = states)) + 
  # map points to the fifty_states shape data
  geom_map(aes(fill = freq), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  labs(x = "", y = "") +
  theme(legend.position = "bottom", 
        panel.background = element_blank())


p
