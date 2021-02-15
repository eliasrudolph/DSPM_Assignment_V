rm(list = ls())

if (!require("jsonlite")) install.packages("jsonlite")
if (!require("httr")) install.packages("httr")
if (!require("rlist")) install.packages("rlist")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("naniar")) install.packages("naniar")

library(jsonlite)
library(httr)
library(rlist)
library(tidyverse)
library(naniar)

setwd("C:/Users/Elias Rudolph/Documents/Studium/9. Semester/DSPM/Assignments/5/code/DSPM_Assignment_V")

# 3. Interacting with the API - the basics

# Get the API key.
source("keyfile.R")

# Perform the first GET request
venues <- GET(url = "https://app.ticketmaster.com/discovery/v2/venues?",
              query= list(apikey = tm_key,
                          locale = "*",
                          countryCode = "DE"))
# Extract the necessary data 
venues <- fromJSON(content(venues, as = "text")) 

# Create the venuesData dataframe where the different information of the venues should be stored.
venuesData <- tibble(
  name = character(20),
  city = character(20),
  postalCode = character(20),
  address = character(20),
  url = character(20),
  longitude = double(20),
  latitude = double(20)
)
#set the part of the path which is needed for every data selection
datapath <- venues[["_embedded"]][["venues"]]

name <- datapath[["name"]]
venuesData[1] <- name

city <- datapath[["city"]][["name"]]
venuesData[2] <- city

postalCode <- datapath[["postalCode"]]
venuesData[3] <- postalCode

address <- datapath[["address"]][["line1"]]
venuesData[4] <- address

locationUrl <- datapath[["url"]]
venuesData[5] <- locationUrl

longitude <- datapath[["location"]][["longitude"]]
venuesData[6] <- as.double(longitude) 

latitude <- datapath[["location"]][["latitude"]]
venuesData[7] <- as.double(latitude) 

glimpse(venuesData)

# 4. Interacting with the API - advanced

# There are 12238 venues listed as this Assignment was solved. 
# It is possible to set the elements per page up to < 1000.
# For mathematically simplicity the elements per page are kept by 500.

# Calculate the number of pages.
n <- 12238
n/500
# So 25 iterations are needed in total.
# 24 iterations will be included in the loop.
# The last iteration will be done manually in order to avoid an error.
pages <- floor(n/500)

# Set up the empty dataframe with the according size.
venuesData2 <- tibble(
  name = character(n),
  city = character(n),
  postalCode = character(n),
  address = character(n),
  url = character(n),
  longitude = character(n),
  latitude = character(n)
)

# Use a loop to iterate over the first 24 pages. 
for (i in 1:pages) {
  tep_data <- GET(url = "https://app.ticketmaster.com/discovery/v2/venues?",
                    query= list(apikey = tm_key,
                                locale = "*",
                                size = "500",
                                page = i-1,
                                countryCode = "DE"))
  # Note that we it is necessary to set page = i -1 because page counting starts with page 0.
  
  venues_tep <- fromJSON(content(tep_data, as = "text"))
  datapath <- venues_tep[["_embedded"]][["venues"]]
  
  # Set up an temporary dataframe to store the data of each page. 
  # Secondly add the data to the  each iteration   
  venuesData_tep <- tibble(
    name = character(500),
    city = character(500),
    postalCode = character(500),
    address = character(500),
    url = character(500),
    longitude = character(500),
    latitude = character(500)
  )
  # Extract the relevant data from the API request 
  # and store it each in a column of the data frame.
  
  name <- datapath[["name"]]
  venuesData_tep[1] <- name
  
  city <- datapath[["city"]][["name"]]
  venuesData_tep[2] <- city
  
  postalCode <- datapath[["postalCode"]]
  venuesData_tep[3] <- postalCode
  
  address <- datapath[["address"]][["line1"]]
  venuesData_tep[4] <- address
  
  locationUrl <- datapath[["url"]]
  venuesData_tep[5] <- locationUrl
  
  longitude <- datapath[["location"]][["longitude"]]
  venuesData_tep[6] <- longitude 
  
  latitude <- datapath[["location"]][["latitude"]]
  venuesData_tep[7] <- latitude
  # Fill the original dataframe with the data from the temporary dataframe.  
  venuesData2[(500 * i - 499):(500 * i),] <- venuesData_tep
  # Set a timer to avoid breaking rate limitations for the requests. 
  Sys.sleep(0.2)
}

# To fill the last entries a additional request is needed. 
# Calculate how many values are still missing.
r <- n - (500*pages)

tep_data <- GET(url = "https://app.ticketmaster.com/discovery/v2/venues?",
                query= list(apikey = tm_key,
                            locale = "*",
                            size = "500",
                            page = i,
                            countryCode = "DE"))

venues_tep <- fromJSON(content(tep_data, as = "text"))

# Use the same structure of data acquisition as before, but now adapted to the rest of the values. 
datapath <- venues_tep[["_embedded"]][["venues"]]

venuesData_tep <- tibble(
  name = character(r),
  city = character(r),
  postalCode = character(r),
  address = character(r),
  url = character(r),
  longitude = character(r),
  latitude = character(r)
)

# Extract the relevant data from the API request 
# and store it each in a column of the data frame.

name <- datapath[["name"]]
venuesData_tep[1] <- name

city <- datapath[["city"]][["name"]]
venuesData_tep[2] <- city

postalCode <- datapath[["postalCode"]]
venuesData_tep[3] <- postalCode

address <- datapath[["address"]][["line1"]]
venuesData_tep[4] <- address

locationUrl <- datapath[["url"]]
venuesData_tep[5] <- locationUrl

longitude <- datapath[["location"]][["longitude"]]
venuesData_tep[6] <- longitude 

latitude <- datapath[["location"]][["latitude"]]
venuesData_tep[7] <- latitude
# Fill the the missing 238 values of the venue dataframe.
venuesData2[(n-r+1):(n),] <- venuesData_tep

# Change the geospatial data into doubles
venuesData2$longitude <- as.double(venuesData2$longitude)
venuesData2$latitude <- as.double(venuesData2$latitude)

glimpse(venuesData2)


# 5. Visualizing the extracted data

# Set the extreme points for Germany.
maxLongitude <- 15.043611
minLongitude <- 5.866944
maxLatitude <- 55.0846
minLatitude <- 47.271679

# Choose only the data which is within the extreme points.
geoData <-  filter(venuesData2, 
                   latitude < maxLatitude & latitude > minLatitude & longitude < maxLongitude & longitude > minLongitude)

# Plot the data for Germany.
ggplot() +
  geom_polygon(
    aes(x = long, y = lat, group = group), data = map_data("world", region = "Germany"),
    fill = "grey90",color = "black") +
  theme_void() + coord_quickmap() + 
  geom_point(data = geoData, aes(x = longitude, y = latitude), alpha = 0.3, colour= "darkblue") +
  labs(title = "Event locations across Germany", caption = "Source: ticketmaster.com") +
  geom_text(show.legend = FALSE) +
  theme(title = element_text(size=8, face='bold'),
        plot.caption = element_text(face = "italic"))

# 6. Event locations in other countries

#  As a second example Spain is chosen. 

# 6.3 Interacting with the API - the basics
# Perform the first GET request
venuesEs <- GET(url = "https://app.ticketmaster.com/discovery/v2/venues?",
              query= list(apikey = tm_key,
                          locale = "*",
                          countryCode = "ES"))
# Extract the necessary data 
venuesEs <- fromJSON(content(venuesEs, as = "text")) 

# Create the "venuesData" dataframe where the different information 
# of the venues should be stored.
venuesDataEs <- tibble(
  name = character(20),
  city = character(20),
  postalCode = character(20),
  address = character(20),
  url = character(20),
  longitude = double(20),
  latitude = double(20)
)
#set the part of the path which is needed for every data selection. 
datapath <- venuesEs[["_embedded"]][["venues"]]

# Extract the relevant data from the API request 
# and store it each in a column of the data frame.
name <- datapath[["name"]]
venuesDataEs[1] <- name

city <- datapath[["city"]][["name"]]
venuesDataEs[2] <- city

postalCode <- datapath[["postalCode"]]
venuesDataEs[3] <- postalCode

address <- datapath[["address"]][["line1"]]
venuesDataEs[4] <- address

locationUrl <- datapath[["url"]]
venuesDataEs[5] <- locationUrl

longitude <- datapath[["location"]][["longitude"]]
venuesDataEs[6] <- as.double(longitude) 

latitude <- datapath[["location"]][["latitude"]]
venuesDataEs[7] <- as.double(latitude) 

glimpse(venuesDataEs)

# 6.4 Interacting with the API - advanced

n <- 3066
n/500
# So 7 iterations are needed. 
# 6 of them through a loop, 1 iteration is needed to be proceeded manually. 
pages <- floor(n/500)

# Set up the empty dataframe with the according size.
venuesDataES2 <- tibble(
  name = character(n),
  city = character(n),
  postalCode = character(n),
  address = character(n),
  url = character(n),
  longitude = character(n),
  latitude = character(n)
)

# Use a loop to iterate over the first 6 pages. 
for (i in 1:pages) {
  tep_data <- GET(url = "https://app.ticketmaster.com/discovery/v2/venues?",
                  query= list(apikey = tm_key,
                              locale = "*",
                              size = "500",
                              page = i-1,
                              countryCode = "ES"))
  
  
  venues_tep <- fromJSON(content(tep_data, as = "text"))
  datapath <- venues_tep[["_embedded"]][["venues"]]
  
  # Set up an temporary dataframe to store the data of each page. 
  # Secondly add the data to the  each iteration   
  venuesData_tep <- tibble(
    name = character(500),
    city = character(500),
    postalCode = character(500),
    address = character(500),
    url = character(500),
    longitude = character(500),
    latitude = character(500)
  )
  
  # Extract the relevant data from the API request 
  # and store it each in a column of the data frame. 
  name <- datapath[["name"]]
  venuesData_tep[1] <- name
  
  city <- datapath[["city"]][["name"]]
  venuesData_tep[2] <- city
  
  postalCode <- datapath[["postalCode"]]
  venuesData_tep[3] <- postalCode
  
  address <- datapath[["address"]][["line1"]]
  venuesData_tep[4] <- address
  
  locationUrl <- datapath[["url"]]
  venuesData_tep[5] <- locationUrl
  
  longitude <- datapath[["location"]][["longitude"]]
  venuesData_tep[6] <- longitude 
  
  latitude <- datapath[["location"]][["latitude"]]
  venuesData_tep[7] <- latitude
  # Fill the original dataframe with the data from the temporary dataframe.  
  venuesDataES2[(500 * i - 499):(500 * i),] <- venuesData_tep
  # Set a timer to avoid breaking rate limitations for the requests. 
  Sys.sleep(0.2)
}

# Calculate the missing venues for the last iteration.
r <- n - (500*pages)

tep_data <- GET(url = "https://app.ticketmaster.com/discovery/v2/venues?",
                query= list(apikey = tm_key,
                            locale = "*",
                            size = "500",
                            page = i,
                            countryCode = "ES"))

venues_tep <- fromJSON(content(tep_data, as = "text"))

# Use the same structure of data acquisition as before, 
# but now adapted to the rest of the values. 
datapath <- venues_tep[["_embedded"]][["venues"]]

venuesData_tep <- tibble(
  name = character(r),
  city = character(r),
  postalCode = character(r),
  address = character(r),
  url = character(r),
  longitude = character(r),
  latitude = character(r)
)

# Extract the relevant data from the API request 
# and store it each in a column of the data frame.
name <- datapath[["name"]]
venuesData_tep[1] <- name

city <- datapath[["city"]][["name"]]
venuesData_tep[2] <- city

postalCode <- datapath[["postalCode"]]
venuesData_tep[3] <- postalCode

address <- datapath[["address"]][["line1"]]
venuesData_tep[4] <- address

locationUrl <- datapath[["url"]]
venuesData_tep[5] <- locationUrl

longitude <- datapath[["location"]][["longitude"]]
venuesData_tep[6] <- longitude 

latitude <- datapath[["location"]][["latitude"]]
venuesData_tep[7] <- latitude
# Fill the the missing 238 values of the venue dataframe.
venuesDataES2[(n-r+1):(n),] <- venuesData_tep

# Change the geospatial data into doubles
venuesDataES2$longitude <- as.double(venuesDataES2$longitude)
venuesDataES2$latitude <- as.double(venuesDataES2$latitude)


# 6.5 Visualizing the extracted data

# Set the extreme points for Germany.
maxLongitude <- 3.316667
minLongitude <- -9.3
maxLatitude <- 43.783333
minLatitude <- 27.633333

# Choose only the data which is only within the extreme points.
geoData <-  filter(venuesDataES2, 
                   latitude < maxLatitude & latitude > minLatitude & longitude < maxLongitude & longitude > minLongitude)

# Plot the data for Spain.
ggplot() +
  geom_polygon(
    aes(x = long, y = lat, group = group), data = map_data("world", region = "Spain"),
    fill = "grey90",color = "black") +
  theme_void() + coord_quickmap() + 
  geom_point(data = geoData, aes(x = longitude, y = latitude), alpha = 0.3, colour= "darkred") +
  labs(title = "Event locations across Spain", caption = "Source: ticketmaster.com") +
  geom_text(show.legend = FALSE) +
  theme(title = element_text(size=8, face='bold'),
        plot.caption = element_text(face = "italic"))
