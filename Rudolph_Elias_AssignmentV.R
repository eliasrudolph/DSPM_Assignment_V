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

# 3. Interacting with the API - the basics

# get the API key
source("keyfile.R")

# perform the first GET request
venues <- GET(url = "https://app.ticketmaster.com/discovery/v2/venues?",
              query= list(apikey = tm_key,
                          locale = "*",
                          countryCode = "DE"))
# extract the necessary data 
venues <- fromJSON(content(venues, as = "text")) 

#create the venuesData dataframe where the different information o the venues should be stored 
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


