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
