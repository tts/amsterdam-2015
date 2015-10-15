library(dplyr)
library(shiny)
library(shinydashboard)
library(ggvis)
library(DT)
library(dygraphs)
library(xts)
library(sunburstR)
library(networkD3)
library(rCharts)

################################################
#
# Read in cleaned ImpactStory data,
# logged since Dec 2014
#
#################################################

#issstats <- read.csv(file = "isstats.csv", stringsAsFactors = F)
issstats <- read.table(file = "isstatsnew.csv", stringsAsFactors = F, sep = ";", header = TRUE)

issstats$Date <- as.Date(issstats$Date)

################################################
#
# Read in cleaned and prepared Altmetric.com data 
# returned by the API query
# to all DOI's now in our CRIS 
#
################################################

# Data for charts and DT datatable
dataForCharts <- read.table(file = "dataforcharts.csv", sep = ";", header = TRUE, stringsAsFactors = FALSE)


# Data for sunburst
datafreq <- read.table(file = "datafreq.csv", sep = ";", header = TRUE, stringsAsFactors = FALSE)

# Data for network graph
nodes <- read.csv("aaltonodes.csv")
links <- read.csv("aaltolinks.csv")

#######################
#
# Function for filling
# gaps in dygraph
#
#######################

# http://stackoverflow.com/questions/7735647/replacing-nas-with-latest-non-na-value
repeat.before = function(x) {   # repeats the last non NA value. Keeps leading NA
  ind = which(!is.na(x))        # get positions of nonmissing values
  if(is.na(x[1]))               # if it begins with a missing, add the 
    ind = c(1,ind)              # first position to the indices
  rep(x[ind], times = diff(     # repeat the values at these indices
    c(ind, length(x) + 1) ))    # diffing the indices + length yields how often 
}                               # they need to be repeated

####################
#
# Help variables
#
####################

metrics <- sort(c("Altmetric", "Mendeley", "Twitter", "Facebook", "GPlus", "CiteULike", "Readers", "Posts", "Accounts", "Feeds", "Delicious",
             "Videos", "Reddit", "News.Outlets", "Wikipedia", "LinkedIn", "StackExchange", "Forums", "Research.forums"))


schools <- c("ARTS", "BIZ", "CHEM", "ELEC", "ENG", "SCI")



