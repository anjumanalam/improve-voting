################################################
# Student name: Anjuman Alam
# Final Project
# Date due: January 17, 2021
#
# Attribution statement:
# 2. I did this project with help from the book and the professor and these Internet sources:
#

dev.off() # Clear the graph window
rm(list=ls()) # Clear user objects from the environment
cat('\014')   # Clear the console

# import packages
library(tidyverse)
library(ggplot2)
library(kernlab)
library(arules)
library(e1071)
library(arulesViz)
library(maps)
library(mapproj)
library(ggmap)
library(caret)

# User-defined functions



#---- DATA COLLECTION ----
#import data-set of results from poll on why Americans don't vote
nonvoters_data <- read_csv("nonvoters_data.csv")

#view information on data-set
summary(nonvoters_data)
glimpse(nonvoters_data)
str(nonvoters_data)

#---- DATA EXPLORATION ----

#




#---- DATA MUNGING ----

#rename columns for clarification


#clean data to include only people we have voting history of



#separate voters into groups: nonvoters, sometimes voters, and always voters



#---- DATA EXPLORATION (with clean data) ----

#


#---- DATA MODELING ----

#



#---- INSIGHTS ----

#











