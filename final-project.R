################################################
# Student name: Anjuman Alam
# Final Project
# Date due: January 18, 2021
#
# Attribution statement:
# 2. I did this project with help from the book and the professor and these Internet sources:
# https://www.statmethods.net/management/operators.html
# https://www.guru99.com/r-data-frames.html#:~:text=We%20can%20create%20a%20dataframe,the%20name%20of%20the%20variables.
# https://www.datanovia.com/en/lessons/rename-data-frame-columns-in-r/

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
countNum <- function(v) {
  selected <- length(which(v == 1))
  return(selected)
}


#---- DATA COLLECTION ----
# import data-set of results from poll on why Americans don't vote
nonvoters_data <- read_csv("nonvoters_data.csv")

# view information on data-set
summary(nonvoters_data)
glimpse(nonvoters_data)

#---- DATA MUNGING ----

# remove rows with 'NA' in Likelihood.to.recommend column
# clean_data <- nonvoters_data[!is.na(nonvoters_data$Likelihood.to.recommend),]

# remove information related to 2020 election/Covid-19; we are analyzing general reason why Americans do not vote
clean_data <- select(nonvoters_data, -c(Q1,    #all survey takers in data-set are citizens
                                        Q5,    #2020 elections
                                        Q8_4,  #CDC more related to COVID-19
                                        Q10_4, #related to COVID-19 / "past year"
                                        Q11_1, #Q11 related to 2020
                                        Q11_2,
                                        Q11_3,
                                        Q11_4,
                                        Q11_5,
                                        Q11_6,
                                        Q21,   #2020 elections related
                                        Q23,   #2020 elections related
                                        Q25,   #2020 elections related
                                        Q31,   #strength of party affiliation not necessary
                                        Q32,   #strength of party affiliation not necessary
                                        Q33))  #strength of party affiliation not necessary

#take a peak at clean_data to ensure columns are deleted
summary(clean_data)
glimpse(clean_data)

#create subset we want to study: people who rarely vote and sometimes vote
nonvoters <- subset(clean_data, clean_data$voter_category != "always")

#---- DATA ANALYSIS ----

#view distribution of age among non-voters
hist(nonvoters$ppage)
mean(nonvoters$ppage)

#BAR GRAPH OF FOR WHY NONVOTERS CHOOSE NOT TO VOTE

#Reason 1: Disliked all candidates
nonvoters %>%
  group_by(voter_category) %>%
  summarise(numberOfPeople=countNum(Q29_1)) %>%
  ggplot(aes(x = voter_category, y=numberOfPeople))  +
  geom_col() + 
  theme(axis.text.x= element_text(angle = 45, hjust= 1)) + 
  ggtitle("Disliked all candidates") +
  xlab("Type of Voter") + ylab("Number of Voters") +
  ylim(0, 400)

#Reason 2: Vote doesn't matter where I live
nonvoters %>%
  group_by(voter_category) %>%
  summarise(numberOfPeople=countNum(Q29_2)) %>%
  ggplot(aes(x = voter_category, y=numberOfPeople))  +
  geom_col() + 
  theme(axis.text.x= element_text(angle = 45, hjust= 1)) + 
  ggtitle("Vote doesn't matter where I live") +
  xlab("Type of Voter") + ylab("Number of Voters") +
  ylim(0, 400)

#Reason 3: Nothing will change no matter who wins
nonvoters %>%
  group_by(voter_category) %>%
  summarise(numberOfPeople=countNum(Q29_3)) %>%
  ggplot(aes(x = voter_category, y=numberOfPeople))  +
  geom_col() + 
  theme(axis.text.x= element_text(angle = 45, hjust= 1)) + 
  ggtitle("Nothing will change no matter who wins") +
  xlab("Type of Voter") + ylab("Number of Voters") +
  ylim(0, 400)

#Reason 4: System is too broken to be fixed by voting
nonvoters %>%
  group_by(voter_category) %>%
  summarise(numberOfPeople=countNum(Q29_4)) %>%
  ggplot(aes(x = voter_category, y=numberOfPeople))  +
  geom_col() + 
  theme(axis.text.x= element_text(angle = 45, hjust= 1)) + 
  ggtitle("System is too broken to be fixed by voting") +
  xlab("Type of Voter") + ylab("Number of Voters") +
  ylim(0, 400)

#Reason 5: Something came up
nonvoters %>%
  group_by(voter_category) %>%
  summarise(numberOfPeople=countNum(Q29_5)) %>%
  ggplot(aes(x = voter_category, y=numberOfPeople))  +
  geom_col() + 
  theme(axis.text.x= element_text(angle = 45, hjust= 1)) + 
  ggtitle("Something came up") +
  xlab("Type of Voter") + ylab("Number of Voters") +
  ylim(0, 400)


#Reason 6: Unsure of voter eligibility
nonvoters %>%
  group_by(voter_category) %>%
  summarise(numberOfPeople=countNum(Q29_6)) %>%
  ggplot(aes(x = voter_category, y=numberOfPeople))  +
  geom_col() + 
  theme(axis.text.x= element_text(angle = 45, hjust= 1)) + 
  ggtitle("Unsure of voter eligibility") +
  xlab("Type of Voter") + ylab("Number of Voters") +
  ylim(0, 400)

#Reason 7: Issues important to me not discussed
nonvoters %>%
  group_by(voter_category) %>%
  summarise(numberOfPeople=countNum(Q29_7)) %>%
  ggplot(aes(x = voter_category, y=numberOfPeople))  +
  geom_col() + 
  theme(axis.text.x= element_text(angle = 45, hjust= 1)) + 
  ggtitle("Issues important to me not discussed") +
  xlab("Type of Voter") + ylab("Number of Voters") +
  ylim(0, 400)

#Reason 8: Candidates are all the same
nonvoters %>%
  group_by(voter_category) %>%
  summarise(numberOfPeople=countNum(Q29_8)) %>%
  ggplot(aes(x = voter_category, y=numberOfPeople))  +
  geom_col() + 
  theme(axis.text.x= element_text(angle = 45, hjust= 1)) + 
  ggtitle("Candidates are all the same") +
  xlab("Type of Voter") + ylab("Number of Voters") +
  ylim(0, 400)

#Reason 9: Don't believe in voting
nonvoters %>%
  group_by(voter_category) %>%
  summarise(numberOfPeople=countNum(Q29_9)) %>%
  ggplot(aes(x = voter_category, y=numberOfPeople))  +
  geom_col() + 
  theme(axis.text.x= element_text(angle = 45, hjust= 1)) + 
  ggtitle("Don't believe in voting") +
  xlab("Type of Voter") + ylab("Number of Voters") +
  ylim(0, 400)


#BAR GRAPH FOR EACH BARRIER NONVOTERS FACE WHEN TRYING TO VOTE

#Barrier 1: Told they didn't have proper ID
nonvoters %>%
  group_by(voter_category) %>%
  summarise(numberOfPeople=countNum(Q18_1)) %>%
  ggplot(aes(x = voter_category, y=numberOfPeople))  +
  geom_col() + 
  theme(axis.text.x= element_text(angle = 45, hjust= 1)) + 
  ggtitle("Told they didn't have proper ID") +
  xlab("Type of Voter") + ylab("Number of Voters") +
  ylim(0, 600)

#Barrier 2: Couldn't find polling place
nonvoters %>%
  group_by(voter_category) %>%
  summarise(numberOfPeople=countNum(Q18_2)) %>%
  ggplot(aes(x = voter_category, y=numberOfPeople))  +
  geom_col() + 
  theme(axis.text.x= element_text(angle = 45, hjust= 1)) + 
  ggtitle("Couldn't find polling place") +
  xlab("Type of Voter") + ylab("Number of Voters") +
  ylim(0, 600)

#Barrier 3: Missed voter registration deadline
nonvoters %>%
  group_by(voter_category) %>%
  summarise(numberOfPeople=countNum(Q18_3)) %>%
  ggplot(aes(x = voter_category, y=numberOfPeople))  +
  geom_col() + 
  theme(axis.text.x= element_text(angle = 45, hjust= 1)) + 
  ggtitle("Missed voter registration deadline") +
  xlab("Type of Voter") + ylab("Number of Voters") +
  ylim(0, 600)


#Barrier 4: Unable to physically access polling place
nonvoters %>%
  group_by(voter_category) %>%
  summarise(numberOfPeople=countNum(Q18_4)) %>%
  ggplot(aes(x = voter_category, y=numberOfPeople))  +
  geom_col() + 
  theme(axis.text.x= element_text(angle = 45, hjust= 1)) + 
  ggtitle("Unable to physically access polling place") +
  xlab("Type of Voter") + ylab("Number of Voters") +
  ylim(0, 600)

#Barrier 5: Couldn't obtain necessary assistance to fill out ballot
nonvoters %>%
  group_by(voter_category) %>%
  summarise(numberOfPeople=countNum(Q18_5)) %>%
  ggplot(aes(x = voter_category, y=numberOfPeople))  +
  geom_col() + 
  theme(axis.text.x= element_text(angle = 45, hjust= 1)) + 
  ggtitle("Couldn't obtain necessary assistance to fill out ballot") +
  xlab("Type of Voter") + ylab("Number of Voters") +
  ylim(0, 600)

#Barrier 6: Required to cast provisional ballot
nonvoters %>%
  group_by(voter_category) %>%
  summarise(numberOfPeople=countNum(Q18_6)) %>%
  ggplot(aes(x = voter_category, y=numberOfPeople))  +
  geom_col() + 
  theme(axis.text.x= element_text(angle = 45, hjust= 1)) + 
  ggtitle("Required to cast provisional ballot") +
  xlab("Type of Voter") + ylab("Number of Voters") +
  ylim(0, 600)

#Barrier 7: Couldn't get off work
nonvoters %>%
  group_by(voter_category) %>%
  summarise(numberOfPeople=countNum(Q18_7)) %>%
  ggplot(aes(x = voter_category, y=numberOfPeople))  +
  geom_col() + 
  theme(axis.text.x= element_text(angle = 45, hjust= 1)) + 
  ggtitle("Couldn't get off work") +
  xlab("Type of Voter") + ylab("Number of Voters") +
  ylim(0, 600)

#Barrier 8: Wait more than 1 hour in line
nonvoters %>%
  group_by(voter_category) %>%
  summarise(numberOfPeople=countNum(Q18_8)) %>%
  ggplot(aes(x = voter_category, y=numberOfPeople))  +
  geom_col() + 
  theme(axis.text.x= element_text(angle = 45, hjust= 1)) + 
  ggtitle("Wait more than 1 hour in line") +
  xlab("Type of Voter") + ylab("Number of Voters") +
  ylim(0, 600)

#Barrier 9: Told name isn't listed even though they registered
nonvoters %>%
  group_by(voter_category) %>%
  summarise(numberOfPeople=countNum(Q18_9)) %>%
  ggplot(aes(x = voter_category, y=numberOfPeople))  +
  geom_col() + 
  theme(axis.text.x= element_text(angle = 45, hjust= 1)) + 
  ggtitle("Told name isn't listed even though they registered") +
  xlab("Type of Voter") + ylab("Number of Voters") +
  ylim(0, 600)

#Barrier 10: Absentee ballot delivered too late
nonvoters %>%
  group_by(voter_category) %>%
  summarise(numberOfPeople=countNum(Q18_10)) %>%
  ggplot(aes(x = voter_category, y=numberOfPeople))  +
  geom_col() + 
  theme(axis.text.x= element_text(angle = 45, hjust= 1)) + 
  ggtitle("Absentee ballot delivered too late") +
  xlab("Type of Voter") + ylab("Number of Voters") +
  ylim(0, 600)

#How many nonvoters find it easy to vote
hist(nonvoters$Q16)


#---- DATA MODELING ----

bars <- c('Q18_1', 'Q18_2', 'Q18_3', 'Q18_4', 'Q18_5', 'Q18_6', 'Q18_7', 'Q18_8', 'Q18_9', 'Q18_10')
timesHappened <- c(countNum(clean_data$Q18_1), countNum(clean_data$Q18_2), countNum(clean_data$Q18_3), countNum(clean_data$Q18_4), countNum(clean_data$Q18_5), countNum(clean_data$Q18_6), countNum(clean_data$Q18_7), countNum(clean_data$Q18_8), countNum(clean_data$Q18_9), countNum(clean_data$Q18_10))
barriers <- data.frame(bars, timesHappened)
barriers

#Linear model

# Build linear model to predict voter type based on Barriers to voting
model1 <- lm(medv ~ crim+rm+dis, clean_data)

summary(model1)

#9: One row df with values of predictors
predDF <- data.frame(crim = 0.26, dis = 3.2, rm = 6.2)

#10: predict new value of medv from df
predict(model1, predDF)

#view scatter plot to visualize the model
plot(clean_data$Q18_1, clean_data$voter_category)
abline(model1)


#SVM model

# Build SVM model to predict who will be a detractor
# use Eating.and.Drinking.at.Airport attribute + Type.of.Travel attribute

#convert is.Detractor to a factor
cleanSurvey$is.Detractor <- as.factor(cleanSurvey$is.Detractor)

#generate list of cases for training data
trainList <- createDataPartition(y=cleanSurvey$is.Detractor, p=0.05, list=FALSE)

#create training set
trainSet <- cleanSurvey[trainList,]
str(trainSet)

#create test set
testSet <- cleanSurvey[-trainList,]
str(testSet)

#Train SVM model
svmModel <- ksvm(is.Detractor ~ Eating.and.Drinking.at.Airport + Type.of.Travel, data=trainSet, C=5, cross=3)
svmModel


#Association Rules model

# Create association rules that predict if someone will be a nonvoter

#create a data frame with attributes that make sense to affect not voting
#convert those attributes to factors
surveyDF <- data.frame(status = as.factor(cleanSurvey$Airline.Status),
                       pricesense = as.factor(cleanSurvey$Price.Sensitivity),
                       loyalty = as.factor(cleanSurvey$Loyalty),
                       typeoftravel = as.factor(cleanSurvey$Type.of.Travel),
                       totalfreqflyeracc = as.factor(cleanSurvey$Total.Freq.Flyer.Accts),
                       shopamount = as.factor(round(cleanSurvey$Shopping.Amount.at.Airport, digits=-2)),
                       eatanddrink = as.factor(cleanSurvey$Eating.and.Drinking.at.Airport),
                       class = as.factor(cleanSurvey$Class),
                       partnername = as.factor(cleanSurvey$Partner.Name),
                       originstate = as.factor(cleanSurvey$Origin.State),
                       destinationstate = as.factor(cleanSurvey$Destination.State),
                       flightcancel = as.factor(cleanSurvey$Flight.cancelled),
                       arrivaldelay = as.factor(cleanSurvey$is.Arrival.Delayed),
                       detractor = cleanSurvey$is.Detractor)


#convert surveyDF to transactions data set
surveyTrans <- as(surveyDF, "transactions")


# Show top 10 rules based on confidence of rule
rules1	<- apriori(surveyTrans,	
                  parameter=list(supp=0.110,	conf=0.700),	
                  appearance=list(default="lhs",rhs=("detractor=TRUE")))

rules1
summary(rules1)
inspect(rules1)
plot(rules1)






#---- INSIGHTS ----

#











