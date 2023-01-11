rm(list = ls())

library(dplyr)
library(Metrics)
library(ggplot2)

setwd("C:/Users/cayde/OneDrive/Data Science/R Projects/Data Transformation/Cleaning Political Ambition")
pa_data_og <- read.csv("Political Ambition (Dirty).csv")


#Cleaning Data
pa_data <- pa_data_og
pa_data <- pa_data[-c(1:2), -c(1:8, 10:17)]

no_response <- c("659448", "762817", "391501", "802419", "759285", "345561", 
                 "558169", "523583", "706479", "462932", "100319", "338725")
pa_data <- pa_data[!pa_data$SurveyID %in% no_response,]

#Transforming Data

names(pa_data)

pa_data$Gender <- recode_factor(pa_data$Gender,
                             "Female" = 0,
                             "Male" = 1)

pa_data$College <- recode_factor(pa_data$Education,
                              "Some high school" = 0,
                              "High school graduate" = 0,
                              "Some college" = 0,
                              "2 year degree" = 0,
                              "4 year degree" = 1,
                              "Master's degree" = 1,
                              "Doctorate degree" = 1)

pa_data$Children <- recode_factor(pa_data$Children,
                               "No" = 0,
                               "Yes" = 1)    

pa_data$Income <- recode(pa_data$Income,
                             "Less than $10,000" = 1,
                             "$10,000 - $19,999" = 2,
                             "$20,000 - $29,999" = 3,
                             "$30,000 - $39,999" = 4,
                             "$40,000 - $49,999" = 5,
                             "$50,000 - $59,999" = 6,
                             "$60,000 - $69,999" = 7,
                             "$70,000 - $79,999" = 8,
                             "$80,000 - $89,999" = 9,
                             "$90,000 - $99,999" = 10,
                             "$100,000 - $149,999" = 11,
                             "More than $150,000" = 12)

for(i in which(names(pa_data) == "H1.Flattery"):which(names(pa_data) == "Competition")){
  pa_data[i] <- recode(pa_data[,i],
                           "Strongly disagree" = 1,
                           "Disagree" = 2,
                           "Neither agree nor disagree" = 3,
                           "Agree"  = 4,
                           "Strongly agree" = 5)
}

table(pa_data$Interest.Politics)

pa_data$Interest.Politics <- recode(pa_data$Interest.Politics,
                                        "Not interested at all" = 1,
                                        "Slightly interested" = 2,
                                        "Interested" = 3,
                                        "Very interested"  = 4)

pa_data$Discuss.Politics <- recode(pa_data$Discuss.Politics,
                                       "Never" = 1,
                                       "Occasionally" = 2,
                                       "Sometimes" = 3,
                                       "Frequently"  = 4)            

pa_data$Ideology <- recode(pa_data$Ideology,
                               "Very Liberal" = 1,
                               "Liberal" = 2,
                               "Somewhat liberal" = 3,
                               "Independent"  = 4,
                               "Somewhat conservative" = 5,
                               "Conservative" = 6,
                               "Very conservative" = 7)  

pa_data$PID <- recode(pa_data$PID,
                          "Democrat" = 1,
                          "Independent" = 2,
                          "Other" = 2,
                          "Republican" = 3)

pa_data$Political.Ambition <- recode(pa_data$Political.Ambition,
                                         "Never thought about running" = 1,
                                         "Considered running" = 2,
                                         "Seriously considered running" = 3,
                                         "Ran for office" = 4)

pa_data$Qualification <- recode(pa_data$Qualification,
                                    "Not at all qualified" = 1,
                                    "Somewhat qualified" = 2,
                                    "Qualified" = 3,
                                    "Very qualified" = 4)

pa_data$Winning <- recode(pa_data$Winning,
                              "Very unlikely" = 1,
                              "Unlikely" = 2,
                              "Likely" = 3,
                              "Very likely"  =4)

pa_data$Encourage.Run <- recode(pa_data$Encourage.Run,
                                    "Never" = 1,
                                    "Occasionally" = 2,
                                    "Sometimes" = 3,
                                    "Frequently" = 4)

pa_data$Job.Enjoyment <- recode(pa_data$Job.Enjoyment,
                                    "Dislike a great deal" = 1,
                                    "Dislike somewhat" = 2,
                                    "Neither like nor dislike" =3,
                                    "Like somewhat" = 4,
                                    "Like a great deal" = 5)

pa_data$Marital.Status <- recode(pa_data$Marital.Status,
                                     "Married" = 1,
                                     .default = 0)

pa_data$Ethnicity <- recode(pa_data$Ethnicity,
                                "White" = 1,
                                .default = 0)

#Cleaning Age
##############

#For those that put their birth year (numbers over 1000) I am subtracting the 
#year they took the survey to get their age 
pa_data$Age <- as.numeric(pa_data$Age)
pa_data$Age[pa_data$Age > 1000 & !is.na(pa_data$Age)] <- 2021 - pa_data$Age[pa_data$Age > 1000]

#Battery Creation
#################

#Subsetting the data to transform variables 
honesty <- pa_data[grepl("H\\d", names(pa_data))]
agreeableness <- pa_data[grepl("A\\d", names(pa_data))]
narcissism <- pa_data[grepl("N\\d", names(pa_data))]
machiavellianism <- pa_data[grepl("M\\d", names(pa_data))]
political_ambition <- pa_data[,c('Political.Ambition', "Qualification", "Winning", "Job.Enjoyment")]


#Changing the values for questions that are reverse coded
reverse_col_honesty <- c(2,4,5,7,8,10)
reverse_col_agreeableness <- c(2,3,4,10)

honesty[,reverse_col_honesty] <- 6 - honesty[,reverse_col_honesty]
agreeableness[,reverse_col_agreeableness] <- 6 - agreeableness[,reverse_col_agreeableness]


############################
#imputing missing variables
############################

sapply(pa_data, function(x){sum(is.na(x))}) #checking to see the missing data

#Personality Traits
###################

#Finds all the rows with NA values and calculates the mean of the row and rounds it
missing_data_honesty <- round(rowMeans(honesty[is.na(rowSums(honesty)),], na.rm = TRUE))

#Using the previous vector, goes through all the rows with NA values and replaces
#all NAs with the mean of the row 
for(row in names(missing_data_honesty)){
  honesty[which(row.names(honesty) == row),][is.na(honesty[which(row.names(honesty) == row),])] <- missing_data_honesty[row]
}

#Repeat this for the other variables

#Agreeableness
missing_data_agreeableness <- round(rowMeans(agreeableness[is.na(rowSums(agreeableness)),], na.rm = TRUE))

for(row in names(missing_data_agreeableness)){
  agreeableness[which(row.names(agreeableness) == row),][is.na(agreeableness[which(row.names(agreeableness) == row),])] <- missing_data_agreeableness[row]
}

#Narcissism
missing_data_narcissism <- round(rowMeans(narcissism[is.na(rowSums(narcissism)),], na.rm = TRUE))

for(row in names(missing_data_narcissism)){
  narcissism[which(row.names(narcissism) == row),][is.na(narcissism[which(row.names(narcissism) == row),])] <- missing_data_narcissism[row]
}

#Machiavellianism
missing_data_machiavellianism <- round(rowMeans(machiavellianism[is.na(rowSums(machiavellianism)),], na.rm = TRUE))

for(row in names(missing_data_machiavellianism)){
  machiavellianism[which(row.names(machiavellianism) == row),][is.na(machiavellianism[which(row.names(machiavellianism) == row),])] <- missing_data_machiavellianism[row]
}

#Imputing Age

pa_data$Age[is.na(pa_data$Age)] <- round(mean(pa_data$Age, na.rm = TRUE))


#Battery Creation

pa_data$honesty <- rowSums(honesty)
pa_data$agreeableness <- rowSums(agreeableness)
pa_data$machiavellianism <- rowSums(machiavellianism)
pa_data$narcissism <- rowSums(narcissism)
pa_data$political_ambition <- rowSums(political_ambition)


sapply(pa_data, function(x){sum(is.na(x))})

write.csv(pa_data, file = "Political Ambition (Cleaned).csv")
