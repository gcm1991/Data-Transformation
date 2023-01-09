rm(list = ls())

setwd("C:/Users/cayde/OneDrive/Graduate Studies/Research/Affective Polarization/Data")
pilot_data_raw <- read.csv("Dehumanization Pilot Data (Master).csv", header=FALSE, comment.char="#")

library(dplyr)

#Removing unnecessary rows and columns
pilot_data_draft <- pilot_data_raw #Creating a duplicate of the data to preserve the original 
pilot_data_draft <- pilot_data_raw[- c(1,2,3),] #removing question wording rows
pilot_data_draft <- pilot_data_draft[pilot_data_draft$V18 != "I do not wish to participate in the survey",] #removing people who opted out of the survey
pilot_data_draft <- pilot_data_draft[,-c(1:18)] #removing irrelevant meta data

#Renaming variables
pilot_data_draft <- rename(pilot_data_draft, "sex" = "V19")
pilot_data_draft <- rename(pilot_data_draft, "age" = "V20")
pilot_data_draft <- rename(pilot_data_draft, "race" = "V21")
pilot_data_draft <- rename(pilot_data_draft, "education" = "V22")
pilot_data_draft <- rename(pilot_data_draft, "PID" = "V23")

#Converting Party Identification to numeric
pilot_data_draft$PID7 <- recode(pilot_data_draft$PID, "Strong Democrat" = 1,
                                "Democrat" = 2,
                                "Lean Democrat" = 3,
                                "Independent" = 4,
                                "Lean Republican" = 5,
                                "Republican" = 6,
                                "Strong Republican" = 7)

table(pilot_data_draft$PID, pilot_data_draft$PID7) #checking the re-coding

#Creating new binary variable that measures partisanship
pilot_data_draft$partisan <- 0
pilot_data_draft$partisan[pilot_data_draft$PID != "Independent"] <- 1

#Getting Dehumanization, Feeling, and Meta ratings for Democrats and Republicans 
pilot_data_draft$ascent_D <- as.numeric(paste(pilot_data_draft$V24, pilot_data_draft$V34))
pilot_data_draft$ascent_R <- as.numeric(paste(pilot_data_draft$V25, pilot_data_draft$V33))

pilot_data_draft$therm_D <- as.numeric(paste(pilot_data_draft$V27, pilot_data_draft$V35))
pilot_data_draft$therm_R <- as.numeric(paste(pilot_data_draft$V26, pilot_data_draft$V36))

pilot_data_draft$meta_prejudice <- paste(pilot_data_draft$V28, pilot_data_draft$V37)
pilot_data_draft$meta_dehum <- paste(pilot_data_draft$V29, pilot_data_draft$V38)

#Re-coding individual batter items: Political Threats, harassment, and violence
pilot_data_draft$political_threats <- recode(pilot_data_draft$V30, "1 (never)" = 1,
                                             "2" = 2,
                                             "3" = 3,
                                             "4 (always)" =4)

pilot_data_draft$political_harassment <- recode(pilot_data_draft$V31, "1 (never)" = 1,
                                                "2" = 2,
                                                "3" = 3,
                                                "4 (always)" =4)

pilot_data_draft$political_violence <- recode(pilot_data_draft$V32, "1 (not at all)" = 1,
                                              "2" = 2,
                                              "3" = 3,
                                              "4" = 4,
                                              "5 (a great deal" = 5)

#Combining items into an index
pilot_data_draft$toxic <- pilot_data_draft$political_violence + pilot_data_draft$political_harassment + pilot_data_draft$political_threats

#Creating experiment assignment based on what version survey the respondent took
pilot_data_draft$experiment <- trimws(paste(pilot_data_draft$V57, pilot_data_draft$V58,  #Independents assigned the D/R) vignette
                                            pilot_data_draft$V64, pilot_data_draft$V68,  #Democrats and Republicans assigned to their Vignette
                                            pilot_data_draft$V63, pilot_data_draft$V67,  #Democrats and Republicans assigned to their Image
                                            pilot_data_draft$V54, pilot_data_draft$V55,  #Independents assigned the D/R Image 
                                            pilot_data_draft$V65, pilot_data_draft$V69,  #Democrats and Republicans assigned to their Image
                                            pilot_data_draft$V60, pilot_data_draft$V61)) #Independents assigned to the Republican or Democratic Graph

pilot_data_draft$experiment <- recode(pilot_data_draft$experiment, "FL_177" = "RCG",     #Due to Qualtrics, the control graph condition needs to be renamed
                                      "FL_181" = "DCG",
                                      "FL_186" = "RCG",
                                      "FL_190" = "DCG")

#Separating the experiment condition into three new columns. Each letter in the experiment assignment corresponds to a certain condition 
pilot_data_draft$version[sapply(pilot_data_draft$experiment, function(x){"I" %in% unlist(strsplit(x, split = ""))})] <- "Image"
pilot_data_draft$version[sapply(pilot_data_draft$experiment, function(x){"V" %in% unlist(strsplit(x, split = ""))})] <- "Vignette"
pilot_data_draft$version[sapply(pilot_data_draft$experiment, function(x){"G" %in% unlist(strsplit(x, split = ""))})] <- "Graph"

pilot_data_draft$target_party[sapply(pilot_data_draft$experiment, function(x){"R" %in% unlist(strsplit(x, split = ""))})] <- "Republican"
pilot_data_draft$target_party[sapply(pilot_data_draft$experiment, function(x){"D" %in% unlist(strsplit(x, split = ""))})] <- "Democrat"

pilot_data_draft$condition[sapply(pilot_data_draft$experiment, function(x){"T" %in% unlist(strsplit(x, split = ""))})] <- "Treatment"
pilot_data_draft$condition[sapply(pilot_data_draft$experiment, function(x){"C" %in% unlist(strsplit(x, split = ""))})] <- "Control"

#Renaming remaining Questions
# pilot_data_draft <- rename(pilot_data_draft, "trump_sat" = "V39")
# pilot_data_draft <- rename(pilot_data_draft, "trump_qual" = "V40")
# pilot_data_draft <- rename(pilot_data_draft, "trump_pres" = "V41")
# pilot_data_draft <- rename(pilot_data_draft, "hill_sat" = "V42")
# pilot_data_draft <- rename(pilot_data_draft, "hill_qual" = "V43")
# pilot_data_draft <- rename(pilot_data_draft, "hill_pres" = "V44")
# 
# pilot_data_draft <- rename(pilot_data_draft, "refuse_mask" = "V45")
# pilot_data_draft <- rename(pilot_data_draft, "misinfo" = "V46")
# pilot_data_draft <- rename(pilot_data_draft, "downplay_die" = "V47")
# pilot_data_draft <- rename(pilot_data_draft, "le_just" = "V48")
# pilot_data_draft <- rename(pilot_data_draft, "protestor_hurt" = "V49")
# pilot_data_draft <- rename(pilot_data_draft, "protestor_pepper" = "V50")

#Removing old variables that have been transformed or that are not needed for final dataset 
pilot_data_draft <- pilot_data_draft[,sapply(names(pilot_data_draft), function(x){!"V" %in% unlist(strsplit(x, split = ""))})]

write.csv(pilot_data_draft, file = "C:/Users/cayde/OneDrive/Data Science/R Projects/Data Transformation/Pilot Data Cleaned.csv")