#Climate change sensitivity project
#Web app prep

setwd("~/GitHub/ESA_climate_sensitivity_app_final")

#Install packages
library(plyr)
library(dplyr)
library(ggplot2)
library(esquisse)
library(forcats)
library(rio)
library(stringr)
library(tidyverse)

#####
#Importing data
#####

#IMPORT AND SAVE METADATA AS CSV
sensmeta = read.csv("data/sensitivity_metadata.csv", header = T)
dismeta = read.csv("data/discussion_metadata.csv", header = T)
#exporting data for web app download
saveRDS(sensmeta, file="data/sensitivity_metadata.rds")
saveRDS(dismeta, file="data/discussion_metadata.rds")


#PREPARE DATA
#clean version
#data = read.csv("Sensitivity database 2018 version Feb 2019.csv", header = T)
#data = read.csv("Sensitivity database 2019-04-15.csv", header = T) #includes Aimee's final Explanation and Source
data = read.csv("orig_database/Sensitivity database 2019-08-05.csv", header = T) #includes Aimee's FINAL Explanation and Source after Reviewer's comments

#removing species not assessed (extinct, etc)
remove.list <- paste(c("NOT ASSESSED", "Not assessed","not asssessed", "NOt ASSESSED"), collapse = '|')
data <- data %>% filter (!grepl(remove.list, Other.info))
data <- data[,c(1:12)]

#Add theme word for each question
data$Theme <- NA
data$Theme[data$Q.==1] <- "Temperature"
data$Theme[data$Q.==2] <- "Hydrology"
data$Theme[data$Q.==3] <- "Disturbance"
data$Theme[data$Q.==4] <- "Isolation"
data$Theme[data$Q.==5] <- "Injurious species"
data$Theme[data$Q.==6] <- "Chemistry"
data$Theme[data$Q.==7] <- "Phenology"
data$Theme[data$Q.==8] <- "Obligate relationships"
data$Theme <- as.factor(data$Theme)

#recoding Fish as Vertebrate
data$Taxon[data$Taxon=="Fish"] <- "Vertebrate"

#recoding Taxon to fix capitalization
data$Subtaxon <- as.character(data$Subtaxon)
data$Subtaxon[data$Subtaxon=="crustacean"] <- "Crustacean"
data$Subtaxon[data$Subtaxon=="fish"] <- "Fish"
data$Subtaxon[data$Subtaxon=="insect"] <- "Insect"
data$Subtaxon[data$Subtaxon=="crustacean"] <- "Crustacean"
data$Subtaxon[data$Subtaxon=="mollusc"] <- "Mollusk"
data$Subtaxon[data$Subtaxon=="arachnid"] <- "Arachnid"

#Separate inverts into 2 categories
data$Subtaxon[data$Subtaxon=="Crustacean"] <- "Arthropod"
data$Subtaxon[data$Subtaxon=="Insect"] <- "Arthropod"
data$Subtaxon[data$Subtaxon=="Arachnid"] <- "Arthropod"
data$Subtaxon[data$Subtaxon=="Mollusc"] <- "Mollusk"
data$Subtaxon[data$Subtaxon=="Clam"] <- "Mollusk"
data$Subtaxon[data$Subtaxon=="Snail"] <- "Mollusk"
data$Subtaxon <- factor(data$Subtaxon)

#reclassify Regions
data$Lead.Region[data$Lead.Region=="Region 5 "] <- "Region 5"
data$Lead.Region <- as.character(data$Lead.Region)
data$Lead.Region[data$Lead.Region=="Region 1"] <- "FWS Region 1: Pacific"
data$Lead.Region[data$Lead.Region=="Region 2"] <- "FWS Region 2: Southwest"
data$Lead.Region[data$Lead.Region=="Region 3"] <- "FWS Region 3: Midwest"
data$Lead.Region[data$Lead.Region=="Region 4"] <- "FWS Region 4: Southeast"
data$Lead.Region[data$Lead.Region=="Region 5"] <- "FWS Region 5: Northeast"
data$Lead.Region[data$Lead.Region=="Region 6"] <- "FWS Region 6: Mountain Prairie"
data$Lead.Region[data$Lead.Region=="Region 7"] <- "FWS Region 7: Alaska"
data$Lead.Region[data$Lead.Region=="Region 8"] <- "FWS Region 8: Pacific Southwest"
data$Lead.Region[data$Lead.Region=="NMFS"] <- "NMFS: Marine"
data$Lead.Region <- factor(data$Lead.Region)

#reclassify levels to be consistent
data$Sensitivity <- NA
#summary(data$Y.N)
data$Sensitivity[data$Y.N=="N" | data$Y.N=="n " | data$Y.N=="N?" | data$Y.N=="n"] <- "n"
data$Sensitivity[data$Y.N=="y" | data$Y.N=="Y " | data$Y.N=="Y" | data$Y.N=="Y?"] <- "y"
data$Sensitivity[data$Y.N=="? " | data$Y.N=="?" ] <- "?" #note that ?'s only exist in species for Q1-8
data$Sensitivity[data$Y.N=="n/y" | data$Y.N=="n/y?"] <- "n/y"
data$Sensitivity[data$Y.N=="y/n" | data$Y.N=="Y/n" | data$Y.N=="Y/N"] <- "y/n"
data$Sensitivity[data$Y.N=="y/na" | data$Y.N=="Y/na" | data$Y.N=="Y/Na" | data$Y.N=="Y/NA"] <- "y/na"
data$Sensitivity[data$Y.N=="y/study" | data$Y.N=="Y/study"] <- "y/study"
data$Sensitivity[data$Y.N=="n/study"] <- "n/study"
data$Sensitivity[data$Y.N=="not a threat"] <- "not a threat"
data$Sensitivity <- as.factor(data$Sensitivity)
data <- data[!is.na(data$Sensitivity),]
#summary(data$Sensitivity)


#replacing column names and labels for ease
colnames(data)[colnames(data) == 'Sensitivity'] <- 'Sens_Disc'
colnames(data)[colnames(data) == 'Scientific.Name'] <- 'Scientific_Name'
colnames(data)[colnames(data) == 'Theme'] <- 'Factor'
colnames(data)[colnames(data) == 'Q.'] <- 'Q_n'
data$Q_n <- as.character(data$Q_n)
data$Q_n <- as.numeric(data$Q_n)
colnames(data)[colnames(data) == 'Common.Name'] <- 'Common_Name'
colnames(data)[colnames(data) == 'Q.Text'] <- 'Question'
colnames(data)[colnames(data) == 'Lead.Region'] <- 'Lead_Region'

#Recoding Source for consistency
data$Source <- gsub("5-year", "five-year", data$Source)
data$Source <- gsub("5-Year", "five-year", data$Source)
data$Source <- gsub("5 Year", "five-year", data$Source)
data$Source <- gsub("5 year", "five-year", data$Source)
data$Source <- gsub("Five-year Review", "five-year review", data$Source)
data$Source <- gsub("Five-year review", "five-year review", data$Source)
data$Source <- gsub("five-year Review", "five-year review", data$Source)
data$Source <- gsub("Recovery Plan", "recovery plan", data$Source)
data$Source <- gsub("Listing decision", "listing decision", data$Source)
data$Source <- gsub("Listing Decision", "listing decision", data$Source)
data$Source <- gsub("Recovery plan", "recovery plan", data$Source)
data$Explanation <- gsub("5 Year", "five-year", data$Explanation)

#removing unnecessary columns
data$Other.info <- NULL
data$Y.N <- NULL

#separating into three datasets to recod discussion for three analyses of 1) species sensitivity, 2) is cc discussed? and 3) what level is discussed?
##1) species sensitivity
data8 <- subset(data, Q_n!=9)
#recoding abbreviations to full for web app legend
data8$Sens_Disc <- as.character(data8$Sens_Disc)
data8$Sens_Disc[data8$Sens_Disc=="y"] <- "Yes"
data8$Sens_Disc[data8$Sens_Disc=="n"] <- "No"
data8$Sens_Disc[data8$Sens_Disc=="?"] <- "Unknown"
data8$Sens_Disc <- as.factor(data8$Sens_Disc)
colnames(data8)[12] <- "Sensitive"

##2) is cc discussed? and 3) what level is discussed?
data9 <- subset(data, Q_n==9)
data9$CC_Discuss <- NA
data9$Discuss_Level <- NA
data9$Sens_Disc <- as.character(data9$Sens_Disc)

#coded for 4-15-19 reanalysis to split discussion data into two parts
data9$CC_Discuss[data9$Sens_Disc=="n"] <- "No"
data9$Discuss_Level[data9$Sens_Disc=="n"] <- "No discussion"

data9$CC_Discuss[data9$Sens_Disc=="y/n"] <- "Yes"
data9$Discuss_Level[data9$Sens_Disc=="y/n"] <- "No discussion"

data9$CC_Discuss[data9$Sens_Disc=="y/study"] <- "Yes"
data9$Discuss_Level[data9$Sens_Disc=="y/study"] <- "Further study"

data9$CC_Discuss[data9$Sens_Disc=="n/study"] <- "No"
data9$Discuss_Level[data9$Sens_Disc=="n/study"] <- "Further study"

data9$CC_Discuss[data9$Sens_Disc=="y"] <- "Yes"
data9$Discuss_Level[data9$Sens_Disc=="y"] <- "Action"

data9$CC_Discuss[data9$Sens_Disc=="n/y"] <- "No"
data9$Discuss_Level[data9$Sens_Disc=="n/y"] <- "Action"

data9$CC_Discuss[data9$Sens_Disc=="y/na"] <- "Yes"
data9$Discuss_Level[data9$Sens_Disc=="y/na"] <- "Excluded from analysis" #n=39 to remove b/c newly listed

data9$CC_Discuss[data9$Sens_Disc=="not a threat"] <- "Yes"
data9$Discuss_Level[data9$Sens_Disc=="not a threat"] <- "No threat, no action needed"

data9$CC_Discuss <- as.factor(data9$CC_Discuss)
data9$Discuss_Level <- as.factor(data9$Discuss_Level)
data9$Sens_Disc <- as.factor(data9$Sens_Disc)



#Merge year data with Q9, separate into other data frame
data.horiz <- data9
data.horiz <- data.horiz[,c(1,13,14)]

date.orig = read.csv("orig_database/species by date 2019-04-15.csv", header = T)
date <- date.orig[,c(1,13)]
colnames(date) <- c("Scientific_Name","Date")

data9.1 <- merge(data.horiz,date, by=("Scientific_Name"))
#data.horiz$Scientific_Name[!(data.horiz$Scientific_Name %in% date$Scientific_Name)] #checking difference

extra <- subset(data, Q_n==1)
extra <- extra[,c(1:6)]
data9.date <- merge(data9.1,extra,by=("Scientific_Name"))

#data9.date = discussion data; 1 row per species
#data8 = sensitivity data; 8 rows per species (for Q1-8)

#saving data files for download on web app
saveRDS(data8,file="data/Delach_et_al_2019_sensitivity_data.rds")
saveRDS(data9.date,file="data/Delach_et_al_2019_discussion_data.rds")
write.csv(data8,file="data/Delach_et_al_2019_sensitivity_data.csv")
write.csv(data9.date,file="data/Delach_et_al_2019_discussion_data.csv")

###Formatting date following Jacob's original code so it matches index file...don't fully understand this code
#Moved this state formatting code into this file so this file is strictly data prep; index file is web app design
dat <- data8 #Sensitivity factors
colnames(dat)[12] <- "Sens_Disc"
dat2 <- data9.date #Discussion and year

#creating State obj in both data frames
state_exp <- dat$State %>% str_replace_all("\r\n", "") %>%
  str_replace_all("OH WI", "OH, WI") %>%
  str_replace_all("IN IA", "IN, IA") %>%
  str_replace_all("Ak", "AK") %>%
  str_replace_all("AL ", "AL") %>%
  str_split(",|, ") %>%
  unlist() %>%
  str_trim("both")
table(state_exp)
dat$State_2 <- lapply(dat$State, function(x) {
  x %>% str_replace_all("\r\n", "") %>%
    str_replace_all("OH WI", "OH, WI") %>%
    str_replace_all("IN IA", "IN, IA") %>%
    str_replace_all("Ak", "AK") %>%
    str_replace_all("AL ", "AL") %>%
    str_split(",|, ") %>%
    unlist() %>%
    str_trim("both")
})

state_exp <- dat2$State %>% str_replace_all("\r\n", "") %>%
  str_replace_all("OH WI", "OH, WI") %>%
  str_replace_all("IN IA", "IN, IA") %>%
  str_replace_all("Ak", "AK") %>%
  str_replace_all("AL ", "AL") %>%
  str_split(",|, ") %>%
  unlist() %>%
  str_trim("both")
table(state_exp)
dat2$State_2 <- lapply(dat2$State, function(x) {
  x %>% str_replace_all("\r\n", "") %>%
    str_replace_all("OH WI", "OH, WI") %>%
    str_replace_all("IN IA", "IN, IA") %>%
    str_replace_all("Ak", "AK") %>%
    str_replace_all("AL ", "AL") %>%
    str_split(",|, ") %>%
    unlist() %>%
    str_trim("both")
})

#creating name with common and species
dat$combo_name <- paste0(dat$Common_Name, " (", dat$Scientific_Name, ")")
dat2$combo_name <- paste0(dat2$Common_Name, " (", dat2$Scientific_Name, ")")

#finalizing format
dat[c(1:6,7:14)] <- lapply(dat[c(1:6,7:14)], as.character)
dat2[c(1:2,4:10)] <- lapply(dat2[c(1:2,4:10)], as.character)


#save for loading in app script
saveRDS(dat, "data/sensitivity_data_prepped.rds")
saveRDS(dat2, "data/discussion_data_prepped.rds")


#####
#Unused code from Jacob's orig script - keep in case needed
#####

# Split q9
nq_9 <- filter(dat, Q != 9)
q9 <- filter(dat, Q == 9)
q9_ans <- q9$Sens_Disc
q12_ans <- q9$Sens_Disc

q12_pre <- select(q9, c(1:6))
q12_pre$Q_n <- 12
q12_pre$Q_Text <- "How is climate change management discussed in recovery materials?"
q12_pre$Q_response <- q12_ans
q12_pre$Explanation <- q11$Explanation
q12_pre$Source <- q11$Source
q12_pre$Other_info <- q11$Other_info
q12_pre$state_2 <- q11$state_2

q11_pre <- select(q11, c(1:6))
q11_pre$Q_n <- 11
q11_pre$Q_Text <- "Is climate change discussed as a threat?"
q11_pre$Q_response <- q11_ans
q11_pre$Explanation <- q11$Explanation
q11_pre$Source <- q11$Source
q11_pre$Other_info <- q11$Other_info
q11_pre$state_2 <- q11$state_2

q11_12 <- bind_rows(q11_pre, q12_pre)

final <- bind_rows(nq_11, q11_12)
table(final$Q_n)
final$combo_name <- paste0(final$Common_Name, " (", final$Scientific_Name, ")")
saveRDS(final, "data_clean.rds")

