#Climate change sensitivity project
#Cleaning dataset and analysis (#1)

#####
#Prepare workspace
#####

#PREPARE WORKSPACE
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
library(viridis)

#####
#Importing data
#####

#PREPARE DATA
#data = read.csv("Sensitivity database 2019-04-15.csv", header = T) #includes Aimee's final Explanation and Source
data = read.csv("orig_database/Sensitivity database 2019-08-05.csv", header = T) #includes Aimee's updates to Explanation and Source after Reviewer's comments for Revision 1 to Nature Climate Change

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

#reclassify levels to be consistent; later we break these down further for Q1-8 vs Q9
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


#####
#DATA PREP FOR ANALYSIS AND GRAPHING
#####

#new dataframe for Q1-8
data8 <- subset(data, Q.!=9)
#all these are yes or no or ?

#split off part 2 (question 9) data on recovery docs to recode data
data9 <- subset(data, Q.==9)
data9$CC_Discuss <- NA
data9$Discuss_Level <- NA
data9$Sensitivity <- as.character(data9$Sensitivity)

#coded for 4-15-19 reanalysis to split discussion data into two parts
data9$CC_Discuss[data9$Sensitivity=="n"] <- "No"
data9$Discuss_Level[data9$Sensitivity=="n"] <- "No discussion"

data9$CC_Discuss[data9$Sensitivity=="y/n"] <- "Yes"
data9$Discuss_Level[data9$Sensitivity=="y/n"] <- "No discussion"

data9$CC_Discuss[data9$Sensitivity=="y/study"] <- "Yes"
data9$Discuss_Level[data9$Sensitivity=="y/study"] <- "Further study"

data9$CC_Discuss[data9$Sensitivity=="n/study"] <- "No"
data9$Discuss_Level[data9$Sensitivity=="n/study"] <- "Further study"

data9$CC_Discuss[data9$Sensitivity=="y"] <- "Yes"
data9$Discuss_Level[data9$Sensitivity=="y"] <- "Action"

data9$CC_Discuss[data9$Sensitivity=="n/y"] <- "No"
data9$Discuss_Level[data9$Sensitivity=="n/y"] <- "Action"

data9$CC_Discuss[data9$Sensitivity=="y/na"] <- "Yes"
data9$Discuss_Level[data9$Sensitivity=="y/na"] <- "Not expected (newly listed)"

data9$CC_Discuss[data9$Sensitivity=="not a threat"] <- "Yes"
data9$Discuss_Level[data9$Sensitivity=="not a threat"] <- "No threat, no action needed"

data9$CC_Discuss <- as.factor(data9$CC_Discuss)
data9$Discuss_Level <- as.factor(data9$Discuss_Level)

data9$Discuss_Level <- factor(data9$Discuss_Level, levels(data9$Discuss_Level)[c(6,5,3,2,4,1)])

#adding regional level for all FWS; FWS n=435, NMFS n=24
data9$Region2 <- NULL
data9$Region2 <- revalue(data9$Lead.Region, c("FWS Region 1: Pacific"="FWS", "FWS Region 2: Southwest"="FWS",
                                              "FWS Region 3: Midwest"="FWS", "FWS Region 4: Southeast"="FWS",
                                              "FWS Region 5: Northeast"="FWS","FWS Region 6: Mountain Prairie"="FWS",
                                              "FWS Region 7: Alaska"="FWS","FWS Region 8: Pacific Southwest"="FWS",
                                              "NMFS: Marine"="NMFS"))

#####
#PLOTS AND ANALYSIS
#####

#####
#Palettes
#####

#sen <- c("#e8fa5bff","#f7cb44ff","#f9a242ff")
#cc <- c("#eb8055ff","#cc6a70ff", "black")
#dis <- c("#7e4e90ff","#593d9cff","#13306dff","#042333ff", "black")

sen <- c("#e8fa5bff","#f7cb44ff","#f9a242ff")
cc <- c("#de7065ff","#a65c85ff", "black")
dis <- c("#277f8eff","#37598cff","#45337dff","#440154ff", "black")

#####
#FIG 1A Sensitivity factors vs species; didn't use
#FIG 2B Sensitivity factors vs taxa
#####

#CREATE DATA
df <- data8
df.yn <- df %>%
          group_by(Scientific.Name, Sensitivity) %>%
          summarise(nfactors = n(), Taxon=first(Subtaxon)) %>%
          as.data.frame()
colnames(df.yn) <- c("Scientific.Name","Sensitivity","Factors","Taxon")
df.yn$Sensitivity <- as.factor(df.yn$Sensitivity)
summary(df.yn$Sensitivity)
df.y <- df.yn[which(df.yn$Sensitivity=="y"),]
df.y$Sensitivity <- NULL
#Hawaiian Goose has no 'yes' factors, so adding manually as 0
Scientific.Name <- "Branta sandvicensis"
Factors <- "0"
Taxon <- "Bird"
goose <- cbind(Scientific.Name,Factors)
goose <- cbind(goose,Taxon)
df.y2 <- rbind(df.y,goose)
tail(df.y2,10)
df.y2$Taxon <- as.factor(df.y2$Taxon)
df.y2$Factors <- as.numeric(df.y2$Factors)

###MANUSCRIPT FIG 1B,C ON DISCUSSION SECTION BELOW
###FIG 1A FACTORS BY SPECIES
df.y2$Factors <- as.factor(df.y2$Factors)
levels(df.y2$Factors) <- c(levels(df.y2$Factors),"8")
ggplot(df.y2, aes(x=Factors)) +
  geom_bar( fill="#f9a242ff") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title=element_text(hjust=-.18), legend.position='none',
        axis.text=element_text(colour="black", size=10),
        axis.title=element_text(size=10)) +
  scale_x_discrete(breaks=c("0","1","2","3","4","5","6","7","8"), drop=FALSE) +
  scale_y_continuous(limits=c(0,175), breaks=c(0,25,50,75,100,125,150,175)) +
  labs(x="Total number of sensitivity factors", y="Number of species")

#SAVE PLOT
ggsave("Fig 1A.tiff", device = "tiff", path = "C:/Users/jmiller/OneDrive - Defenders of Wildlife/Science support/Climate change/Sensitivity/Figures",
       width = 85, height = 70, units = "mm", dpi = 300)

###FIG 2B FACTORS BY TAXA
#Didn't end up using this in manuscript
df.y2$Factors <- as.numeric(df.y2$Factors)
ggplot(df.y2, aes(x=fct_reorder(Taxon, Factors, fun=median), y=Factors)) +
  geom_violin(fill="#f9a242ff", adjust=2, color=NA, weight=5) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title=element_text(hjust=-.4), legend.position='none',
        axis.text=element_text(colour="black", size=8),
        axis.title=element_text(size=10)) +
  scale_y_continuous(limits=c(0,8)) +
  labs(x="Taxon", y="Number of sensitivity factors") +
  stat_summary(fun.y=median, geom="point", size=3, color="black") +
  coord_flip()

#SAVE PLOT
ggsave("Fig 1B.tiff", device = "tiff", path = "C:/Users/jmiller/OneDrive - Defenders of Wildlife/Science support/Climate change/Sensitivity/Figures",
       width = 85, height = 70, units = "mm", dpi = 300)

#ANALYSIS
#breakdown by # sens factors
df2 <- NULL
df2 <- df.y2 %>%
  group_by(Factors) %>%
  summarise(species = n(),Pct = round(species/sum(df2$species) * 100,digits=1)) %>%
  as.data.frame()
df2

#stats
df.y2$Scientific.Name <- as.character(df.y2$Scientific.Name)
df.y2$Factors <- as.numeric(df.y2$Factors)
stats <- ddply(df.y2,~Taxon,summarise,median.factors=median(Factors),sd=sd(Factors), min=min(Factors),
               max=max(Factors), cv=(sd(Factors)/mean(Factors)))


#####
#FIG 2A matrix of factor sensitivity and taxonomic variation
#####

#create data
df.y3 <- data8
df.y3$Sensitivity[df.y3$Sensitivity=="?"] <- "n" #?'s = n for Q1-8
df.y3 <- data8 %>%
  group_by(Subtaxon, Theme, Sensitivity) %>%
  summarise(nspecies = n()) %>%
  group_by(Subtaxon, Theme) %>%
  mutate(freq = nspecies / sum(nspecies)) %>%
  as.data.frame()
colnames(df.y3) <- c("Taxon","Theme","Sensitivity","Nspecies","Proportion")
df.y3$Theme <- factor(df.y3$Theme, levels(df.y3$Theme)[c(8,3,2,5,4,1,7,6)])

#matrix
#creating data subset to only 'yes'
df.y4 <- df.y3[which(df.y3$Sensitivity=='y'),]
df.y4 <- df.y4[,c(-3,-4)]
#manually adding mollusk 'yes' row
de <- c("Mollusk","Phenology",0)
names(de) <- c("Taxon","Theme","Proportion")
df.y4 <- rbind(df.y4,de)
df.y4$Proportion <- as.numeric(df.y4$Proportion)
#graphing matrix plot
ggplot(df.y4, aes(x=Taxon, y=Theme,fill=Proportion)) +
  geom_tile() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title=element_text(hjust=-.18),
        legend.position='top', legend.justification='left', legend.direction='horizontal',
        axis.text.x=element_text(angle=30,hjust=1,colour="black",size=8),
        axis.text.y=element_text(colour="black", size=8),
        strip.background=element_blank(),
        strip.text.x = element_text(size=8),
        legend.text=element_text(size=8), legend.title=element_text(size=8)) +
  labs(x="Taxon", y="Sensitivity factor") +
  scale_fill_gradient(limits=c(0,1),breaks=c(0,0.5,1),low = "#e8fa5bff", high = "sienna3") +
  scale_y_discrete(limits=rev(levels(df.y4$Theme)),expand = c(0,0)) + scale_x_discrete(expand = c(0,0)) +
  guides(fill=guide_colourbar(title="Proportion of species sensitive",
                              title.position="top", ticks=FALSE))

#SAVE PLOT
ggsave("Fig 2A.tiff", device = "tiff", path = "C:/Users/jmiller/OneDrive - Defenders of Wildlife/Science support/Climate change/Sensitivity/Sensitivity_shared/Figures",
       width = 88, height = 100, units = "mm", dpi = 300)

#stacked bar plot (unused)
ggplot(df.y3, aes(Taxon)) +
  geom_bar(aes(weight=Proportion, fill=Sensitivity), position = position_stack(reverse=TRUE)) +
  facet_wrap(~Theme, ncol=2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title=element_text(hjust=-.18),
        legend.position='top', legend.justification='left',
        axis.text.x=element_text(angle=30,hjust=1,colour="black",size=7),
        axis.text.y=element_text(colour="black", size=8),
        strip.background=element_blank(),
        strip.text.x = element_text(size=8),
        legend.text=element_text(size=8), legend.title=element_text(size=8)) +
  labs(x="Taxon", y="Proportion of species") +
  scale_fill_manual(values=sen, labels=c("Unknown","No","Yes")) +
  guides(fill=guide_legend(title="Is the species sensitive to climate change?", title.position="top")) +
  scale_y_continuous(limits=c(0,1))

#SAVE PLOT
ggsave("Fig 1C.tiff", device = "tiff", path = "C:/Users/jmiller/OneDrive - Defenders of Wildlife/Science support/Climate change/Sensitivity/Figures",
       width = 88, height = 160, units = "mm", dpi = 300)

#ANALYSIS
newdata <- df.y3[order(df.y3$Theme, df.y3$Taxon),]
stats <- group_by(df.y3, Theme, Sensitivity)
stats <- summarise(stats, mean.prop=mean(Proportion), sd=sd(Proportion),
               min=min(Proportion), max=max(Proportion),cv=(sd(Proportion)/mean(Proportion)))


#####
#DISCUSSION FIGURES
#####

#New analysis to split discussion into two parts
#data9 #dataset for 'Is climate change discussed?'
data9.1 <- subset(data9, Discuss_Level!="Not expected (newly listed)") #dataset for 'What level of climate change management is discussed?'
data9.1$Discuss_Level <- as.character(data9.1$Discuss_Level)
data9.1$Discuss_Level <- as.factor(data9.1$Discuss_Level)
data9.1$Discuss_Level <- factor(data9.1$Discuss_Level, levels(data9.1$Discuss_Level)[c(3,4,2,1)])

###By CC DISCUSSION

#####
#FIG. 1B CLIMATE CHANGE DISCUSSION BY NUMBER OF SPECIES
#####
#create dataset (number of species)
df.y2 <- data9 %>%
  group_by(CC_Discuss) %>%
  summarise(nspecies = n()) %>%
  as.data.frame()
colnames(df.y2) <- c("CC_Discussion","Nspecies")

#FIG 1B BAR CHART CLIMATE CHANGE DISCUSSION
ggplot(data9,aes(x=CC_Discuss)) +
  geom_bar(width=0.5,aes(fill=factor(CC_Discuss)))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title=element_text(hjust=-.18), legend.position='none',
        axis.text=element_text(colour="black", size=8),
        axis.title=element_text(size=10)) +
  labs(x="Discussion", y="Number of species", title="") +
  scale_fill_manual(values=cc)

#SAVE PLOT
ggsave("Fig 6a.tiff", device = "tiff", path = "C:/Users/jmiller/OneDrive - Defenders of Wildlife/Science support/Climate change/Sensitivity/Figures",
       width = 85, height = 70, units = "mm", dpi = 300)

#ANALYSIS
summary(data.full$CC_Discuss)
165/459 #no
294/459 #yes


#####
#DISCUSSION BY DATE
#REGRESSION ANALYSIS
#####

#bring in date
date.orig = read.csv("species by date 2019-04-15.csv", header = T)
date <- date.orig[,c(1,13)]
data.full <- merge(data9,date, by=("Scientific.Name"))
#data.full <- data.full[!(data.full$Scientific.Name=="Branta sandvicensis"),] #Exclude HI Goose, not sensitive
#data9$Scientific.Name[!(data9$Scientific.Name %in% date$Scientific.Name)]

#lump pre-2007 into one category
data.full$Date.of.most.recent.doc[data.full$Date.of.most.recent.doc<=2006] <- "1973-2006"
data.full$Date.of.most.recent.doc <- as.factor(data.full$Date.of.most.recent.doc)

#CREATING DF.Y2 of # sensitivity factors
#Used in manuscript
df <- data8
df.yn <- df %>%
  group_by(Scientific.Name, Sensitivity) %>%
  summarise(nfactors = n(), Taxon=first(Subtaxon)) %>%
  as.data.frame()
colnames(df.yn) <- c("Scientific.Name","Sensitivity","Factors","Taxon")
df.yn$Sensitivity <- as.factor(df.yn$Sensitivity)
summary(df.yn$Sensitivity)
df.y <- df.yn[which(df.yn$Sensitivity=="y"),]
df.y$Sensitivity <- NULL
#Hawaiian Goose has no 'yes' factors, so adding manually as 0
Scientific.Name <- "Branta sandvicensis"
Factors <- "0"
Taxon <- "Bird"
goose <- cbind(Scientific.Name,Factors)
goose <- cbind(goose,Taxon)
df.y2 <- rbind(df.y,goose)
tail(df.y2,10)
df.y2$Taxon <- as.factor(df.y2$Taxon)
df.y2$Factors <- as.numeric(df.y2$Factors)

#Analysis of factors by discussion level
data.full1 <- merge(data.full,df.y2, by=("Scientific.Name")) #df.y2 from fig 1A
df1 <- data.full1[,c("Scientific.Name","Factors","CC_Discuss", "Discuss_Level")]

#RESPONSE TO REVIEWER 3: SENSITIVITY ANALYSIS TO DROP 'ISOLATION' FACTOR
#only testing for response; don't run in true analysis
#data prep, then run regression below
dz<- data8
dz1 <- dz[which(dz$Theme!='Isolation'),]
df.yn <- dz1 %>%
  group_by(Scientific.Name, Sensitivity) %>%
  summarise(nfactors = n(), Taxon=first(Subtaxon)) %>%
  as.data.frame()
colnames(df.yn) <- c("Scientific.Name","Sensitivity","Factors","Taxon")
df.yn$Sensitivity <- as.factor(df.yn$Sensitivity)
summary(df.yn$Sensitivity)
df.y <- df.yn[which(df.yn$Sensitivity=="y"),]
df.y$Sensitivity <- NULL
#Hawaiian Goose has no 'yes' factors, so adding manually as 0
Scientific.Name <- "Branta sandvicensis"
Factors <- "0"
Taxon <- "Bird"
goose <- cbind(Scientific.Name,Factors)
goose <- cbind(goose,Taxon)
df.y2 <- rbind(df.y,goose)
tail(df.y2,10)
df.y2$Taxon <- as.factor(df.y2$Taxon)
df.y2$Factors <- as.numeric(df.y2$Factors)
#Analysis of factors by discussion level
data.full1 <- merge(data.full,df.y2, by=("Scientific.Name")) #df.y2 from fig 1A
df1 <- data.full1[,c("Scientific.Name","Factors","CC_Discuss", "Discuss_Level")]


#ANALYSIS
#stats on prop species per year
#connection broken, will need to update 8/1/19
new <- subset(df.y2,Date %in% c("2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018"))
aggregate(new$Proportion, by=list(Category=new$CC_Discussion), FUN=summary)

#REGRESSION BTW SCORE AND ACTION
#calculate Chi Sq test; p value is "correlation", closer to 1 = higher correlation
#chi1 <- chisq.test(table(df1[,2:3])) #cc as a threat
#calculate Crammer's V as a measure of correlation; close to 0 = very correlated
#sqrt(chi1$statistic / 459) #cc
#logistic regression; used this in paper
logit <- glm(CC_Discuss ~ Factors, data=df1, family="binomial")
summary(logit)
xtabs(~CC_Discuss + Factors, data=df1)
exp(coef(logit)) #odds of no action (because no = 1)
1- (exp(0.15084)/(1+exp(0.15084))) #probability of yes

df2 <- subset(df1, Discuss_Level!="Not expected (newly listed)") #remove species listed
df2$Discuss_Level <- ifelse(df2$Discuss_Level == "Action","Action","No action")
df2$Discuss_Level <- as.factor(df2$Discuss_Level)
#chi2 <- chisq.test(table(df2[,c(2,4)])) #discussion level
#sqrt(chi2$statistic / 459) #disc level
#logistic regression
logit <- glm(Discuss_Level ~ Factors, data=df2, family="binomial")
summary(logit)
xtabs(~Discuss_Level + Factors, data=df2)
exp(coef(logit)) #odds of no action (because no action = 1)
1- (exp(0.3106)/(1+exp(0.3106))) #probability of action
predict(logit, data.frame(Factors=7),type="response")
100-90
100-59


#stats on prop species per year
new <- subset(df2,Date %in% c("2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018"))
aggregate(new$Proportion, by=list(Category=new$CC_Discussion), FUN=summary)

#create dataset for plot
df.y2 <- data.full %>%
  group_by(Date.of.most.recent.doc, CC_Discuss) %>%
  summarise(nspecies = n()) %>%
  group_by(Date.of.most.recent.doc) %>%
  mutate(freq = nspecies / sum(nspecies)) %>%
  as.data.frame()
colnames(df.y2) <- c("Date","CC_Discussion","Nspecies","Proportion")
df.y2$Date <- as.factor(df.y2$Date)
#df.y2$Date <- factor(df.y2$Date, levels(df.y2$Date)[c(14,1:13)])

#stacked bar plot (proportional)
ggplot(df.y2, aes(Date)) +
  geom_bar(aes(weight=Proportion, fill=CC_Discussion), position = position_stack(reverse=TRUE)) +
  labs(x="Date", y="Proportion of species") +
 # ggtitle("a") +
  scale_fill_manual(values=cc) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title=element_text(hjust=-.22), legend.position='none',
        legend.text=element_text(size=8),
        axis.text.y=element_text(colour="black", size=8),
        axis.text.x=element_text(colour="black",angle=55, hjust=1, size=8),
        axis.title=element_text(size=10)) +
  guides(fill=guide_legend(nrow=2,byrow=TRUE, title.position="top")) +
  annotate("text", y = -.03, x="1973-2006", label="46", colour="black", size=3) +
  annotate("text", y = -.03, x="2007", label="21", colour="black", size=3) +
  annotate("text", y = -.03, x="2008", label="32", colour="black", size=3) +
  annotate("text", y = -.03, x="2009", label="43", colour="black", size=3) +
  annotate("text", y = -.03, x="2010", label="37", colour="black", size=3) +
  annotate("text", y = -.03, x="2011", label="44", colour="black", size=3) +
  annotate("text", y = -.03, x="2012", label="59", colour="black", size=3) +
  annotate("text", y = -.03, x="2013", label="55", colour="black", size=3) +
  annotate("text", y = -.03, x="2014", label="37", colour="black", size=3) +
  annotate("text", y = -.03, x="2015", label="31", colour="black", size=3) +
  annotate("text", y = -.03, x="2016", label="32", colour="black", size=3) +
  annotate("text", y = -.03, x="2017", label="7", colour="black", size=3) +
  annotate("text", y = -.03, x="2018", label="15", colour="black", size=3)

#SAVE PLOT
ggsave("Fig 2b.tiff", device = "tiff", path = "C:/Users/jmiller/OneDrive - Defenders of Wildlife/Science support/Climate change/Sensitivity/Sensitivity_shared/Figures",
       width = 85, height = 70, units = "mm", dpi = 300)

#plot to save horizontal legend
ggplot(df.y2, aes(Date)) +
  geom_bar(aes(weight=Proportion, fill=CC_Discussion), position = position_stack(reverse=TRUE)) +
  labs(x="Date", y="Proportion of species") +
#  ggtitle("(a)") +
  scale_fill_manual(values=cc, name="Is climate change discussed as a potential threat?") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title=element_text(hjust=-.17,vjust=-33), legend.position='top',
        legend.text=element_text(size=8), legend.title=element_text(size=8)) +
  guides(fill=guide_legend(title.position="top"))

#SAVE PLOT LEGEND
ggsave("Fig 6b legend.tiff", device= "tiff", path = "C:/Users/jmiller/OneDrive - Defenders of Wildlife/Science support/Climate change/Sensitivity/Figures",
       width = 85, height = 70, units = "mm", dpi = 300)


#####
#FIG. 1C NUMBER OF SPECIES BY DISCUSSION
#FIG. 1A NUMBER SENSITIVITY FACTORS BY NUMBER AND PERCENTAGE OF SPECIES
#####
#df1 is all 459 species, df2 is 420 species

#cc discussion data
#to test Reviewer 3 drop Isolation, run df1 and df2 above with Reviewer 3 script
dfcc <- df1 %>%
  group_by(Factors,CC_Discuss) %>%
  summarise(nspecies = n()) %>%
  group_by(Factors) %>%
  mutate(freq = nspecies / sum(nspecies)) %>%
  as.data.frame()
colnames(dfcc) <- c("Factors","Discuss","Nspecies","Proportion")
dfcc <- dfcc[which(dfcc$Discuss=='Yes'),]
#action discussion
dfdis <- df2 %>%
  group_by(Factors,Discuss_Level) %>%
  summarise(nspecies = n()) %>%
  group_by(Factors) %>%
  mutate(freq = nspecies / sum(nspecies)) %>%
  as.data.frame()
colnames(dfdis) <- c("Factors","Discuss","Nspecies","Proportion")
dfdis <- dfdis[which(dfdis$Discuss=='Action'),]
#merge into same frame
df.y5 <- rbind(dfcc,dfdis)
df.y5 <- rbind(df.y5,data.frame(Factors=0,Discuss="Action",Nspecies=0,Proportion=0))
df.y5 <- rbind(df.y5,data.frame(Factors=0,Discuss="Yes",Nspecies=0,Proportion=0))
df.y5$Discuss <- droplevels(df.y5$Discuss)

#histogrph data of number factors by number species
df <- data8
#df <- df[which(df$Theme!='Isolation'),]
df.yn <- df %>%
  group_by(Scientific.Name, Sensitivity) %>%
  summarise(nfactors = n(), Taxon=first(Subtaxon)) %>%
  as.data.frame()
colnames(df.yn) <- c("Scientific.Name","Sensitivity","Factors","Taxon")
df.yn$Sensitivity <- as.factor(df.yn$Sensitivity)
summary(df.yn$Sensitivity)
df.y <- df.yn[which(df.yn$Sensitivity=="y"),]
df.y$Sensitivity <- NULL
#Hawaiian Goose has no 'yes' factors, so adding manually as 0
Scientific.Name <- "Branta sandvicensis"
Factors <- "0"
Taxon <- "Bird"
goose <- cbind(Scientific.Name,Factors)
goose <- cbind(goose,Taxon)
df.y2 <- rbind(df.y,goose)
tail(df.y2,10)
df.y2$Taxon <- as.factor(df.y2$Taxon)
df.y2$Factors <- as.numeric(df.y2$Factors)

df6 <- df.y2 %>%
  group_by(Factors) %>%
  summarise(nspecies = n()) %>%
  as.data.frame()
colnames(df6) <- c("Factors","Nspecies")

df.y5$Percentage <- NULL
df.y5$Percentage <- df.y5$Proportion*100

#FIG 1A FACTORS BY SPECIES
ggplot() +
  geom_bar(data=df6, aes(x=Factors, y=Nspecies), stat="identity",fill="#f9a242ff") +
  geom_line(data=df.y5, aes(x=Factors, y=(Percentage*2), group=Discuss, colour=Discuss ), size=1) +
  geom_point(data=df.y5, aes(x=Factors, y=(Percentage*2), group=Discuss, colour=Discuss ), size=2) +
  scale_color_manual(values=c("#a65c85ff", "#440154ff")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title=element_text(hjust=-.18),
        axis.text=element_text(colour="black", size=8),
        axis.title=element_text(size=10), legend.position="none") +
  scale_y_continuous(name="Number of species", limits=c(0,200), breaks=c(0,50,100,150,200),
                     sec.axis=sec_axis(trans=~./2,name="Percentage of species",
                                       breaks=waiver())) +
  labs(x="Total number of sensitivity factors")

#SAVE PLOT
ggsave("Fig 1A.tiff", device = "tiff", path = "C:/Users/jmiller/OneDrive - Defenders of Wildlife/Science support/Climate change/Sensitivity/Sensitivity_shared/Figures",
       width = 85, height = 70, units = "mm", dpi = 300)
ggsave("Review3.tiff", device = "tiff", path = "C:/Users/jmiller/OneDrive - Defenders of Wildlife/Science support/Climate change/Sensitivity/Sensitivity_shared/Figures",
       width = 85, height = 70, units = "mm", dpi = 300)


#STACKED BAR PLOT
#create dataset (proportional)
df.y2 <- data9 %>%
  group_by(Subtaxon, CC_Discuss) %>%
  summarise(nspecies = n()) %>%
  group_by(Subtaxon) %>%
  mutate(freq = nspecies / sum(nspecies)) %>%
  as.data.frame()
colnames(df.y2) <- c("Taxon","CC_Discussion","Nspecies","Proportion")
df.y2$Taxon <- factor(df.y2$Taxon, levels(df.y2$Taxon)[c(7,5,4,2,6,8,3)])

#stacked bar plot (proportional)
ggplot(df.y2, aes(Taxon)) +
  geom_bar(aes(weight=Proportion, fill=CC_Discussion), position = position_stack(reverse=TRUE)) +
  labs(x="Taxon", y="Proportion of species") +
#  ggtitle("b") +
  scale_fill_manual(values=cc) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title=element_text(hjust=-.21), legend.position='none',
        legend.text=element_text(size=8),
        axis.text.y=element_text(colour="black", size=8),
        axis.text.x=element_text(colour="black",angle=55, hjust=1, size=8),
        axis.title=element_text(size=10))+
  guides(fill=guide_legend(nrow=2,byrow=TRUE, title.position="top")) +
  annotate("text", y = -0.03, x="Amphibian", label="19", colour="black",size=3) +
  annotate("text", y = -0.03, x="Bird", label="62", colour="black",size=3) +
  annotate("text", y = -0.03, x="Fish", label="83", colour="black",size=3) +
  annotate("text", y = -0.03, x="Mollusk", label="107", colour="black",size=3) +
  annotate("text", y = -0.03, x="Arthropod", label="110", colour="black",size=3) +
  annotate("text", y = -0.03, x="Mammal", label="62", colour="black",size=3) +
  annotate("text", y = -0.03, x="Reptile", label="16", colour="black",size=3)

#SAVE PLOT
ggsave("Fig 6c.tiff", device= "tiff", path = "C:/Users/jmiller/OneDrive - Defenders of Wildlife/Science support/Climate change/Sensitivity/Figures",
       width = 85, height = 70, units = "mm", dpi = 300)


#####
#REGION BY CC DISCUSSION
#####
#Fig 3C
#create dataset (proportional)
df.y2 <- data9 %>%
  group_by(Lead.Region, CC_Discuss) %>%
  summarise(nspecies = n()) %>%
  group_by(Lead.Region) %>%
  mutate(freq = nspecies / sum(nspecies)) %>%
  as.data.frame()
colnames(df.y2) <- c("Region","CC_Discussion","Nspecies","Proportion")
df.y2$Region <- factor(df.y2$Region, levels(df.y2$Region)[c(6,5,7,9,10,3,2,4,8)])

#stacked bar plot (proportional)
ggplot(df.y2, aes(Region)) +
  geom_bar(aes(weight=Proportion, fill=CC_Discussion), position = position_stack(reverse=TRUE)) +
  labs(x="Agency and region", y="Proportion of species") +
#  ggtitle("c") +
  scale_fill_manual(values=cc) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title=element_text(hjust=-.21), legend.position='none',
        legend.text=element_text(size=8),
        axis.text.y=element_text(colour="black", size=8),
        axis.text.x=element_text(colour="black",angle=55, hjust=1, size=8),
        axis.title=element_text(size=10))+
  guides(fill=guide_legend(nrow=2,byrow=TRUE, title.position="top")) +
  annotate("text", y = -.03, x="NMFS: Marine", label="24", colour="black",size=3) +
  annotate("text", y =  -.03, x="FWS Region 2: Southwest", label="79", colour="black",size=3) +
  annotate("text", y =  -.03, x="FWS Region 6: Mountain Prairie", label="14", colour="black",size=3) +
  annotate("text", y =  -.03, x="FWS Region 5: Northeast", label="20", colour="black",size=3) +
  annotate("text", y =  -.03, x="FWS Region 1: Pacific", label="88", colour="black",size=3) +
  annotate("text", y =  -.03, x="FWS Region 8: Pacific Southwest", label="78", colour="black",size=3) +
  annotate("text", y =  -.03, x="FWS Region 4: Southeast", label="131", colour="black",size=3) +
  annotate("text", y =  -.03, x="FWS Region 3: Midwest", label="24", colour="black",size=3) +
  annotate("text", y =  -.03, x="FWS Region 7: Alaska", label="1", colour="black",size=3)

#SAVE PLOT
ggsave("Fig 6d.tiff", device= "tiff", path = "C:/Users/jmiller/OneDrive - Defenders of Wildlife/Science support/Climate change/Sensitivity/Sensitivity_shared/Figures",
       width = 85, height = 100, units = "mm", dpi = 300)

#Plot by FWS vs NMFS
#create dataset (proportional)
df.y2 <- data9 %>%
  group_by(Region2, CC_Discuss) %>%
  summarise(nspecies = n()) %>%
  group_by(Region2) %>%
  mutate(freq = nspecies / sum(nspecies)) %>%
  as.data.frame()
colnames(df.y2) <- c("Region","CC_Discussion","Nspecies","Proportion")
#df.y2$Region <- factor(df.y2$Region, levels(df.y2$Region)[c(6,5,7,9,10,3,2,4,8)])

#stacked bar plot (proportional)
ggplot(df.y2, aes(Region)) +
  geom_bar(aes(weight=Proportion, fill=CC_Discussion), position = position_stack(reverse=TRUE)) +
  labs(x="Agency", y="Proportion of species") +
  #  ggtitle("c") +
  scale_fill_manual(values=cc) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title=element_text(hjust=-.21), legend.position='none',
        legend.text=element_text(size=8),
        axis.text.y=element_text(colour="black", size=8),
        axis.text.x=element_text(colour="black",angle=55, hjust=1, size=8),
        axis.title=element_text(size=10))+
  guides(fill=guide_legend(nrow=2,byrow=TRUE, title.position="top"))+
  annotate("text", y = -.03, x="FWS", label="435", colour="black",size=3) +
  annotate("text", y =  -.03, x="NMFS", label="24", colour="black",size=3)

#SAVE PLOT
ggsave("Fig agency cc supp.tiff", device= "tiff", path = "C:/Users/jmiller/OneDrive - Defenders of Wildlife/Science support/Climate change/Sensitivity/Sensitivity_shared/Figures",
       width = 85, height = 100, units = "mm", dpi = 300)


#####
#BY LEVEL OF DISCUSSION
#####
#Fig 1C?
#DISCUSSION BY NUMBER OF SPECIES
#create dataset (number of species)
df.y2 <- NULL
df.y2 <- data9.1 %>%
  group_by(Discuss_Level) %>%
  summarise(nspecies = n(),Pct = round(nspecies/sum(df.y2$nspecies) * 100,digits=1)) %>%
  as.data.frame()
colnames(df.y2) <- c("Discussion","Nspecies")
df.y2

ggplot(data9.1,aes(x=Discuss_Level)) +
  geom_bar(width=0.5,aes(fill=factor(Discuss_Level)))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title=element_text(hjust=-.18), legend.position='none',
        axis.text=element_text(colour="black", size=8),
        axis.title=element_text(size=10)) +
  labs(x="Discussion level", y="Number of species", title="") +
  scale_fill_manual(values=dis)+
  scale_y_continuous(limits=c(0,350),breaks=c(0,50,100,150,200,250,300,350)) +
  scale_x_discrete(labels=c("No discussion","No threat,\nno action\nneeded","Further\nstudy","Action"))

#SAVE PLOT
ggsave("Fig 2e.tiff", device = "tiff", path = "C:/Users/jmiller/OneDrive - Defenders of Wildlife/Science support/Climate change/Sensitivity/Figures",
       width = 85, height = 70, units = "mm", dpi = 300)

######
#DISCUSSION BY DATE
#####

#bring in date
date.orig = read.csv("species by date 2019-04-15.csv", header = T)
date <- date.orig[,c(1,13)]
data.full <- merge(data9.1,date, by=("Scientific.Name"))
#data.full <- data.full[!(data.full$Scientific.Name=="Branta sandvicensis"),] #Exclude HI Goose, not sensitive
#data9$Scientific.Name[!(data9$Scientific.Name %in% date$Scientific.Name)]

#lump pre-2007 into one category
data.full$Date.of.most.recent.doc[data.full$Date.of.most.recent.doc<=2006] <- "1973-2006"
data.full$Date.of.most.recent.doc <- as.factor(data.full$Date.of.most.recent.doc)

#create dataset for plot
df.y2 <- data.full %>%
  group_by(Date.of.most.recent.doc, Discuss_Level) %>%
  summarise(nspecies = n()) %>%
  group_by(Date.of.most.recent.doc) %>%
  mutate(freq = nspecies / sum(nspecies)) %>%
  as.data.frame()
colnames(df.y2) <- c("Date","Discussion","Nspecies","Proportion")
df.y2$Date <- as.factor(df.y2$Date)
#manually adding 2006 and Pre-2006 since no date but want in graph
a <- c(2006,"","","")
names(a) <- c("Date","Discussion","Nspecies","Proportion")
b <- c("1973-2005","","","")
names(b) <- c("Date","Discussion","Nspecies","Proportion")
c <- rbind(a,b)
c <- as.data.frame(c)
df.y2 <- rbind(df.y2,c)
df.y2$Date <- as.factor(df.y2$Date)
df.y2$Nspecies <- as.numeric(df.y2$Nspecies)
df.y2$Proportion <- as.numeric(df.y2$Proportion)

#stacked bar plot (proportional)
#Fig 4b
ggplot(df.y2, aes(Date)) +
  geom_bar(aes(weight=Proportion, fill=Discussion), position = position_stack(reverse=TRUE)) +
  labs(x="Date", y="Proportion of species") +
  scale_fill_manual(values=dis) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title=element_text(hjust=-.22), legend.position='none',
        legend.text=element_text(size=8),
        axis.text.y=element_text(colour="black", size=8),
        axis.text.x=element_text(colour="black",angle=55, hjust=1, size=8),
        axis.title=element_text(size=10)) +
  guides(fill=guide_legend(nrow=2,byrow=TRUE, title.position="top")) +
  annotate("text", y = -.03, x="1973-2006", label="46", colour="black", size=3) +
  annotate("text", y = -.03, x="2007", label="21", colour="black", size=3) +
  annotate("text", y = -.03, x="2008", label="32", colour="black", size=3) +
  annotate("text", y = -.03, x="2009", label="43", colour="black", size=3) +
  annotate("text", y = -.03, x="2010", label="35", colour="black", size=3) +
  annotate("text", y = -.03, x="2011", label="43", colour="black", size=3) +
  annotate("text", y = -.03, x="2012", label="53", colour="black", size=3) +
  annotate("text", y = -.03, x="2013", label="52", colour="black", size=3) +
  annotate("text", y = -.03, x="2014", label="36", colour="black", size=3) +
  annotate("text", y = -.03, x="2015", label="22", colour="black", size=3) +
  annotate("text", y = -.03, x="2016", label="16", colour="black", size=3) +
  annotate("text", y = -.03, x="2017", label="6", colour="black", size=3) +
  annotate("text", y = -.03, x="2018", label="15", colour="black", size=3)

#tapply(df.y2$Nspecies, df.y2$Date, FUN=sum)

#SAVE PLOT
ggsave("Fig 2f.tiff", device = "tiff", path = "C:/Users/jmiller/OneDrive - Defenders of Wildlife/Science support/Climate change/Sensitivity/Sensitivity_shared/Figures",
       width = 85, height = 70, units = "mm", dpi = 300)

#stats on prop species per year with management action
new <- subset(df.y2,Date %in% c("2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018"))
aggregate(new$Proportion, by=list(Category= new$Discussion=="Action"), FUN=summary)

#plot to save horizontal legend
df.y2 <- df.y2[-c(41,40),]
df.y2$Discussion <- as.factor(df.y2$Discussion)
ggplot(df.y2, aes(Date)) +
  geom_bar(aes(weight=Proportion, fill=Discussion), position = position_stack(reverse=TRUE)) +
  labs(x="Date", y="Proportion of species") +
  ggtitle("(a)") +
  scale_fill_manual(values=dis, name="At what level is climate change management discussed?") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title=element_text(hjust=-.17,vjust=-33), legend.position='top',
        legend.text=element_text(size=7), legend.title=element_text(size=8)) +
  guides(fill=guide_legend(nrow=2,title.position="top"))

#SAVE PLOT LEGEND
ggsave("Fig 6f legend.tiff", device= "tiff", path = "C:/Users/jmiller/OneDrive - Defenders of Wildlife/Science support/Climate change/Sensitivity/Figures",
       width = 85, height = 70, units = "mm", dpi = 300)


######
#DISCUSSION BY SPECIES
#####
#Fig 3a
#create dataset (proportional)
df.y2 <- data9.1 %>%
  group_by(Subtaxon, Discuss_Level) %>%
  summarise(nspecies = n()) %>%
  group_by(Subtaxon) %>%
  mutate(freq = nspecies / sum(nspecies)) %>%
  as.data.frame()
colnames(df.y2) <- c("Taxon","Discussion","Nspecies","Proportion")
df.y2$Taxon <- factor(df.y2$Taxon, levels(df.y2$Taxon)[c(7,2,6,5,4,3,8)])

#stacked bar plot (proportional)
ggplot(df.y2, aes(Taxon)) +
  geom_bar(aes(weight=Proportion, fill=Discussion), position = position_stack(reverse=TRUE)) +
  labs(x="Taxon", y="Proportion of species") +
  scale_fill_manual(values=dis) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title=element_text(hjust=-.21), legend.position='none',
        legend.text=element_text(size=8),
        axis.text.y=element_text(colour="black", size=8),
        axis.text.x=element_text(colour="black",angle=55, hjust=1, size=8),
        axis.title=element_text(size=10))+
  guides(fill=guide_legend(nrow=2,byrow=TRUE, title.position="top")) +
  annotate("text", y = -0.03, x="Amphibian", label="18", colour="black",size=3) +
  annotate("text", y = -0.03, x="Bird", label="59", colour="black",size=3) +
  annotate("text", y = -0.03, x="Fish", label="82", colour="black",size=3) +
  annotate("text", y = -0.03, x="Mollusk", label="96", colour="black",size=3) +
  annotate("text", y = -0.03, x="Arthropod", label="92", colour="black",size=3) +
  annotate("text", y = -0.03, x="Mammal", label="59", colour="black",size=3) +
  annotate("text", y = -0.03, x="Reptile", label="14", colour="black",size=3)

#tapply(df.y2$Nspecies, df.y2$Taxon, FUN=sum)

#SAVE PLOT
ggsave("Fig 2g.tiff", device= "tiff", path = "C:/Users/jmiller/OneDrive - Defenders of Wildlife/Science support/Climate change/Sensitivity/Figures",
       width = 85, height = 70, units = "mm", dpi = 300)

#ANALYSIS
#stats on prop species per year with management action
new <- subset(df.y2,Date %in% c("2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018"))
aggregate(new$Proportion, by=list(Category= new$Discussion=="Action"), FUN=summary)

#####
#DISCUSSION BY REGION
#####
data9.1 <- subset(data9, Discuss_Level!="Not expected (newly listed)") #dataset for 'What level of climate change management is discussed?'
data9.1$Discuss_Level <- as.character(data9.1$Discuss_Level)
data9.1$Discuss_Level <- as.factor(data9.1$Discuss_Level)
data9.1$Discuss_Level <- factor(data9.1$Discuss_Level, levels(data9.1$Discuss_Level)[c(3,4,2,1)])

#create dataset (proportional)
df.y2 <- data9.1 %>%
  group_by(Lead.Region, Discuss_Level) %>%
  summarise(nspecies = n()) %>%
  group_by(Lead.Region) %>%
  mutate(freq = nspecies / sum(nspecies)) %>%
  as.data.frame()
colnames(df.y2) <- c("Region","Discussion","Nspecies","Proportion")
df.y2$Region <- factor(df.y2$Region, levels(df.y2$Region)[c(8,5,4,9,6,2,7,3,10)])

#stacked bar plot (proportional)
#Fig 3d
ggplot(df.y2, aes(Region)) +
  geom_bar(aes(weight=Proportion, fill=Discussion), position = position_stack(reverse=TRUE)) +
  labs(x="Agency and region", y="Proportion of species") +
  #  ggtitle("c") +
  scale_fill_manual(values=dis) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title=element_text(hjust=-.21), legend.position='none',
        legend.text=element_text(size=8),
        axis.text.y=element_text(colour="black", size=8),
        axis.text.x=element_text(colour="black",angle=55, hjust=1, size=8),
        axis.title=element_text(size=10))+
  annotate("text", y = -.03, x="NMFS: Marine", label="23", colour="black",size=3) +
  annotate("text", y =  -.03, x="FWS Region 2: Southwest", label="77", colour="black",size=3) +
  annotate("text", y =  -.03, x="FWS Region 6: Mountain Prairie", label="14", colour="black",size=3) +
  annotate("text", y =  -.03, x="FWS Region 5: Northeast", label="20", colour="black",size=3) +
  annotate("text", y =  -.03, x="FWS Region 1: Pacific", label="60", colour="black",size=3) +
  annotate("text", y =  -.03, x="FWS Region 8: Pacific Southwest", label="78", colour="black",size=3) +
  annotate("text", y =  -.03, x="FWS Region 4: Southeast", label="128", colour="black",size=3) +
  annotate("text", y =  -.03, x="FWS Region 3: Midwest", label="19", colour="black",size=3) +
  annotate("text", y =  -.03, x="FWS Region 7: Alaska", label="1", colour="black",size=3)

#tapply(df.y2$Nspecies, df.y2$Region, FUN=sum)

#SAVE PLOT
ggsave("Fig 2h.tiff", device= "tiff", path = "C:/Users/jmiller/OneDrive - Defenders of Wildlife/Science support/Climate change/Sensitivity/Sensitivity_shared/Figures",
       width = 85, height = 100, units = "mm", dpi = 300)

#By FWS vs NMFS
#create dataset (proportional)
df.y2 <- data9.1 %>%
  group_by(Region2, Discuss_Level) %>%
  summarise(nspecies = n()) %>%
  group_by(Region2) %>%
  mutate(freq = nspecies / sum(nspecies)) %>%
  as.data.frame()
colnames(df.y2) <- c("Region","Discussion","Nspecies","Proportion")
#df.y2$Region <- factor(df.y2$Region, levels(df.y2$Region)[c(8,5,4,9,6,2,7,3,10)])

#stacked bar plot (proportional)
ggplot(df.y2, aes(Region)) +
  geom_bar(aes(weight=Proportion, fill=Discussion), position = position_stack(reverse=TRUE)) +
  labs(x="Agency", y="Proportion of species") +
  #  ggtitle("c") +
  scale_fill_manual(values=dis) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title=element_text(hjust=-.21), legend.position='none',
        legend.text=element_text(size=8),
        axis.text.y=element_text(colour="black", size=8),
        axis.text.x=element_text(colour="black",angle=55, hjust=1, size=8),
        axis.title=element_text(size=10))+
  annotate("text", y = -.03, x="FWS", label="397", colour="black",size=3) +
  annotate("text", y =  -.03, x="NMFS", label="23", colour="black",size=3)

#tapply(df.y2$Nspecies, df.y2$Region, FUN=sum)
#df.y2

#SAVE PLOT
ggsave("Fig S2b.tiff", device= "tiff", path = "C:/Users/jmiller/OneDrive - Defenders of Wildlife/Science support/Climate change/Sensitivity/Sensitivity_shared/Figures",
       width = 85, height = 100, units = "mm", dpi = 300)

#####
#FIG S1 factor sensitivity by region
#####

#sample size in each region
data8 %>%
  group_by(Lead.Region) %>%
  summarize(nspec=n_distinct(Scientific.Name))

#create data
df.y3 <- data8 %>%
  group_by(Lead.Region, Theme, Sensitivity) %>%
  summarise(nspecies = n()) %>%
  group_by(Lead.Region, Theme) %>%
  mutate(freq = nspecies / sum(nspecies)) %>%
  as.data.frame()
colnames(df.y3) <- c("Region","Theme","Sensitivity","Nspecies","Proportion")

#matrix
#creating data subset to only 'yes'
df.y4 <- df.y3[which(df.y3$Sensitivity=='y'),]
df.y4 <- df.y4[,c(-3,-4)]
#manually adding mollusk 'yes' row
dg <- c("FWS Region 1: Pacific","FWS Region 5: Northeast","FWS Region 7: Alaska","FWS Region 7: Alaska",
        "FWS Region 7: Alaska","FWS Region 7: Alaska","FWS Region 7: Alaska")
dh <- c("Phenology","Phenology","Phenology","Temperature","Injurious species","Hydrology","Chemistry")
di <- c(0,0,0,0,0,0,0)
all <- data.frame(dg,dh,di)
names(all) <- c("Region","Theme","Proportion")
df.y4 <- rbind(df.y4,all)
df.y4$Proportion <- as.numeric(df.y4$Proportion)

#renaming to show n in panel headers
levels(df.y4$Region)[levels(df.y4$Region)=="NMFS: Marine"] <- "NMFS: Marine (n=24)"
levels(df.y4$Region)[levels(df.y4$Region)=="FWS Region 1: Pacific"] <- "FWS Region 1: Pacific (n=88)"
levels(df.y4$Region)[levels(df.y4$Region)=="FWS Region 2: Southwest"] <- "FWS Region 2: Southwest (n=78)"
levels(df.y4$Region)[levels(df.y4$Region)=="FWS Region 3: Midwest"] <- "FWS Region 3: Midwest (n=24)"
levels(df.y4$Region)[levels(df.y4$Region)=="FWS Region 4: Southeast"] <- "FWS Region 4: Southeast (n=131)"
levels(df.y4$Region)[levels(df.y4$Region)=="FWS Region 5: Northeast"] <- "FWS Region 5: Northeast (n=20)"
levels(df.y4$Region)[levels(df.y4$Region)=="FWS Region 6: Mountain Prairie"] <- "FWS Region 6: Mountain Prairie (n=14)"
levels(df.y4$Region)[levels(df.y4$Region)=="FWS Region 7: Alaska"] <- "FWS Region 7: Alaska (n=1)"
levels(df.y4$Region)[levels(df.y4$Region)=="FWS Region 8: Pacific Southwest"] <- "FWS Region 8: Pacific Southwest (n=78)"

#graphing matrix plot
ggplot(df.y4, aes(x=Region, y=Theme,fill=Proportion)) +
  geom_tile() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title=element_text(hjust=-.18),
        legend.position='top', legend.justification='left', legend.direction='horizontal',
        axis.text.x=element_text(angle=30,hjust=1,colour="black",size=7),
        axis.text.y=element_text(colour="black", size=8),
        strip.background=element_blank(),
        strip.text.x = element_text(size=8),
        legend.text=element_text(size=8), legend.title=element_text(size=8)) +
  labs(x="Agency and region", y="Sensitivity factor") +
  scale_fill_gradient(limits=c(0,1),breaks=c(0,0.5,1),low = "#e8fa5bff", high = "sienna3") +
  scale_y_discrete(limits=rev(levels(df.y4$Theme)),expand = c(0,0)) + scale_x_discrete(expand = c(0,0)) +
  guides(fill=guide_colourbar(title="Proportion of species sensitive",
                              title.position="top", ticks=FALSE))

#SAVE PLOT
ggsave("Fig S1.tiff", device = "tiff", path = "C:/Users/jmiller/OneDrive - Defenders of Wildlife/Science support/Climate change/Sensitivity/Sensitivity_shared/Figures",
       width = 100, height = 120, units = "mm", dpi = 300)

#stacked bar plot
ggplot(df.y3, aes(Theme)) +
  geom_bar(aes(weight=Proportion, fill=Sensitivity), position = position_stack(reverse=TRUE)) +
  facet_wrap(~Region, ncol=2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title=element_text(hjust=-.18),
        legend.position='top', legend.justification='center',
        axis.text.x=element_text(angle=30,hjust=1,colour="black",size=7),
        axis.text.y=element_text(colour="black", size=8),
        strip.background=element_blank(),
        strip.text.x = element_text(size=8)) +
  labs(x="Sensitivity factor", y="Proportion of species") +
  scale_fill_manual(values=sen, labels=c("Unknown","No","Yes")) +
  guides(fill=guide_legend(title="Is the species sensitive?",title.position = "top")) +
  scale_y_continuous(limits=c(0,1))

#SAVE PLOT
ggsave("Fig 1S.tiff", device= "tiff", path = "C:/Users/jmiller/OneDrive - Defenders of Wildlife/Science support/Climate change/Sensitivity/Figures",
       width = 120, height = 180, units = "mm", dpi = 300)


ggplot(df.y3, aes(Taxon)) +
  geom_bar(aes(weight=Proportion, fill=Sensitivity), position = position_stack(reverse=TRUE)) +
  facet_wrap(~Theme, ncol=2) +
  # ggtitle("c") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title=element_text(hjust=-.18),
        legend.position='top', legend.justification='left',
        axis.text.x=element_text(angle=30,hjust=1,colour="black",size=7),
        axis.text.y=element_text(colour="black", size=8),
        strip.background=element_blank(),
        strip.text.x = element_text(size=8),
        legend.text=element_text(size=8), legend.title=element_text(size=8)) +
  labs(x="Taxon", y="Proportion of species") +
  # coord_flip() +
  scale_fill_manual(values=pal, labels=c("Unknown","No","Yes")) +
  guides(fill=guide_legend(title="Is the species sensitive to climate change?", title.position="top")) +
  scale_y_continuous(limits=c(0,1))

#SAVE PLOT
ggsave("Fig 1C.tiff", device = "tiff", path = "C:/Users/jmiller/OneDrive - Defenders of Wildlife/Science support/Climate change/Sensitivity/Figures",
       width = 88, height = 160, units = "mm", dpi = 300)





#####
#Fig 2
#Old analysis with discussion analysis combined
#outdated; do not use
#####

#create dataset (number of species)
df.y2 <- data9 %>%
  group_by(Sensitivity) %>%
  summarise(nspecies = n()) %>%
  as.data.frame()
colnames(df.y2) <- c("Discussion","Nspecies")

ggplot(data9,aes(x=Sensitivity)) +
  geom_bar(width=0.5,fill="#a50f15")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title=element_text(hjust=-.18), legend.position='none',
        axis.text.x=element_text(angle=45,hjust=1,colour="black", size=8),
        axis.text.y=element_text(colour="black", size=10),
        axis.title=element_text(size=10)) +
  labs(x="Discussion", y="Number of species", title="")

#SAVE PLOT
ggsave("Fig 2A.tiff", device = "tiff", path = "C:/Users/jmiller/OneDrive - Defenders of Wildlife/Science support/Climate change/Sensitivity/Figures",
       width = 85, height = 120, units = "mm", dpi = 300)


#####
#FIG 2B Species discussion by date
#####

#PREPARE DATA
#bring in date
date.orig = read.csv("species by date 2019-03-28.csv", header = T)
date <- date.orig[,c(1,13)]
data.full <- merge(data9,date, by=("Scientific.Name"))
data.full <- data.full[!(data.full$Scientific.Name=="Branta sandvicensis"),]

#lump pre-2007 into one category
data.full$Date.of.most.recent.doc[data.full$Date.of.most.recent.doc<=2005] <- "Pre-2006"

#analysis
summary(data.full$Sensitivity)
76/459 #% species with management action
(153)/459 #discussed without action
28/459 #further study
155/459 #no discussion

#Analysis of factors by discussion level
data.full1 <- merge(data.full,df.y2, by=("Scientific.Name"))
df1 <- data.full1[,c("Scientific.Name","Factors","Sensitivity")]

#create dataset for plot
df.y2 <- data.full %>%
  group_by(Date.of.most.recent.doc, Sensitivity) %>%
  summarise(nspecies = n()) %>%
  group_by(Date.of.most.recent.doc) %>%
  mutate(freq = nspecies / sum(nspecies)) %>%
  as.data.frame()
colnames(df.y2) <- c("Date","Discussion","Nspecies","Proportion")
df.y2$Date <- as.factor(df.y2$Date)
df.y2$Date <- factor(df.y2$Date, levels(df.y2$Date)[c(14,1:13)])

#stacked bar plot (proportional)
#pal <- c("#f2f0f7","#dadaeb","#bcbddc","#9e9ac8","#756bb1","#54278f")
pal <- c("#feedde","#fdd0a2","#fdae6b","#fd8d3c","#e6550d","#a63603")
pal <- c("#fee5d9","#fcbba1","#fc9272","#fb6a4a","#de2d26","#a50f15")
ggplot(df.y2, aes(Date)) +
  geom_bar(aes(weight=Proportion, fill=Discussion), position = position_stack(reverse=TRUE)) +
  labs(x="Date", y="Proportion of species") +
  ggtitle("a") +
  scale_fill_manual(values=pal) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title=element_text(hjust=-.22), legend.position='none',
        legend.text=element_text(size=8),
        axis.text.y=element_text(colour="black", size=10),
        axis.text.x=element_text(colour="black",angle=55, hjust=1, size=8)) +
  guides(fill=guide_legend(nrow=2,byrow=TRUE, title.position="top")) +
  annotate("text", y = -.03, x="Pre-2006", label="35", colour="black", size=3) +
  annotate("text", y = -.03, x="2006", label="12", colour="black", size=3) +
  annotate("text", y = -.03, x="2007", label="21", colour="black", size=3) +
  annotate("text", y = -.03, x="2008", label="31", colour="black", size=3) +
  annotate("text", y = -.03, x="2009", label="43", colour="black", size=3) +
  annotate("text", y = -.03, x="2010", label="36", colour="black", size=3) +
  annotate("text", y = -.03, x="2011", label="44", colour="black", size=3) +
  annotate("text", y = -.03, x="2012", label="59", colour="black", size=3) +
  annotate("text", y = -.03, x="2013", label="56", colour="black", size=3) +
  annotate("text", y = -.03, x="2014", label="36", colour="black", size=3) +
  annotate("text", y = -.03, x="2015", label="30", colour="black", size=3) +
  annotate("text", y = -.03, x="2016", label="32", colour="black", size=3) +
  annotate("text", y = -.03, x="2017", label="7", colour="black", size=3) +
  annotate("text", y = -.03, x="2018", label="15", colour="black", size=3)

#SAVE PLOT
ggsave("Fig 3A.tiff", device = "tiff", path = "C:/Users/jmiller/OneDrive - Defenders of Wildlife/Science support/Climate change/Sensitivity/Figures",
       width = 85, height = 100, units = "mm", dpi = 300)


#plot to save vertical legend
ggplot(df.y2, aes(Date)) +
  geom_bar(aes(weight=Proportion, fill=Discussion), position = position_stack(reverse=TRUE)) +
  labs(x="Date", y="Proportion of species") +
  ggtitle("(a)") +
  scale_fill_manual(values=pal) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title=element_text(hjust=-.17,vjust=-33), legend.position='left',
        legend.text=element_text(size=8),
        axis.text.y=element_text(colour="black", size=10),
        axis.text.x=element_text(colour="black",angle=55, hjust=1, size=10)) +
  guides(fill=guide_legend(reverse=TRUE))

#SAVE PLOT LEGEND
ggsave("Fig 3 legend.tiff", device= "tiff", path = "C:/Users/jmiller/OneDrive - Defenders of Wildlife/Science support/Climate change/Sensitivity/Figures",
       width = 88, height = 88, units = "mm", dpi = 300)


#Plot species by date discussion (stacked bar)
pal <- c("#f2f0f7","#cbc9e2","#9e9ac8","#756bb1","#54278f")
ggplot(df.y2, aes(Date)) +
  geom_bar(aes(weight=Nspecies, fill=Discussion), position = position_stack(reverse=TRUE)) +
  labs(x="Date", y="Number of species") +
  ggtitle("(a)") +
  scale_fill_manual(values=pal) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title=element_text(hjust=-0.12), legend.position='top',
        legend.text=element_text(size=8),
        axis.text.y=element_text(colour="black", size=10),
        axis.text.x=element_text(colour="black",angle=55, hjust=1, size=10)) +
  guides(fill=guide_legend(nrow=2,byrow=TRUE, title.position="top")) +
  scale_y_continuous(breaks=seq(0,70,by=10), limits = c(0,70) )

#stats
aggregate(df.y2$Date, by=list(Category=df.y2$Nspecies), FUN=sum)

new <- subset(df.y2,Date %in% c("2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018"))
aggregate(new$Proportion, by=list(Category=new$Discussion), FUN=summary)


#####
#FIG 2C species vs action level (discussion)
#####

#Updated 4-15-19 to split discussion analysis into two

#create dataset (proportional)
df.y2 <- data9 %>%
  group_by(Subtaxon, Discuss_Level) %>%
  summarise(nspecies = n()) %>%
  group_by(Subtaxon) %>%
  mutate(freq = nspecies / sum(nspecies)) %>%
  as.data.frame()
colnames(df.y2) <- c("Taxon","Discussion","Nspecies","Proportion")
df.y2$Taxon <- factor(df.y2$Taxon, levels(df.y2$Taxon)[c(7,2,4,6,5,3,8)])

#stacked bar plot (proportional)
#pal <- c("#f2f0f7","#cbc9e2","#9e9ac8","#756bb1","#54278f")
#pal <- c("#feedde","#fdd0a2","#fdae6b","#fd8d3c","#e6550d","#a63603")
pal <- c("#fee5d9","#fcbba1","#fc9272","#fb6a4a","#de2d26","#a50f15")
ggplot(df.y2, aes(Taxon)) +
  geom_bar(aes(weight=Proportion, fill=Discussion), position = position_stack(reverse=TRUE)) +
  labs(x="Taxon", y="Proportion of species") +
  ggtitle("b") +
  scale_fill_manual(values=pal) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title=element_text(hjust=-.21), legend.position='none',
        legend.text=element_text(size=8),
        axis.text.y=element_text(colour="black", size=10),
        axis.text.x=element_text(colour="black",angle=55, hjust=1, size=10)) +
  guides(fill=guide_legend(nrow=2,byrow=TRUE, title.position="top")) +
  annotate("text", y = -0.03, x="Amphibian", label="19", colour="black",size=3) +
  annotate("text", y = -0.03, x="Bird", label="62", colour="black",size=3) +
  annotate("text", y = -0.03, x="Fish", label="83", colour="black",size=3) +
  annotate("text", y = -0.03, x="Mollusck", label="107", colour="black",size=3) +
  annotate("text", y = -0.03, x="Arthropod", label="110", colour="black",size=3) +
  annotate("text", y = -0.03, x="Mammal", label="62", colour="black",size=3) +
  annotate("text", y = -0.03, x="Reptile", label="16", colour="black",size=3)

#SAVE PLOT
ggsave("Fig 3B.tiff", device= "tiff", path = "C:/Users/jmiller/OneDrive - Defenders of Wildlife/Science support/Climate change/Sensitivity/Figures",
       width = 85, height = 100, units = "mm", dpi = 300)




#Original analysis with discussion analysis as one

#create dataset (proportional)
df.y2 <- data9 %>%
  group_by(Subtaxon, Sensitivity) %>%
  summarise(nspecies = n()) %>%
  group_by(Subtaxon) %>%
  mutate(freq = nspecies / sum(nspecies)) %>%
  as.data.frame()
colnames(df.y2) <- c("Taxon","Discussion","Nspecies","Proportion")
df.y2$Taxon <- factor(df.y2$Taxon, levels(df.y2$Taxon)[c(7,2,4,6,5,3,8)])

#stacked bar plot (proportional)
#pal <- c("#f2f0f7","#cbc9e2","#9e9ac8","#756bb1","#54278f")
#pal <- c("#feedde","#fdd0a2","#fdae6b","#fd8d3c","#e6550d","#a63603")
pal <- c("#fee5d9","#fcbba1","#fc9272","#fb6a4a","#de2d26","#a50f15")
ggplot(df.y2, aes(Taxon)) +
  geom_bar(aes(weight=Proportion, fill=Discussion), position = position_stack(reverse=TRUE)) +
  labs(x="Taxon", y="Proportion of species") +
  ggtitle("b") +
  scale_fill_manual(values=pal) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title=element_text(hjust=-.21), legend.position='none',
        legend.text=element_text(size=8),
        axis.text.y=element_text(colour="black", size=10),
        axis.text.x=element_text(colour="black",angle=55, hjust=1, size=10)) +
  guides(fill=guide_legend(nrow=2,byrow=TRUE, title.position="top")) +
  annotate("text", y = -0.03, x="Amphibian", label="19", colour="black",size=3) +
  annotate("text", y = -0.03, x="Bird", label="62", colour="black",size=3) +
  annotate("text", y = -0.03, x="Fish", label="83", colour="black",size=3) +
  annotate("text", y = -0.03, x="Mollusck", label="107", colour="black",size=3) +
  annotate("text", y = -0.03, x="Arthropod", label="110", colour="black",size=3) +
  annotate("text", y = -0.03, x="Mammal", label="62", colour="black",size=3) +
  annotate("text", y = -0.03, x="Reptile", label="16", colour="black",size=3)

#SAVE PLOT
ggsave("Fig 3B.tiff", device= "tiff", path = "C:/Users/jmiller/OneDrive - Defenders of Wildlife/Science support/Climate change/Sensitivity/Figures",
       width = 85, height = 100, units = "mm", dpi = 300)


#create dataset (number of species)
df.y2 <- data9 %>%
  group_by(Subtaxon, Sensitivity) %>%
  summarise(nspecies = n()) %>%
  group_by(Subtaxon) %>%
  as.data.frame()
colnames(df.y2) <- c("Taxon","Discussion","Nspecies")
#df.y2$Taxon <- factor(df.y2$Taxon, levels(df.y2$Taxon)[7:1])
df.y2$Taxon <- factor(df.y2$Taxon, levels=c("Arthropod","Mollusck","Fish",
                                            "Mammal","Bird","Amphibian","Reptile"))
#stacked bar plot (number of species)
pal <- c("#f2f0f7","#cbc9e2","#9e9ac8","#756bb1","#54278f")
pal <- c("#f2f0f7","#dadaeb","#bcbddc","#9e9ac8","#756bb1","#54278f")
pal <- c("#b35806","#f1a340","#fee0b6","#d8daeb","#998ec3","#542788")
ggplot(df.y2, aes(Taxon)) +
  geom_bar(aes(weight=Nspecies, fill=Discussion), position = position_stack(reverse=TRUE)) +
  labs(x="Taxon", y="Number of species") +
  ggtitle("(b)") +
  #coord_flip() +
  scale_fill_manual(values=pal) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title=element_text(hjust=-.08), legend.position='top',
        axis.text=element_text(colour="black", size=10)) +
  scale_y_continuous(limits = c(0,120))



######
#FIG 2D
######

#renaming to show n in panel headers
data9$Lead.Region <- as.character(data9$Lead.Region)
data9$Lead.Region <- factor(data9$Lead.Region)

#create dataset (proportional)
df.y2 <- data9 %>%
  group_by(Lead.Region, Sensitivity) %>%
  summarise(nspecies = n()) %>%
  group_by(Lead.Region) %>%
  mutate(freq = nspecies / sum(nspecies)) %>%
  as.data.frame()
colnames(df.y2) <- c("Region","Discussion","Nspecies","Proportion")
df.y2$Region <- factor(df.y2$Region, levels(df.y2$Region)[c(7,3,4,8,1,5,6,2,9)])

#stacked bar plot (proportional)
#pal <- c("#f2f0f7","#cbc9e2","#9e9ac8","#756bb1","#54278f")
#pal <- c("#b35806","#f1a340","#fee0b6","#d8daeb","#998ec3","#542788")
pal <- c("#feedde","#fdd0a2","#fdae6b","#fd8d3c","#e6550d","#a63603")
pal <- c("#fee5d9","#fcbba1","#fc9272","#fb6a4a","#de2d26","#a50f15")
ggplot(df.y2, aes(Region)) +
  geom_bar(aes(weight=Proportion, fill=Discussion), position = position_stack(reverse=TRUE)) +
  labs(x="Agency and region", y="Proportion of species") +
  ggtitle("c") +
  scale_fill_manual(values=pal) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title=element_text(hjust=-.21), legend.position='none',
        legend.text=element_text(size=8),
        axis.text.y=element_text(colour="black", size=10),
        axis.text.x=element_text(colour="black",angle=55, hjust=1, size=9))+
  guides(fill=guide_legend(nrow=2,byrow=TRUE, title.position="top")) +
  annotate("text", y = -.03, x="NMFS: Marine", label="24", colour="black",size=3) +
  annotate("text", y =  -.03, x="FWS Region 2: Southwest", label="78", colour="black",size=3) +
  annotate("text", y =  -.03, x="FWS Region 6: Mountain Prairie", label="14", colour="black",size=3) +
  annotate("text", y =  -.03, x="FWS Region 5: Northeast", label="20", colour="black",size=3) +
  annotate("text", y =  -.03, x="FWS Region 1: Pacific", label="88", colour="black",size=3) +
  annotate("text", y =  -.03, x="FWS Region 8: Pacific Southwest", label="78", colour="black",size=3) +
  annotate("text", y =  -.03, x="FWS Region 4: Southeast", label="131", colour="black",size=3) +
  annotate("text", y =  -.03, x="FWS Region 3: Midwest", label="24", colour="black",size=3) +
  annotate("text", y =  -.03, x="FWS Region 7: Alaska", label="1", colour="black",size=3)

#SAVE PLOT
ggsave("Fig 3C.tiff", device= "tiff", path = "C:/Users/jmiller/OneDrive - Defenders of Wildlife/Science support/Climate change/Sensitivity/Figures",
       width = 85, height = 140, units = "mm", dpi = 300)


#create dataset (num species)
df.y2 <- data9 %>%
  group_by(Lead.Region, Sensitivity) %>%
  summarise(nspecies = n()) %>%
  group_by(Lead.Region) %>%
  as.data.frame()
colnames(df.y2) <- c("Region","Discussion","Nspecies")
df.y2$Region <- factor(df.y2$Region, levels(df.y2$Region)[c(4,1,2,8,9,3,5,6,7)])

#stacked bar plot (num species)
#pal <- c("#ffffd4","#fed98e","#fe9929","#d95f0e","#993404")
#pal <- c("#f2f0f7","#cbc9e2","#9e9ac8","#756bb1","#54278f")
pal <- c("#feedde","#fdd0a2","#fdae6b","#fd8d3c","#e6550d","#a63603")
ggplot(df.y2, aes(Region)) +
  geom_bar(aes(weight=Nspecies, fill=Discussion), position = position_stack(reverse=TRUE)) +
  labs(x="Agency and region", y="Number of species") +
  ggtitle("(c)") +
  scale_fill_manual(values=pal) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title=element_text(hjust=-.075), legend.position='NONE',
        axis.text=element_text(colour="black"), axis.text.x=element_text(angle=55,hjust=1)) +
  scale_y_continuous(limits=c(0,150)) +
  annotate("text", y = -.03, x="NMFS: Marine", label="24", colour="black",size=3) +
  annotate("text", y =  -.03, x="FWS Region 2: Southwest", label="78", colour="black",size=3) +
  annotate("text", y =  -.03, x="FWS Region 6: Mountain Prairie", label="14", colour="black",size=3) +
  annotate("text", y =  -.03, x="FWS Region 5: Northeast", label="20", colour="black",size=3) +
  annotate("text", y =  -.03, x="FWS Region 1: Pacific", label="88", colour="black",size=3) +
  annotate("text", y =  -.03, x="FWS Region 8: Pacific Southwest", label="78", colour="black",size=3) +
  annotate("text", y =  -.03, x="FWS Region 4: Southeast", label="131", colour="black",size=3) +
  annotate("text", y =  -.03, x="FWS Region 3: Midwest", label="24", colour="black",size=3) +
  annotate("text", y =  -.03, x="FWS Region 7: Alaska", label="1", colour="black",size=3)


