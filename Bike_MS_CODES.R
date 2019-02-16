
#Team:KDD_DEEP_LEARNERS

#-	Bandhavi Bodduluri - 801042711
#-	Karthik Ravi - 801053977
#-	Chandra Bhumireddy - 801053363
#-	Shashikant Jaiswal - 801053461

#################### Importing and mergind Data Files##############
library(xlsx)
##set working directory
setwd("G:/My Drive/Projects/KDD Deep Learners/Files/Tera_data_final_codes")

##reading 2 datasets
ParticipantData <- read.csv('2013-2017 Bike MS Participants.csv', header=TRUE, sep=',', , na.strings=c("","NA"))
TeamData <- read.csv('2013-2017BikeTeams.csv', header=TRUE, sep=',', , na.strings=c("","NA"))

##viewing the 2 read datasets
View(TeamData)
View(ParticipantData)

##Sumary of two datasets
View(Summary(TeamData))
View(Summary(ParticipantData))

##Number of Rows in two datasets
nrow(TeamData)
nrow(ParticipantData)

##joining the 2 datasets based on Team.ID
team_participants <- merge(TeamData, ParticipantData, by='Team.ID')

##number of rows in the new dataset
nrow(team_participants)
##343858

##saving this new dataset as a team_participants.csv
write.csv(team_participants, file = "team_participants.csv")

##summary of team_participants.csv
summary(team_participants)

##viewing the team_participants.csv
View(team_participants)




#################### Exploratory DataAnalysis ###########################

#=================== EDA - 1 - Event Types ============

#Generating counts for categorical variable Event Type
counts_event_type <- table(team_participants$Event.Type)

##viewing the counts_event_type table 
counts_event_type

##plotting the bar graphs for counts_event_type
x <- barplot(counts_event_type, xlim = c(0,4), main="Event Types Distribution", xlab="Event Type")
y <- data.frame(counts_event_type)
text(x,50000,labels=as.character(y$Freq))
box(which = "plot",
    lty = "solid",
    col="black")

##Based on the analysis, we have selected only events type = Bike
team_participants_bike <-subset(team_participants, Event.Type == "Bike")

##The number of rows in the original dataset
nrow(team_participants)
##343858

##The number of rows in the derived dataset Event = Bike
nrow(team_participants_bike)
##343805

##we have removed 53 data rows


#=================== EDA - 2 - Participant Occupation ============= 

## Plotting the occupation counts
Participant_occupation <- table(team_participants_bike$Participant.Occupation)
Participant_occupation <- sort(Participant_occupation, decreasing = TRUE, na.last = NA)
View(Participant_occupation)
x <- barplot(Participant_occupation,  main="Participant occupation Distribution", 
             xlab="Participant occupation", 
             las=2, space = 1)
box(which = "plot",
    lty = "solid",
    col="black")

#y <- data.frame(Participant_occupation)
#text(x,500,labels=as.character(y$Freq))

##From the above graph, we remove the outliers that have occupation count below 10
cVals <- data.frame(table(team_participants_bike$Participant.Occupation))
Rows <- team_participants_bike$Participant.Occupation %in% cVals[cVals$Freq > 10,1]
team_participants_occupation <- team_participants_bike[Rows,]

##the new dataset : removed outliners of occupations 
View(summary(team_participants_occupation$Participant.Occupation))

#================= EDA 3 - Funds Raised by Occupation ========

##suming the "Total.of.All.Confirmed.Gifts" based on the occupation
totalgifts_allyear = aggregate(team_participants_occupation$Total.of.All.Confirmed.Gifts..., by=list(Category=team_participants_occupation$Participant.Occupation), FUN=sum)

View(totalgifts_allyear)

View(table(totalgifts_allyear))

totalgifts_allyear_for_graph <- totalgifts_allyear[with(totalgifts_allyear, order(totalgifts_allyear$x)),]

Participant_occupation <- table(team_participants_bike$Participant.Occupation)
Participant_occupation <- sort(Participant_occupation, decreasing = TRUE, na.last = NA)
View(Participant_occupation)
x <- barplot(Participant_occupation,  main="Participant occupation Distribution", 
             xlab="Participant occupation", 
             las=2, space = 1)
box(which = "plot",
    lty = "solid",
    col="black")

##the total amounts contributed by each occupation field is 
View(totalgifts_allyear)
write.csv(totalgifts_allyear, file = "total_contribution_occupation.csv")

##Classifying dataset based on each - attribute used = Fiscal.Year.x
genuineparticipants_bike_2013 <- subset(team_participants_occupation, Fiscal.Year.x == "2013")
genuineparticipants_bike_2014 <- subset(team_participants_occupation, Fiscal.Year.x == "2014")
genuineparticipants_bike_2015 <- subset(team_participants_occupation, Fiscal.Year.x == "2015")
genuineparticipants_bike_2016 <- subset(team_participants_occupation, Fiscal.Year.x == "2016")
genuineparticipants_bike_2017 <- subset(team_participants_occupation, Fiscal.Year.x == "2017")


##2013 - funds by occupation
##suming the "Total.of.All.Confirmed.Gifts" based on the occupation 
totalgifts_2013 = aggregate(genuineparticipants_bike_2013$Total.of.All.Confirmed.Gifts..., by=list(Category=genuineparticipants_bike_2013$Participant.Occupation), FUN=sum)
totalgifts_2013$year <- 2013
View(totalgifts_2013)
write.csv(totalgifts_2013, file = "total_contribution_2013.csv")

##2014 - funds by occupation
##suming the "Total.of.All.Confirmed.Gifts" based on the occupation 
totalgifts_2014 = aggregate(genuineparticipants_bike_2014$Total.of.All.Confirmed.Gifts..., by=list(Category=genuineparticipants_bike_2014$Participant.Occupation), FUN=sum)
totalgifts_2014$year <- 2014
View(totalgifts_2014)
write.csv(totalgifts_2014, file = "total_contribution_2014.csv")

##2015 - funds by occupation
##suming the "Total.of.All.Confirmed.Gifts" based on the occupation 
totalgifts_2015 = aggregate(genuineparticipants_bike_2015$Total.of.All.Confirmed.Gifts..., by=list(Category=genuineparticipants_bike_2015$Participant.Occupation), FUN=sum)
totalgifts_2015$year <- 2015
View(totalgifts_2015)
write.csv(totalgifts_2015, file = "total_contribution_2015.csv")

##2016 - funds by occupation
##suming the "Total.of.All.Confirmed.Gifts" based on the occupation 
totalgifts_2016 = aggregate(genuineparticipants_bike_2016$Total.of.All.Confirmed.Gifts..., by=list(Category=genuineparticipants_bike_2016$Participant.Occupation), FUN=sum)
totalgifts_2016$year <- 2016
View(totalgifts_2016)
write.csv(totalgifts_2016, file = "total_contribution_2016.csv")

##2017 - funds by occupation
##suming the "Total.of.All.Confirmed.Gifts" based on the occupation 
totalgifts_2017 = aggregate(genuineparticipants_bike_2017$Total.of.All.Confirmed.Gifts..., by=list(Category=genuineparticipants_bike_2017$Participant.Occupation), FUN=sum)
totalgifts_2017$year <- 2017
View(totalgifts_2017)
write.csv(totalgifts_2017, file = "total_contribution_2017.csv")

All_year_individual_occupation <- rbind(totalgifts_2013,totalgifts_2014,totalgifts_2015,totalgifts_2016,totalgifts_2017)

##sorting the total gifts to get the top 5 companies that has highest revenue
topoccupations <- head(sort(totalgifts_allyear$x,decreasing=TRUE), n = 5)

##collecting all the top performing occupations - top 5
library(plyr)
x = arrange(totalgifts_allyear,desc(x))
top_occupation_all = x[1:5,]


##
#x <- top_occupation_all$Category
#y <- All_year_individual_occupation$Category
#for (x in y)
#{
#  print(x)
#}

##subsetting the top 5 occupations based on year
subsetbyyear_Engineering <- subset(All_year_individual_occupation, Category == "Engineering")
subsetbyyear_ExecutiveManagement <- subset(All_year_individual_occupation, Category == "Executive/Management")
subsetbyyear_Healthcare <- subset(All_year_individual_occupation, Category == "Healthcare")
subsetbyyear_InformationTechnology <- subset(All_year_individual_occupation, Category == "Information Technology (IT)")
subsetbyyear_Sales <- subset(All_year_individual_occupation, Category == "Sales")

##viewign the subsets
View(subsetbyyear_Engineering)
View(subsetbyyear_ExecutiveManagement)
View(subsetbyyear_Healthcare)
View(subsetbyyear_InformationTechnology)
View(subsetbyyear_Sales)


#########Modeling########

##install this library before running
library(ggplot2)


##linear Regression 1:

##Engineering
plot(subsetbyyear_Engineering$year, subsetbyyear_Engineering$x)
lr_eng <- lm(subsetbyyear_Engineering$x ~ subsetbyyear_Engineering$year, data = subsetbyyear_Engineering) 
##The equation is 
lr_eng$coefficients

##plots
ggplot(data = subsetbyyear_Engineering, aes(x = subsetbyyear_Engineering$year, y = subsetbyyear_Engineering$x)) + 
  geom_point(color = 'blue') + 
  geom_line(color = 'red', data = lr_eng, aes(x = subsetbyyear_Engineering$year, y = subsetbyyear_Engineering$x))

##SMOOTH LINEAR REGRESSION LINE

ggplot(data = subsetbyyear_Engineering, aes(x = subsetbyyear_Engineering$year, y = subsetbyyear_Engineering$x)) + 
  geom_point(color = 'RED') + 
  geom_smooth(method = "lm" , se = FALSE)


##linear Regression 2:

##ExecutiveManagement
plot(subsetbyyear_ExecutiveManagement$year, subsetbyyear_ExecutiveManagement$x)
lr_em <- lm(subsetbyyear_ExecutiveManagement$x ~ subsetbyyear_ExecutiveManagement$year, data = subsetbyyear_ExecutiveManagement) 
##The equation is 
lr_em$coefficients

##plots
ggplot(data = subsetbyyear_ExecutiveManagement, aes(x = subsetbyyear_ExecutiveManagement$year, y = subsetbyyear_ExecutiveManagement$x)) + 
  geom_point(color = 'blue') + 
  geom_line(color = 'red', data = lr_em, aes(x = subsetbyyear_ExecutiveManagement$year, y = subsetbyyear_ExecutiveManagement$x))

##SMOOTH LINEAR REGRESSION LINE

ggplot(data = subsetbyyear_ExecutiveManagement, aes(x = subsetbyyear_ExecutiveManagement$year, y = subsetbyyear_ExecutiveManagement$x)) + 
  geom_point(color = 'RED') + 
  geom_smooth(method = "lm" , se = FALSE)



##linear Regression 3:

##Engineering
plot(subsetbyyear_Healthcare$year, subsetbyyear_Healthcare$x)
lr_hc<- lm(subsetbyyear_Healthcare$x ~ subsetbyyear_Healthcare$year, data = subsetbyyear_Healthcare) 
##The equation is 
lr_hc$coefficients

##plots
ggplot(data = subsetbyyear_Healthcare, aes(x = subsetbyyear_Healthcare$year, y = subsetbyyear_Healthcare$x)) + 
  geom_point(color = 'blue') + 
  geom_line(color = 'red', data = lr_hc, aes(x = subsetbyyear_Healthcare$year, y = subsetbyyear_Healthcare$x))

##SMOOTH LINEAR REGRESSION LINE

ggplot(data = subsetbyyear_Healthcare, aes(x = subsetbyyear_Healthcare$year, y = subsetbyyear_Healthcare$x)) + 
  geom_point(color = 'RED') + 
  geom_smooth(method = "lm" , se = FALSE)




##linear Regression 4:

##subsetbyyear_InformationTechnology
plot(subsetbyyear_InformationTechnology$year, subsetbyyear_InformationTechnology$x)
lr_IT <- lm(subsetbyyear_InformationTechnology$x ~ subsetbyyear_InformationTechnology$year, data = subsetbyyear_InformationTechnology) 
##The equation is 
lr_IT$coefficients

##plots
ggplot(data = subsetbyyear_InformationTechnology, aes(x = subsetbyyear_InformationTechnology$year, y = subsetbyyear_InformationTechnology$x)) + 
  geom_point(color = 'blue') + 
  geom_line(color = 'red', data = lr_IT, aes(x = subsetbyyear_InformationTechnology$year, y = subsetbyyear_InformationTechnology$x))

##SMOOTH LINEAR REGRESSION LINE

ggplot(data = subsetbyyear_InformationTechnology, aes(x = subsetbyyear_InformationTechnology$year, y = subsetbyyear_InformationTechnology$x)) + 
  geom_point(color = 'RED') + 
  geom_smooth(method = "lm" , se = FALSE)

##linear Regression 5:

##SALES
plot(subsetbyyear_Sales$year, subsetbyyear_Sales$x)
lr_sales <- lm(subsetbyyear_Sales$x ~ subsetbyyear_Sales$year, data = subsetbyyear_Sales) 
##The equation is 
lr_sales$coefficients

##plots
ggplot(data = subsetbyyear_Sales, aes(x = subsetbyyear_Sales$year, y = subsetbyyear_Sales$x)) + 
  geom_point(color = 'blue') + 
  geom_line(color = 'red', data = lr_sales, aes(x = subsetbyyear_Sales$year, y = subsetbyyear_Sales$x))

##SMOOTH LINEAR REGRESSION LINE

ggplot(data = subsetbyyear_Sales, aes(x = subsetbyyear_Sales$year, y = subsetbyyear_Sales$x)) + 
  geom_point(color = 'RED') + 
  geom_smooth(method = "lm" , se = FALSE)

