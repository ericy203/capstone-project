#library(sqldf)
library(devtools)
library(readr) 
library(dplyr)
library(tidyr)
library(ggplot2)
library(caTools)
library(ROCR)
library(collegeballR)
setwd("~/Documents/School/Springboard/Project/nflstatistics")
basic_stats <- read.csv("Basic_Stats.csv", header = TRUE, stringsAsFactors = FALSE)


View(basic_stats)
summary(basic_stats)
str(basic_stats)
dim(basic_stats)

#Check for "" in the Experience column
basic_stats %>%
  filter(Experience == "")

basic_stats$Experience[basic_stats$Experience==""] <- NA

is.na(basic_stats$Experience)

#seperate Years.Played into two columns so we can calculate experience and replace the ""
basic_stats <- separate(basic_stats, Years.Played, c("start_year", "end_year"), sep = "-")

#create a difference of end_year and start_year
year_difference <- (as.numeric(basic_stats$end_year) - as.numeric(basic_stats$start_year))
View(year_difference)
str(year_difference)
#calculate experience for "" and replace
basic_stats$Experience[is.na(basic_stats$Experience)]<- year_difference[is.na(basic_stats$Experience)]

#Converting Experience variable into number of seasons
#creating a vector named v
v <- basic_stats$Experience

#Change rookie to 0 
v[grepl("[rR]ookie", basic_stats$Experience)] <- "0"

#Strip all values except for digites from the string
v <- gsub("[^0-9]", "", v)
View(v)


#check on data integrity if done correctly 
#other numbers to check are 1, and 2 
cbind(basic_stats$Experience[as.integer(v) > 15], v[as.integer(v) > 15])
cbind(basic_stats$Experience[as.integer(v) >= 1], v[as.integer(v) >= 1])
cbind(basic_stats$Experience[as.integer(v) >= 2], v[as.integer(v) >= 2])
#Assign integers to basic_stats$Experience from v
basic_stats$Experience <- as.integer(v)
#basic_stats$Experience <- as.integer(unique(v))


grep("[0-9]+", basic_stats$Experience[1:100], value = TRUE)

#Histogram for basic_stats$Experience
hist(basic_stats$Experience[basic_stats$Experience < 5], xlab = "Experience", main = "Player Experience")
ggplot(filter(basic_stats, Experience < 5), aes(x = Experience)) + 
  geom_bar(fill = "#377EB8")

ggplot(filter(basic_stats, Experience < 5), aes(x = Experience)) + 
  geom_histogram(fill = "#377EB8", binwidth = 0.5) +
  labs(title = "Player Experience")


hist(basic_stats$Experience, xlab = "Experience", main = "Player Experience", breaks = c(0:26 -.01))
abline(v = 3, col = "blue") 

hist(basic_stats$Experience[basic_stats$Experience <= 2], xlab = "Experience", main = "Player Experience")
#Players greater than or equal to 20 years
filter(basic_stats, Experience >= 20)
View(filter(basic_stats, Experience >= 20))

#Check to see if there is a duplicate of Fred Evans
basic_stats %>% 
  select(Name, Experience, Birthday) %>%
  filter(Name == "Evans, Fred")

str(basic_stats %>%
  select(Name, start_year) %>%
  filter(start_year >= 1992))
#Look at the average career for a player 
mean(basic_stats$Experience, na.rm = TRUE)

#filter data based on one set having the 0-1 year and the other with 2 - max
#assign all players with 0 or 1 years experience
basic_stats_le1 <- filter(basic_stats, Experience <= 1)
View(basic_stats_le1)
dim(basic_stats_le1)

#assign all players with 2 or more years of experience
basic_stats_me2 <- filter(basic_stats, Experience >=2)
View(basic_stats_me2)
dim(basic_stats_me2)


unique(basic_stats$Experience)


#It seems the CollegeballR package only has players statistics from 2014 and forward.
#This shows there are only 304 players who meet this criteria.
summary(basic_stats %>%
       select(Name, College, start_year) %>%
       filter(start_year >= 2014))

basic_stats %>%
  select(Name, College, start_year) %>%
  filter(start_year > 2006, College == "Texas")

#start working on collegeballR rpackage 


#Assign the team mapping for 2009 to variable
#this just shows the teams and the codes you need to use to get information from player_stats
teams14 <- team_mapping(2014, "MFB")
str(teams14)
teams14

#Team Stats
team_stats(8,2016,"MFB",by="Season")
team_stats(796,2016,"MFB",by="SEASON")
team_stats(697,2016,"MFB",by="SEASON")

#Player Stats 
player_stats(732, 2014, "MFB", by="Season")
player_stats(703, 2014, "MFB", by="Season")
#Assign player stats from Texas year 2014 to texas14
texas15 <- player_stats(703, 2015, "MFB", by="Season")
texas14 <- player_stats(703, 2014, "MFB", by="Season")

#get all data from collegeballR and put in a list of lists called ps_list
ps_list <- list() 
for (y in 2014:2017) {
  current_season_tm <- team_mapping(y, "MFB")
ps_list[[y]] <- list()
for (i in current_season_tm$team_id) {
try(ps_list[[y]][[i]] <- player_stats(as.numeric(i), y, "MFB", by="Season"))
}
}
print(i)

#ps_list[[year]][[team_id]]

collegedf <- data.frame()
for (y in 2014:2017) {
  for (i in names(ps_list[[y]])) {
    df <- ps_list[[y]][[i]]
    df$season <- y
    collegedf <- rbind(collegedf, df)
  }
}



#Add team name
collegedf <- collegedf %>%
  mutate(team_id = as.character(team_id)) %>%
  inner_join(teams14, by=c("team_id" = "team_id"))

#Drop year.x and year.y
collegedf <- select(collegedf, -c(year.x, year.y))
#Drop N/A years from collegedf
collegedf <- subset(collegedf, collegedf$Yr != "N/A")
collegedf$Yr <- droplevels(collegedf$Yr)
#Check to make sure season still has all years
summary(collegedf)

#number of quarterbacks
collegedf %>%
  filter(Pos == "QB")

collegedf %>%
  filter(Pos == "RUSH")

collegedf %>%
  filter(Player == "Allen, Kyle")

#change position for Allen, Kyle to QB from RUSH
collegedf[collegedf$Pos == "RUSH", "Pos"] <- "QB"
#Change Yr from N/A to Fr
collegedf[collegedf$Yr == "N/A" & collegedf$Player == "Allen, Kyle", "Yr"] <- "Fr"
collegedf[collegedf$Pos == "OL" & collegedf$Player == "Reynolds, Micajah", "Pos"] <- "DL"
collegedf[collegedf$Pos == "OL" & collegedf$Player == "Rowell, Shaq", "Pos"] <- "DL"
collegedf[collegedf$Pos == "OL" & collegedf$Player == "King, C.", "Pos"] <- "DL"

str(collegedf)

collegedf %>%
  filter(Pos == "")

collegedf[collegedf$Pos == "WILL", "Pos"] <- "LB"
collegedf[collegedf$Pos == "WIL", "Pos"] <- "LB"
collegedf[collegedf$Pos == "WLB", "Pos"] <- "LB"
collegedf[collegedf$Pos == "SAM", "Pos"] <- "LB"
collegedf[collegedf$Pos == "Z", "Pos"] <- "WR"
collegedf[collegedf$Pos == "SS", "Pos"] <- "DB"
collegedf[collegedf$Pos == "S", "Pos"] <- "DB"
collegedf[collegedf$Pos == "FS", "Pos"] <- "DB"
collegedf[collegedf$Pos == "RT", "Pos"] <- "OL"
collegedf[collegedf$Pos == "RG", "Pos"] <- "OL"
collegedf[collegedf$Pos == "NT", "Pos"] <- "DL"
collegedf[collegedf$Pos == "NG", "Pos"] <- "DL"
collegedf[collegedf$Pos == "END", "Pos"] <- "DL"
collegedf[collegedf$Pos == "DT", "Pos"] <- "DL"
collegedf[collegedf$Pos == "DE", "Pos"] <- "DL"
collegedf[collegedf$Pos == "LT", "Pos"] <- "OL"
collegedf[collegedf$Pos == "LG", "Pos"] <- "OL"
collegedf[collegedf$Pos == "ILB", "Pos"] <- "LB"
collegedf[collegedf$Pos == "FB", "Pos"] <- "RB"
collegedf[collegedf$Pos == "CB", "Pos"] <- "DB"
collegedf[collegedf$Pos == "C", "Pos"] <- "OL"
collegedf[collegedf$Pos == "BUCK", "Pos"] <- "LB"
collegedf[collegedf$Pos == "BS", "Pos"] <- "DB"
collegedf[collegedf$Pos == "BC", "Pos"] <- "DB"
collegedf[collegedf$Pos == "B", "Pos"] <- "LB"
collegedf[collegedf$Pos == "" & collegedf$Player == "Pierce, Jordan", "Pos"] <- "LB"
collegedf[collegedf$Pos == "" & collegedf$Player == "Rushing, Devin", "Pos"] <- "RB"
collegedf[collegedf$Pos == "" & collegedf$Player == "Wharton, Donnie", "Pos"] <- "LB"
collegedf[collegedf$Pos == "" & collegedf$Player == "Linn, Hayes", "Pos"] <- "DB"
collegedf[collegedf$Pos == "" & collegedf$Player == "Clinton-Earl, Aaron", "Pos"] <- "RB"
collegedf[collegedf$Pos == "" & collegedf$Player == "Uzo-Okereke, Ar", "Pos"] <- "OL"
collegedf[collegedf$Pos == "" & collegedf$Player == "Onyechi, Jacob", "Pos"] <- "LB"
collegedf[collegedf$Pos == "" & collegedf$Player == "Dunn, Brett", "Pos"] <- "K"
collegedf[collegedf$Pos == "" & collegedf$Player == "Deeks, Lochlin", "Pos"] <- "DL"
collegedf[collegedf$Pos == "" & collegedf$Player == "Davis, Pate", "Pos"] <- "QB"

collegedf %>%
  filter(Player == "Deeks, Lochlin")
collegedf %>%
  filter(Player == "Davis, Pate")


#Write collegdf to csv
write.csv(collegedf, file = "college_data.csv")

#Create Data Frame so we can predict who gets drafted and who does not
basic_stats$copyofname <- basic_stats$Name
str(basic_stats)
college_draft <- left_join(collegedf, basic_stats, by = c("team_name" = "College", "Player" = "Name"))
college_draft
str(college_draft)

college_draft$was_drafted <- !is.na(college_draft$copyofname)
str(college_draft)

names(college_draft) <- make.names(names(college_draft))

#remove comma from Rush.YdsGained
college_draft[["Rush.YdsGained"]] <- as.numeric(gsub(",", "", college_draft[["Rush.YdsGained"]]))
college_draft[["Rush.Net.Yards"]] <- as.numeric(gsub(",", "", college_draft[["Rush.Net.Yards"]]))
#Change GP and GS to numeric
college_draft$GP <- as.numeric(college_draft$GP)
college_draft$GS <- as.numeric(college_draft$GS)
#Define a new variable Games Not Started GNS
college_draft$GNS <- (college_draft$GP - college_draft$GS)

#Drop "" from position
college_draft <- subset(college_draft, Pos != "")



#ave() function to update/change
first_season <- ave(collegedf$season, collegedf$Player, FUN=min)
#To see where data sits on the years
table(collegedf$season - first_season)
#Compare student Year to number of records
table(collegedf$Yr)

#Bar plot of season by year
ggplot(collegedf, aes(season, fill = Yr, group = Yr)) +
  geom_bar(position = "dodge")

#
ggplot(collegedf, aes(`Rush Attempts`, `Rush YdsLost`)) +
  geom_point()

#Player by position
ggplot(collegedf, aes(Pos, fill = season, group = season)) +
  geom_bar(position = "dodge")

ggplot(collegedf, aes(Pos, fill = Yr, group = Yr)) +
  geom_bar(position = "dodge")

#Players from which colleges in the NFL
college_teams <- inner_join(collegedf, basic_stats, by = c("team_name" = "College", "Player" = "Name")) 

college_teams
str(college_teams)
table(college_teams$season, college_teams$start_year)
#Shows if players switched positions from college to NFL
table(college_teams$Pos, college_teams$Position)

ggplot(filter(college_teams, start_year >= 2014), aes(x = team_name)) + geom_bar()

ggplot(filter(college_teams, start_year >= 2014), aes(x = team_name)) +
  geom_bar() +
  coord_cartesian(ylim=c(2,7))


#Change the reference group to RB. We need to make sure we are referencing the proper position to get the correct results.
collegedf$Pos <- relevel(as.factor(collegedf$Pos), ref = "RB")
#Linear regression on position and attempts
rushmodel <- lm(`Rush Attempts` ~ Pos, data=collegedf)

#Find by subsets or back joins why there is a large jump from  Freshman 2014 to So 2015 year
collegeFrSo <- inner_join(filter(collegedf, Yr == "Fr"), filter(collegedf, Yr == "So"), by = "Player")

#Put the Yr in specific order 
View(collegedf)
collegedf$Yr <- factor(collegedf$Yr, levels = c("Fr","So","Jr","Sr","N/A"))

#make some plots and tables, summary statistics and start filling in the write up.
summary(collegedf)
str(collegedf)
dim(collegedf)


play_pos <- table(collegedf$Pos)
play_pos

#table how many are in each year and N/A
missing_yr <- table(collegedf$Yr)
missing_yr

collegedf %>%
  filter(Yr == "N/A")


#Join basic stats and teams09
inner_join(basic_stats, texas14, by=c("Name" = "Player"))

basic_stats %>%
  inner_join(texas14, by=c("Name" = "Player")) %>%
  filter(College == "Texas", start_year >= 2014)

basic_stats %>% 
  select(Current.Status, Current.Team) %>% 
  group_by(Current.Status, Current.Team) %>% 
  summarise(n = n()) %>% 
  View()



#plot of are player bigger now than before
ggplot(basic_stats) +
  aes(x = as.numeric(start_year), y = Weight..lbs.) +
  geom_point() +
  geom_smooth() +
  labs(x = "Year", y = "Weight in Lbs")

ggplot(basic_stats) +
  aes(x = as.numeric(start_year), y = Height..inches.) +
  geom_jitter(alpha = .5) +
  geom_smooth() +
  labs(x = "Year", y = "Height in Lbs")
#maybe try boxplot
#Density Plot
ggplot(basic_stats, aes(Height..inches., fill = Current.Team)) +
         geom_density() +
  facet_wrap(~Current.Team) +
  guides(fill = "none") +
  labs(x = "Height in Inches")
      
#change to a scatter or boxplot
ggplot(subset(basic_stats, Experience <= 20), aes(Height..inches., fill = Experience)) +
  geom_density() +
  facet_wrap(~Experience) +
  guides(fill = "none") +
  labs(x = "Height in Inches")

#Experience and Team
ggplot(basic_stats, aes(Current.Team, Experience)) +
  geom_boxplot() +
  labs(x = "NFL Teams", y = "Years of Experience")

 #Linear Model
nfl_model <- lm(Experience ~ College, data = basic_stats)
summary(nfl_model)
nfl_model$residuals
SSE <- sum(nfl_model$residuals^2)
SSE

nfl_model2 <- lm(Experience ~ College + Weight..lbs., data = basic_stats)
summary(nfl_model2)
nfl_model2$residuals
SSE2 <- sum(nfl_model2$residuals^2)
SSE2

nfl_model3 <- lm(Experience ~ Height..inches. + Weight..lbs., data = basic_stats)
summary(nfl_model3)
nfl_model3$residuals
SSE3 <- sum(nfl_model3$residuals^2)
SSE3




#Linear Model for drafting
draft1 <- lm(was_drafted ~ poly(Height..inches., 2) + Weight..lbs., data = na.omit(college_draft[c("Height..inches.", "Weight..lbs.", "was_drafted")]))
summary(draft1)

table(college_draft$was_drafted)

draft2 <- lm(was_drafted ~ Yr + Pos + GP + Rush.Attempts + Rush.Net.Yards + Rush.YdsGained, data = college_draft)
summary(draft2)
#Discuss why there are differences in the drafting rates for the positions
ggplot(college_draft) + aes(x=Pos, fill = was_drafted) + geom_bar()


#Logistic Regression
set.seed(122)
split <- sample.split(college_draft$was_drafted, SplitRatio = 0.75)
split

#create training set
college_draftTrain <- subset(college_draft, split == TRUE)
nrow(college_draftTrain)

#Run the model on Train
#the subset has all colleges where at least one player was drafted
college_draftTrain <- subset(college_draftTrain, ave(college_draftTrain$was_drafted, college_draftTrain$team_name, FUN=sum) > 0)
#Build the Logistic Regression Model
college_draftLog <- glm(was_drafted ~ GP + Pos + Yr + team_name, family = binomial(), college_draftTrain)
summary(college_draftLog)
#Check why LS only has values for 2018
subset(college_draft, college_draft$Pos == "LS" & season == 2017)
#Build prediction of college_draftLog
predict_college_draftTrain <- predict(college_draftLog, newdata = college_draftTrain, type = "response")
summary(predict_college_draftTrain)
#To get the fn, fp, tn, tp
table(college_draftTrain$was_drafted, predict_college_draftTrain > mean(college_draftTrain$was_drafted))
#average predicted probabilities
tapply(predict_college_draftTrain, college_draftTrain$was_drafted,  FUN=mean)
#Precision and recall
#Accuracy
(9887 + 1616)/(9887 + 4862 + 447 + 1616)
#ROC Curve
ROCRpred_Train <- prediction

#create testing set
college_draftTest <- subset(college_draft, split == FALSE)
college_draftTest <- subset(college_draftTest, college_draftTest$team_name %in% college_draftTrain$team_name)
nrow(college_draftTest)
#Test prediction on Test data set
predict_Test <- predict(college_draftLog, newdata = college_draftTest, type = "response" )
summary(predict_Test)
table(college_draftTest$was_drafted, predict_Test > mean(college_draftTrain$was_drafted))
#Accuracy
(3479 + 544)/(3479 + 1593 + 144 + 544)
#ROC Curve
ROCRpred_Test <- prediction(predict_Test, college_draftTest$was_drafted)
ROCRperf_Test <- performance(ROCRpred_Test, "tpr", "fpr")
plot(ROCRperf_Test)



#Logistic Regression 2
#create training set
college_draftTrain2 <- subset(college_draft, split == TRUE)
nrow(college_draftTrain2)

#Run the model on Train2
#the subset has all colleges where at least one player was drafted
college_draftTrain2 <- subset(college_draftTrain2, ave(college_draftTrain2$was_drafted, college_draftTrain2$team_name, FUN=sum) > 0)
#Build the Logistic Regression Model
college_draftLog2 <- glm(was_drafted ~ Pos + GP + GS + season + team_name, family = binomial(), college_draftTrain2)
summary(college_draftLog2)
#Build prediction of college_draftLog2
predict_college_draftTrain2 <- predict(college_draftLog2, newdata = college_draftTrain2, type = "response")
summary(predict_college_draftTrain2)
#Get the fn, fp, tn, tp
table(college_draftTrain2$was_drafted, predict_college_draftTrain2 > mean(college_draftTrain2$was_drafted))
#average predicted probabilities
tapply(predict_college_draftTrain2, college_draftTrain2$was_drafted,  FUN=mean)
#Accuracy
(11291 + 1651)/(11291 + 3456 + 412 + 1651)

#create testing set
college_draftTest2 <- subset(college_draft, split == FALSE)
nrow(college_draftTest2)
#the subset has all colleges where at least one player was drafted in test
college_draftTest2 <- subset(college_draftTest2, ave(college_draftTest2$was_drafted, college_draftTest2$team_name, FUN=sum) > 0)
#Build test prediction of college_draftTestLog
predict_cdTest2 <- predict(college_draftLog2, newdata = college_draftTest2, type = "response")
summary(predict_cdTest2)
#To get the fn, fp, tn, tp
table(college_draftTest2$was_drafted, predict_cdTest2 > mean(college_draftTest2$was_drafted))
#average predicted probabilities on Test
tapply(predict_cdTest2, college_draftTest2$was_drafted, FUN=mean)
#Accuracy
(3811 + 558)/(3811 + 1181 + 130 + 558)
#ROC Curve
ROCRpred_Test2 <- prediction(predict_cdTest2, college_draftTest2$was_drafted)
ROCRperf_Test2 <- performance(ROCRpred_Test2, "tpr", "fpr")
plot(ROCRperf_Test2)
#See if we can stack plots from first test and second test

