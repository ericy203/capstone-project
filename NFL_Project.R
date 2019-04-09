#library(sqldf)
library(devtools)
library(readr) 
library(dplyr)
library(tidyr)
library(ggplot2)
setwd("~/Documents/School/Springboard/Project/nflstatistics")
basic_stats <- read.csv("Basic_Stats.csv", header = TRUE, stringsAsFactors = FALSE)
cs_defensive <- read.csv("Career_Stats_Defensive.csv", stringsAsFactors = FALSE)
cs_fg_kickers <- read.csv("Career_Stats_Field_Goal_Kickers.csv", stringsAsFactors = FALSE)
cs_fumbles <- read.csv("Career_Stats_Fumbles.csv", stringsAsFactors = FALSE)
cs_kreturn <- read.csv("Career_Stats_Kick_Return.csv", stringsAsFactors = FALSE)
cs_kickoff <- read.csv("Career_Stats_Kickoff.csv", stringsAsFactors = FALSE)
cs_ol <- read.csv("Career_Stats_Offensive_Line.csv", stringsAsFactors = FALSE)
cs_passing <- read.csv("Career_Stats_Passing.csv", stringsAsFactors = FALSE)
cs_preturn <- read.csv("Career_Stats_Punt_Return.csv", stringsAsFactors = FALSE)
cs_punting <- read.csv("Career_Stats_Punting.csv", stringsAsFactors = FALSE)
cs_receiving <- read.csv("Career_Stats_Receiving.csv", stringsAsFactors = FALSE)
cs_rushing <- read.csv("Career_Stats_Rushing.csv", stringsAsFactors = FALSE)
gl_dl <- read.csv("Game_Logs_Defensive_Lineman.csv", stringsAsFactors = FALSE)
gl_k <- read.csv("Game_Logs_Kickers.csv", stringsAsFactors = FALSE)
gl_ol <- read.csv("Game_Logs_Offensive_Line.csv", stringsAsFactors = FALSE)
gl_p <- read.csv("Game_Logs_Punters.csv", stringsAsFactors = FALSE)
gl_qb <- read.csv("Game_Logs_Quarterback.csv", stringsAsFactors = FALSE)
gl_rb <- read.csv("Game_Logs_Runningback.csv", stringsAsFactors = FALSE)
gl_wr_te <- read.csv("Game_Logs_Wide_Receiver_and_Tight_End.csv", stringsAsFactors = FALSE)


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

View(basic_stats %>%
  select(Name, Player.Id, Position))

View(basic_stats %>%
       select(Name, College, start_year) %>%
       filter(start_year >= 2014))

#It seems the CollegeballR package only has players statistics from 2014 and forward.
#This shows there are only 304 players who meet this criteria.
summary(basic_stats %>%
       select(Name, College, start_year) %>%
       filter(start_year >= 2014))

basic_stats %>%
  select(Name, College, start_year) %>%
  filter(start_year > 2006, College == "Texas")

#Try to tie in collegeballR rpackage 
library(collegeballR)

#Assign the team mapping for 2009 to variable
#this just shows the teams and the codes you need to use to get information from player_stats
teams09 <- team_mapping(2009, "MFB")
teams09
teams14 <- team_mapping(2014, "MFB")
str(teams14)
teams14

teams05 <- team_mapping(2005, "MFB")
teams05

#Team Stats
team_stats(8,2016,"MFB",by="Season")
team_stats(796,2016,"MFB",by="SEASON")
team_stats(697,2016,"MFB",by="SEASON")

ts_list <- list() 
for (a in 2014:2018) {
  current_season_tm <- team_mapping(a, "MFB")
  ts_list[[a]] <- list()
  for (i in current_season_tm$team_id) {
    try(ts_list[[a]][[i]] <- team_stats(as.numeric(i), a, "MFB", by="season"))
  }
}

teamdf <- data.frame()
for (a in 2014:2018) {
  for (i in names(ts_list[[a]])) {
    ts_df <- ts_list[[a]][[i]]
    ts_df$season <- a
    teamdf <- rbind(teamdf, ts_df)
  }
}

#Player Stats 
player_stats(732, 2014, "MFB", by="Season")
player_stats(703, 2014, "MFB", by="Season")
#Assign player stats from Texas year 2014 to texas14
texas15 <- player_stats(703, 2015, "MFB", by="Season")
texas14 <- player_stats(703, 2014, "MFB", by="Season")

#get all data from collegeballR and put in a list of lists called ps_list
ps_list <- list() 
for (y in 2014:2018) {
  current_season_tm <- team_mapping(y, "MFB")
ps_list[[y]] <- list()
for (i in current_season_tm$team_id) {
try(ps_list[[y]][[i]] <- player_stats(as.numeric(i), y, "MFB", by="Season"))
}
}
print(i)

#ps_list[[year]][[team_id]]

collegedf <- data.frame()
for (y in 2014:2018) {
  for (i in names(ps_list[[y]])) {
    df <- ps_list[[y]][[i]]
    df$season <- y
    collegedf <- rbind(collegedf, df)
  }
}

#Add team name
collegedf2 <- collegedf %>%
  mutate(team_id = as.character(team_id)) %>%
  inner_join(teams14, by=c("team_id" = "team_id"))

#Drop year.x and year.y
collegedf2 <- select(collegedf2, -c(year.x, year.y))

#Check to make sure season still has all years
summary(collegedf2)

#number of quarterbacks
collegedf2 %>%
  filter(Pos == "QB")

collegedf2 %>%
  filter(Pos == "RUSH")

collegedf2 %>%
  filter(Player == "Allen, Kyle")

#change position for Allen, Kyle to QB from RUSH
collegedf2[collegedf2$Pos == "RUSH", "Pos"] <- "QB"
#Change Yr from N/A to Fr
collegedf2[collegedf2$Yr == "N/A" & collegedf2$Player == "Allen, Kyle", "Yr"] <- "Fr"

#ave() function to update/change
first_season <- ave(collegedf2$season, collegedf2$Player, FUN=min)
#To see where data sits on the years
table(collegedf2$season - first_season)
#Compare student Year to number of records
table(collegedf2$Yr)

ggplot(collegedf2, aes(Yr, fill = season, group = season)) + 
  geom_bar(position = "dodge")
#Bar plot of season by year
ggplot(collegedf2, aes(season, fill = Yr, group = Yr)) +
  geom_bar(position = "dodge")

#
ggplot(collegedf2, aes(`Rush Attempts`, `Rush YdsLost`)) +
  geom_point()

#Player by position
ggplot(collegedf2, aes(Pos, fill = season, group = season)) +
  geom_bar(position = "dodge")

ggplot(collegedf2, aes(Pos, fill = Yr, group = Yr)) +
  geom_bar(position = "dodge")

#Players from which colleges in the NFL
college_teams <- inner_join(collegedf2, basic_stats, by = c("team_name" = "College", "Player" = "Name")) 

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
collegedf2$Pos <- relevel(as.factor(collegedf2$Pos), ref = "RB")
#Linear regression on position and attempts
rushmodel <- lm(`Rush Attempts` ~ Pos, data=collegedf2)

#Find by subsets or back joins why there is a large jump from  Freshman 2014 to So 2015 year
collegeFrSo <- inner_join(filter(collegedf2, Yr == "Fr"), filter(collegedf2, Yr == "So"), by = "Player")

#Put the Yr in specific order 
View(collegedf2)
collegedf2$Yr <- factor(collegedf2$Yr, levels = c("Fr","So","Jr","Sr","N/A"))

#make some plots and tables, summary statistics and start filling in the write up.
summary(collegedf2)
str(collegedf2)
dim(collegedf2)


play_pos <- table(collegedf2$Pos)
play_pos

#table how many are in each year and N/A
missing_yr <- table(collegedf2$Yr)
missing_yr

collegedf2 %>%
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


library(ggplot2)

#plot of are player bigger now than before
ggplot(basic_stats) +
  aes(x = as.numeric(start_year), y = Weight..lbs.) +
  geom_point() +
  geom_smooth() +
  labs(x = "Year", y = "Weight in Lbs")

ggplot(basic_stats) +
  aes(x = as.numeric(start_year), y = Height..inches.) +
  geom_point() +
  geom_smooth() +
  labs(x = "Year", y = "Height in Lbs")

#Density Plot
ggplot(basic_stats, aes(Height..inches., fill = Current.Team)) +
         geom_density() +
  facet_wrap(~Current.Team) +
  guides(fill = "none") +
  labs(x = "Height in Inches")
      
#
ggplot(subset(basic_stats, Experience <= 20), aes(Height..inches., fill = Experience)) +
  geom_density() +
  facet_wrap(~Experience) +
  guides(fill = "none") +
  labs(x = "Height in Inches")

#Experience and Team
ggplot(basic_stats, aes(Current.Team, Experience)) +
  geom_boxplot() +
  labs(x = "NFL Teams", y = "Years of Experience")

dnorm()