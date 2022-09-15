########## Predicting soccer outcomes using the Poisson distribution ##########

########################## Load the libraries needed ##########################
library(ggplot2)
library(stringr)
library(gridExtra)
library(MASS)
library(survival)
library(fitdistrplus)
library(extraDistr)
###############################################################################

############################## 0. Load the data ###############################
#Load the data we want to predict
LaLiga2021_22 = read.csv("LaLiga2021-22.csv") #Data set for the season 2021/22

#Load the historical data from the season 2014/15 to 2019/20
LaLiga2014_2020 = read.csv2("HistoricalDataLaLiga2014_2020.csv")
###############################################################################

################### 1. Study and preprocessing of the data ####################
#Preprocessing of the 2021/22 season data
LaLiga2021_22 = LaLiga2021_22[, c(2,4,5,6,7,24,25,26)] #Select columns needed
colnames(LaLiga2021_22) = c('Season', 'HomeTeam', 'AwayTeam', 'HomeGoals', 
                            'AwayGoals', 'Bet365Home', 'Bet365Draw', 
                            'Bet365Away') #Change the name of the variables
GameWeek = c(rep(1,10), rep(2,10), rep(3,10), rep(4,8), rep(5,10), rep(6,10), 
             rep(7,10), rep(8,10), rep(9,8), rep(10,10), rep(11,10), rep(12,10), 
             rep(13,10), rep(14,10), rep(15,10), 9, rep(16,10), rep(17,10), 
             rep(18,10), rep(4,2), 9, 21, rep(19,10), rep(20,10), rep(21,6), 
             rep(22,10), rep(23,10), rep(24,10), 21, rep(25,10), rep(26,10), 21, 
             rep(27,10), rep(28,10), rep(29,10), rep(30,10), rep(31,10), 
             rep(32,10), rep(33,10), 21, rep(34,10), rep(35,10), rep(36,10), 
             rep(37,10), rep(38,10)) 
LaLiga2021_22$GameWeek = GameWeek #Create new variable with gameweek of each match
LaLiga2021_22$Season = rep('2021/2022', 38) #Create a variable with the season 
LaLiga2021_22 = LaLiga2021_22[order(LaLiga2021_22$GameWeek), ] #Order the rows according to the gameweek
LaLiga2021_22$Index = 1:nrow(LaLiga2021_22) #Create a variable with the index

#Preprocessing of the historical data
colnames(LaLiga2014_2020)[1] = 'Team' #Change the name of the first variable to 'Team'
LaLiga2014_2020 = LaLiga2014_2020[order(LaLiga2014_2020$Team),] #Order the rows in alphabetical order according to the teams
LaLiga2014_2020$Index = 1:nrow(LaLiga2014_2020) #Create a variable with the index
LaLiga2014_2020 = LaLiga2014_2020[-c(142:179, 1227:1264, 1265:1480, 1915:1990, 
                                     2132:2576, 2756:2907, 3650:3725, 
                                     3943:4007),] #Remove the teams that are not in La Liga the season 2021/2022
LaLiga2014_2020 = LaLiga2014_2020[,-6] #Remove the variable index

#Change of team names to their diminutives for a better look of further plots
##2021/2022 season data
LaLiga2021_22$HomeTeam =  str_replace_all(LaLiga2021_22$HomeTeam, 
                                          "Alaves", "ALV")
LaLiga2021_22$HomeTeam =  str_replace_all(LaLiga2021_22$HomeTeam, 
                                          "Ath Bilbao", "ATH")
LaLiga2021_22$HomeTeam =  str_replace_all(LaLiga2021_22$HomeTeam, 
                                          "Ath Madrid", "ATM")
LaLiga2021_22$HomeTeam =  str_replace_all(LaLiga2021_22$HomeTeam, 
                                          "Barcelona", "BAR")
LaLiga2021_22$HomeTeam =  str_replace_all(LaLiga2021_22$HomeTeam, 
                                          "Cadiz", "CAD")
LaLiga2021_22$HomeTeam =  str_replace_all(LaLiga2021_22$HomeTeam, 
                                          "Celta", "CEL")
LaLiga2021_22$HomeTeam =  str_replace_all(LaLiga2021_22$HomeTeam, 
                                          "Elche", "ECF")
LaLiga2021_22$HomeTeam =  str_replace_all(LaLiga2021_22$HomeTeam, 
                                          "Espanol", "ESP")
LaLiga2021_22$HomeTeam =  str_replace_all(LaLiga2021_22$HomeTeam, 
                                          "Granada", "GRA")
LaLiga2021_22$HomeTeam =  str_replace_all(LaLiga2021_22$HomeTeam, 
                                          "Getafe", "GTF")
LaLiga2021_22$HomeTeam =  str_replace_all(LaLiga2021_22$HomeTeam, 
                                          "Levante", "LUD")
LaLiga2021_22$HomeTeam =  str_replace_all(LaLiga2021_22$HomeTeam, 
                                          "Mallorca", "MLL")
LaLiga2021_22$HomeTeam =  str_replace_all(LaLiga2021_22$HomeTeam, 
                                          "Osasuna", "OSA")
LaLiga2021_22$HomeTeam =  str_replace_all(LaLiga2021_22$HomeTeam, 
                                          "Betis", "RBB")
LaLiga2021_22$HomeTeam =  str_replace_all(LaLiga2021_22$HomeTeam, 
                                          "Real Madrid", "RMA")
LaLiga2021_22$HomeTeam =  str_replace_all(LaLiga2021_22$HomeTeam, 
                                          "Sociedad", "RSO")
LaLiga2021_22$HomeTeam =  str_replace_all(LaLiga2021_22$HomeTeam, 
                                          "Vallecano", "RVA")
LaLiga2021_22$HomeTeam =  str_replace_all(LaLiga2021_22$HomeTeam, 
                                          "Sevilla", "SFC")
LaLiga2021_22$HomeTeam =  str_replace_all(LaLiga2021_22$HomeTeam, 
                                          "Valencia", "VCF")
LaLiga2021_22$HomeTeam =  str_replace_all(LaLiga2021_22$HomeTeam, 
                                          "Villarreal", "VIL")
LaLiga2021_22$AwayTeam =  str_replace_all(LaLiga2021_22$AwayTeam, 
                                          "Alaves", "ALV")
LaLiga2021_22$AwayTeam =  str_replace_all(LaLiga2021_22$AwayTeam, 
                                          "Ath Bilbao", "ATH")
LaLiga2021_22$AwayTeam =  str_replace_all(LaLiga2021_22$AwayTeam, 
                                          "Ath Madrid", "ATM")
LaLiga2021_22$AwayTeam =  str_replace_all(LaLiga2021_22$AwayTeam, 
                                          "Barcelona", "BAR")
LaLiga2021_22$AwayTeam =  str_replace_all(LaLiga2021_22$AwayTeam, 
                                          "Cadiz", "CAD")
LaLiga2021_22$AwayTeam =  str_replace_all(LaLiga2021_22$AwayTeam, 
                                          "Celta", "CEL")
LaLiga2021_22$AwayTeam =  str_replace_all(LaLiga2021_22$AwayTeam, 
                                          "Elche", "ECF")
LaLiga2021_22$AwayTeam =  str_replace_all(LaLiga2021_22$AwayTeam, 
                                          "Espanol", "ESP")
LaLiga2021_22$AwayTeam =  str_replace_all(LaLiga2021_22$AwayTeam, 
                                          "Granada", "GRA")
LaLiga2021_22$AwayTeam =  str_replace_all(LaLiga2021_22$AwayTeam, 
                                          "Getafe", "GTF")
LaLiga2021_22$AwayTeam =  str_replace_all(LaLiga2021_22$AwayTeam, 
                                          "Levante", "LUD")
LaLiga2021_22$AwayTeam =  str_replace_all(LaLiga2021_22$AwayTeam, 
                                          "Mallorca", "MLL")
LaLiga2021_22$AwayTeam =  str_replace_all(LaLiga2021_22$AwayTeam, 
                                          "Osasuna", "OSA")
LaLiga2021_22$AwayTeam =  str_replace_all(LaLiga2021_22$AwayTeam, 
                                          "Betis", "RBB")
LaLiga2021_22$AwayTeam =  str_replace_all(LaLiga2021_22$AwayTeam, 
                                          "Real Madrid", "RMA")
LaLiga2021_22$AwayTeam =  str_replace_all(LaLiga2021_22$AwayTeam, 
                                          "Sociedad", "RSO")
LaLiga2021_22$AwayTeam =  str_replace_all(LaLiga2021_22$AwayTeam, 
                                          "Vallecano", "RVA")
LaLiga2021_22$AwayTeam =  str_replace_all(LaLiga2021_22$AwayTeam, 
                                          "Sevilla", "SFC")
LaLiga2021_22$AwayTeam =  str_replace_all(LaLiga2021_22$AwayTeam, 
                                          "Valencia", "VCF")

LaLiga2021_22$AwayTeam =  str_replace_all(LaLiga2021_22$AwayTeam, 
                                          "Villarreal", "VIL")
##Historical data
LaLiga2014_2020$Team =  str_replace_all(LaLiga2014_2020$Team, 
                                        "Alaves", "ALV")
LaLiga2014_2020$Team =  str_replace_all(LaLiga2014_2020$Team, 
                                        "Ath Bilbao", "ATH")
LaLiga2014_2020$Team =  str_replace_all(LaLiga2014_2020$Team, 
                                        "Ath Madrid", "ATM")
LaLiga2014_2020$Team =  str_replace_all(LaLiga2014_2020$Team, 
                                        "Barcelona", "BAR")
LaLiga2014_2020$Team =  str_replace_all(LaLiga2014_2020$Team, 
                                        "Cadiz", "CAD")
LaLiga2014_2020$Team =  str_replace_all(LaLiga2014_2020$Team, 
                                        "Celta", "CEL")
LaLiga2014_2020$Team =  str_replace_all(LaLiga2014_2020$Team, 
                                        "Elche", "ECF")
LaLiga2014_2020$Team =  str_replace_all(LaLiga2014_2020$Team, 
                                        "Espanol", "ESP")
LaLiga2014_2020$Team =  str_replace_all(LaLiga2014_2020$Team, 
                                        "Granada", "GRA")
LaLiga2014_2020$Team =  str_replace_all(LaLiga2014_2020$Team, 
                                        "Getafe", "GTF")
LaLiga2014_2020$Team =  str_replace_all(LaLiga2014_2020$Team, 
                                        "Levante", "LUD")
LaLiga2014_2020$Team =  str_replace_all(LaLiga2014_2020$Team, 
                                        "Mallorca", "MLL")
LaLiga2014_2020$Team =  str_replace_all(LaLiga2014_2020$Team, 
                                        "Osasuna", "OSA")
LaLiga2014_2020$Team =  str_replace_all(LaLiga2014_2020$Team, 
                                        "Betis", "RBB")
LaLiga2014_2020$Team =  str_replace_all(LaLiga2014_2020$Team, 
                                        "Real Madrid", "RMA")
LaLiga2014_2020$Team =  str_replace_all(LaLiga2014_2020$Team, 
                                        "Sociedad", "RSO")
LaLiga2014_2020$Team =  str_replace_all(LaLiga2014_2020$Team, 
                                        "Vallecano", "RVA")
LaLiga2014_2020$Team =  str_replace_all(LaLiga2014_2020$Team, 
                                        "Sevilla", "SFC")
LaLiga2014_2020$Team =  str_replace_all(LaLiga2014_2020$Team, 
                                        "Valencia", "VCF")
LaLiga2014_2020$Team =  str_replace_all(LaLiga2014_2020$Team, 
                                        "Villarreal", "VIL")

#Separate the odds from LaLiga 2021-2022 data set
LaLiga2021_22_BettingOdds = LaLiga2021_22[,c(2,3,6,7,8,9)]
LaLiga2021_22 = LaLiga2021_22[,c(1,2,3,4,5,9,10)]

#Summary of data sets
summary(LaLiga2021_22)
summary(LaLiga2014_2020)

#We are going to assume that the first half of the season has been played and the second half remains to be played. 
LaLiga2021_22_SecondHalf = LaLiga2021_22[191:380,-7] #Data set with matches of the second half 
LaLiga2021_22_FirstHalf = LaLiga2021_22[1:190, -7] #Data set with matches and results of the first half 

#We add the goals scored and conceded by each team in the fist half of the season 2021-2022 to the historical data
Team = c(LaLiga2021_22_FirstHalf$HomeTeam, LaLiga2021_22_FirstHalf$AwayTeam)
Home = c(rep(1,190), rep(0,190))
GoalsScored = c(LaLiga2021_22_FirstHalf$HomeGoals, 
                LaLiga2021_22_FirstHalf$AwayGoals)
GoalsConceded = c(LaLiga2021_22_FirstHalf$AwayGoals, 
                  LaLiga2021_22_FirstHalf$HomeGoals)
Season = rep('2021/2022', 380)
HistoricalData2021_22 = data.frame(Team, Home, GoalsScored, GoalsConceded, 
                                   Season)
LaLiga2014_2021 = rbind(LaLiga2014_2020, HistoricalData2021_22) 
LaLiga2014_2021 = LaLiga2014_2021[order(LaLiga2014_2021$Team),]

#Plot of the results of the first half from the season 2021/2022
LaLiga2021_22_FirstHalf$HomeTeam = factor(LaLiga2021_22_FirstHalf$HomeTeam)
LaLiga2021_22_FirstHalf$AwayTeam = factor(LaLiga2021_22_FirstHalf$AwayTeam)
ggplot(data = LaLiga2021_22_FirstHalf, aes(AwayTeam, HomeTeam, fill = 
                                             HomeGoals - AwayGoals)) + 
  geom_tile() + 
  geom_label(aes(label = paste(HomeGoals, AwayGoals, sep = "-")), 
             fill = "white") + 
  scale_fill_gradient2(low = "red", high = "green", midpoint = 0, 
                       guide = FALSE) +
  scale_x_discrete(limits = levels(LaLiga2021_22_FirstHalf$AwayTeam), 
                   position = "top") + 
  scale_y_discrete(limits = rev(levels(LaLiga2021_22_FirstHalf$HomeTeam))) +
  theme_minimal() + 
  ggtitle ("Results of the first half of the season 2021/2022") +
  theme (plot.title = element_text(vjust=2, hjust = 0.5, face="bold", 
                                   lineheight=1.5)) +
  labs(x = "Away Team", y = "Home Team") + 
  theme(axis.title.x = element_text(face="bold", vjust=1)) +
  theme(axis.title.y = element_text(face="bold", vjust=1.5))

#Function to calculate the points of a team (3 points if the team wins, 1 if draws and 0 if loses)
PointsCalculator = function(team){
  points = 0
  for (i in 1:nrow(LaLiga2021_22_FirstHalf)) {
    if (LaLiga2021_22_FirstHalf$HomeTeam[i] == team){
      if (LaLiga2021_22_FirstHalf$HomeGoals[i] > LaLiga2021_22_FirstHalf$AwayGoals[i]){ 
        points = points + 3
      }
      else if (LaLiga2021_22_FirstHalf$HomeGoals[i] == LaLiga2021_22_FirstHalf$AwayGoals[i]){
        points = points + 1
      }
    }
    if (LaLiga2021_22_FirstHalf$AwayTeam[i] == team){
      if (LaLiga2021_22_FirstHalf$HomeGoals[i] < LaLiga2021_22_FirstHalf$AwayGoals[i]){ 
        points = points + 3
      }
      else if (LaLiga2021_22_FirstHalf$HomeGoals[i] == LaLiga2021_22_FirstHalf$AwayGoals[i]){
        points = points + 1
      }
    }
  }
  return(points)
}

#Data frame with the league table until the first half of the league
LaLiga2021_22_FirstHalf$HomeTeam = factor(LaLiga2021_22_FirstHalf$HomeTeam)
Team = c()
Points = c()
for (i in levels(LaLiga2021_22_FirstHalf$HomeTeam)) {
  Team = c(Team, i)
  Points = c(Points, PointsCalculator(i))
}
Classification_FirstRound_nonordered = data.frame(Team, Points)
Classification_FirstRound =  Classification_FirstRound_nonordered[order(
  Classification_FirstRound_nonordered$Points, decreasing = TRUE), ]
Classification_FirstRound$Position = 1:20
Classification_FirstRound = Classification_FirstRound[ , c(3,1,2)]

#Bar plots of the goals scored
LaLiga2014_2021$Home = factor(LaLiga2014_2021$Home)
LaLiga2014_2021_Home = LaLiga2014_2021[LaLiga2014_2021$Home == 1,]
LaLiga2014_2021_Away = LaLiga2014_2021[LaLiga2014_2021$Home == 0,]

##Bar plot of the goals scored when playing at home
ggplot(LaLiga2014_2021_Home, aes(x = factor(GoalsScored))) + 
  geom_bar(stat="count", colour = 'darkblue', fill="deepskyblue1") + 
  labs(title = 'Bar plot of the goals scored at home', x = "Goals Scored",  
       y = "Absolut frequencies") + 
  theme(axis.title.x = element_text(vjust=-0.5, size=rel(1))) +
  theme(axis.title.y = element_text(vjust=3, size=rel(1))) + 
  theme_minimal()

##Bar plot of the goals scored when playing away
ggplot(LaLiga2014_2021_Away, aes(x = factor(GoalsScored))) + 
  geom_bar(stat="count", colour = 'darkred', fill="coral1") + 
  labs(title = 'Bar plot of the goals scored away', x = "Goals Scored",  
       y = "Absolut frequencies") + 
  theme(axis.title.x = element_text(vjust=-0.5, size=rel(1))) +
  theme(axis.title.y = element_text(vjust=3, size=rel(1))) + 
  theme_minimal()

##Bar plot of both, goals scored home and away
LaLiga2014_2021$Home2 <- factor(LaLiga2014_2021$Home, labels = c("Away", 
                                                                 "Home"))
ggplot(LaLiga2014_2021, aes(factor(GoalsScored), fill = Home2)) +
  geom_bar(stat="count", position=position_dodge()) +
  scale_fill_manual(values = c("coral1", "deepskyblue1")) +
  labs(title = 'Bar plots of the goals scored at home and away', 
       x = "Goals Scored",  y = "Absolut frequencies") +
  theme(axis.title.x = element_text(vjust=-0.5, size=rel(1))) +
  theme(axis.title.y = element_text(vjust=3, size=rel(1))) + 
  theme_minimal() +
  theme(legend.position = c(0.8, 0.6)) + 
  labs(fill = " ") 

#Mean of goals scored when playing at home and away
Mean_Home = mean(LaLiga2014_2021$GoalsScored[LaLiga2014_2021$Home == 1])
Mean_Away = mean(LaLiga2014_2021$GoalsScored[LaLiga2014_2021$Home == 0])
Mean_Home
Mean_Away

#Chi^2 test for Home teams
lambda_home = fitdist(LaLiga2014_2021_Home$GoalsScored, 'pois')
test_home = gofstat(lambda_home, chisqbreaks = c(0,1,2,3), discrete = TRUE)
test_home
#Chi^2 test for Away teams
lambda_away = fitdist(LaLiga2014_2021_Away$GoalsScored, 'pois')
test_away = gofstat(lambda_away, chisqbreaks = c(0,1,2), discrete = TRUE)
test_away
################################################################################

################################### 2. Models ##################################
#Calculate the attacking and defensive power of each team when playing at home and away
LaLiga2014_2021$Team = factor(LaLiga2014_2021$Team) #Set variable Team as factor
alpha = aggregate(LaLiga2014_2021$GoalsScored, by = list(LaLiga2014_2021$Team, 
                                                         LaLiga2014_2021$Home), 
                  FUN = 'mean') #Attacking rate
beta = aggregate(LaLiga2014_2021$GoalsConceded, by = list(LaLiga2014_2021$Team, 
                                                          LaLiga2014_2021$Home), 
                 FUN = 'mean') #Defensive rate
AttackDefensiveRates = data.frame('Team' = levels(LaLiga2014_2021$Team), 
                                  'HomeGoalsScored' = alpha$x[alpha$Group.2 == 1], 
                                  'AwayGoalsScored' = alpha$x[alpha$Group.2 == 0], 
                                  'HomeGoalsConceded' = beta$x[beta$Group.2 == 1], 
                                  'AwayGoalsConceded' = beta$x[beta$Group.2 == 0])

#Function that predicts the most probable result given the home team, and the away team
maxgoals = max(max(LaLiga2014_2021$GoalsScored), max(LaLiga2014_2021$GoalsConceded))
Result_predict = function(HomeTeam, AwayTeam){
  HomeGoals = 0
  AwayGoals = 0
  prob_final_home = 0
  prob_final_away = 0
  lambda_home = AttackDefensiveRates$HomeGoalsScored[AttackDefensiveRates$Team 
                                                     == HomeTeam] * 
    AttackDefensiveRates$AwayGoalsConceded[AttackDefensiveRates$Team == AwayTeam]
  lambda_away = AttackDefensiveRates$AwayGoalsScored[AttackDefensiveRates$Team 
                                                     == AwayTeam] * 
    AttackDefensiveRates$HomeGoalsConceded[AttackDefensiveRates$Team == HomeTeam]
  for (i in 0:10) {
    prob_home = dtpois(i, lambda_home, b = 10)
    prob_away = dtpois(i, lambda_away, b = 10)
    if (prob_home > prob_final_home){
      prob_final_home = prob_home
      HomeGoals = i
    }
    if (prob_away > prob_final_away){
      prob_final_away = prob_away
      AwayGoals = i
    }
  }
  return(c(HomeGoals, AwayGoals))
}

#We run the previous function for all the remaining matches of 2021/2022 season
LaLiga2021_22_SecondHalf_predict = LaLiga2021_22_SecondHalf[,c(1,2,3,6)]
HomeGoalsPredicted = c()
AwayGoalsPredicted = c()
for (i in 1:nrow(LaLiga2021_22_SecondHalf)) {
  HomeGoalsPredicted = c(HomeGoalsPredicted, 
                         Result_predict(LaLiga2021_22_SecondHalf$HomeTeam[i], 
                                        LaLiga2021_22_SecondHalf$AwayTeam[i])[1])
  AwayGoalsPredicted = c(AwayGoalsPredicted, 
                         Result_predict(LaLiga2021_22_SecondHalf$HomeTeam[i], 
                                        LaLiga2021_22_SecondHalf$AwayTeam[i])[2])
} 

LaLiga2021_22_SecondHalf_predict$HomeGoalsPredicted = HomeGoalsPredicted
LaLiga2021_22_SecondHalf_predict$AwayGoalsPredicted = AwayGoalsPredicted
LaLiga2021_22_SecondHalf_predict = LaLiga2021_22_SecondHalf_predict[,c(1,2,3,5,6,4)]
################################################################################

########################### 3. Results and conclusions #########################
#We create a new column to determine which team wins or if they draw
##Real results
LaLiga2021_22_SecondHalf$Difference = LaLiga2021_22_SecondHalf$HomeGoals - LaLiga2021_22_SecondHalf$AwayGoals
LaLiga2021_22_SecondHalf$WinLoseDraw[LaLiga2021_22_SecondHalf$Difference > 0] = 1 #1 if the home team wins
LaLiga2021_22_SecondHalf$WinLoseDraw[LaLiga2021_22_SecondHalf$Difference < 0] = 2 #2 if the away team wins
LaLiga2021_22_SecondHalf$WinLoseDraw[LaLiga2021_22_SecondHalf$Difference == 0] = 3 #3 if draw
##Predicted results
LaLiga2021_22_SecondHalf_predict$Difference = 
  LaLiga2021_22_SecondHalf_predict$HomeGoalsPredicted - LaLiga2021_22_SecondHalf_predict$AwayGoalsPredicted
LaLiga2021_22_SecondHalf_predict$WinLoseDraw[LaLiga2021_22_SecondHalf_predict$Difference > 0] = 1 #1 if the home team wins
LaLiga2021_22_SecondHalf_predict$WinLoseDraw[LaLiga2021_22_SecondHalf_predict$Difference < 0] = 2 #2 if the away team wins
LaLiga2021_22_SecondHalf_predict$WinLoseDraw[LaLiga2021_22_SecondHalf_predict$Difference == 0] = 3 #3 if draw

#Confusion matrix
T1_P1 = sum(LaLiga2021_22_SecondHalf_predict$WinLoseDraw[LaLiga2021_22_SecondHalf$WinLoseDraw == 1] == 1) #True 1's predicted as 1's
T2_P2 = sum(LaLiga2021_22_SecondHalf_predict$WinLoseDraw[LaLiga2021_22_SecondHalf$WinLoseDraw == 2] == 2) #True 2's predicted as 2's
T3_P3 = sum(LaLiga2021_22_SecondHalf_predict$WinLoseDraw[LaLiga2021_22_SecondHalf$WinLoseDraw == 3] == 3) #True 3's predicted as 3's
T1_P2 = sum(LaLiga2021_22_SecondHalf_predict$WinLoseDraw[LaLiga2021_22_SecondHalf$WinLoseDraw == 1] == 2) #True 1's predicted as 2's
T1_P3 = sum(LaLiga2021_22_SecondHalf_predict$WinLoseDraw[LaLiga2021_22_SecondHalf$WinLoseDraw == 1] == 3) #True 1's predicted as 3's
T2_P1 = sum(LaLiga2021_22_SecondHalf_predict$WinLoseDraw[LaLiga2021_22_SecondHalf$WinLoseDraw == 2] == 1) #True 2's predicted as 1's
T2_P3 = sum(LaLiga2021_22_SecondHalf_predict$WinLoseDraw[LaLiga2021_22_SecondHalf$WinLoseDraw == 2] == 3) #True 2's predicted as 3's
T3_P1 = sum(LaLiga2021_22_SecondHalf_predict$WinLoseDraw[LaLiga2021_22_SecondHalf$WinLoseDraw == 3] == 1) #True 3's predicted as 1's
T3_P2 = sum(LaLiga2021_22_SecondHalf_predict$WinLoseDraw[LaLiga2021_22_SecondHalf$WinLoseDraw == 3] == 2) #True 3's predicted as 2's
paste('True home wins predicted as home wins:', T1_P1, sep = ' ')
paste('True away wins predicted as away wins:', T2_P2, sep = ' ')
paste('True draws predicted as draws:', T3_P3, sep = ' ')
paste('True home wins predicted as away wins:', T1_P2, sep = ' ')
paste('True home wins predicted as draws:', T1_P3, sep = ' ')
paste('True away wins predicted as home wins:', T2_P1, sep = ' ')
paste('True away wins predicted as draws:', T2_P3, sep = ' ')
paste('True draws  predicted as home wins:', T3_P1, sep = ' ')
paste('True draws  predicted as away wins:', T3_P2, sep = ' ')

#Measures from the confusion matrix
LaLiga2021_22_SecondHalf$WinLoseDraw = factor(LaLiga2021_22_SecondHalf$WinLoseDraw)
summary(LaLiga2021_22_SecondHalf$WinLoseDraw)
##True positives: the number of predictions where a match of a class is well classified 
TPH = 69
TPH
TPA = 15
TPA
TPD = 11
TPD
##True negatives: the number of predictions where a match that does not belong to a class is classified as non-class
TNH = 49
TNH
TNA = 125
TNA
TND = 111
TND
##False positives: the number of matches that does not belong to a class and are incorrectly classified as class
FPH = 55
FPH
FPA = 11
FPA
FPD = 29
FPD
##False negatives: the number of matches that  belong to a class and are incorrectly classified as non-class
FNH = 17 
FNH
FNA = 39 
FNA
FND = 39
FND
##Accuracy: the ratio of the number of correct classifications to the total number of classifications
Accuracy = (T1_P1 + T2_P2 + T3_P3) / (T1_P1 + T1_P2 + T1_P3 + T2_P2 + T2_P1 + T3_P3 + T2_P3 + T3_P1 + T3_P2)
Accuracy
##Precision: the measure of the accuracy relative to the prediction of a specific class. It is calculated as the ratio of the True Positives of the class in question to the sum of its True Positives and False Positives.
PrecisionH = TPH / (TPH + FPH)
PrecisionH
PrecisionA = TPA / (TPA + FPA)
PrecisionA
PrecisionD = TPD / (TPD + FPD)
PrecisionD
##True positive rate: ratio of the true positives to the sum of the true positives and false negatives. It shows how often the classifier classifies a match of a class as that class.
TPRH = TPH / (TPH + FNH)
TPRH  
TPRA = TPA / (TPA + FNA)
TPRA
TPRD = TPD / (TPD + FND)
TPRD
##True negative rate: ratio of the true negatives to the sum of the true negatives and false positives. It shows how often the classifier classifies a non-class match as a non-class.
TNRH = TNH / (TNH + FPH)
TNRH  
TNRA = TNA / (TNA + FPA)
TNRA
TNRD = TND / (TND + FPD)
TNRD

#Function that calculates the probability of winning, losing or drawing of each team, given the home and away teams
ProbWinLoseDraw = function(HomeTeam, AwayTeam){
  lambda_home = AttackDefensiveRates$HomeGoalsScored[AttackDefensiveRates$Team == HomeTeam] * 
    AttackDefensiveRates$AwayGoalsConceded[AttackDefensiveRates$Team == AwayTeam]
  lambda_away = AttackDefensiveRates$AwayGoalsScored[AttackDefensiveRates$Team == AwayTeam] * 
    AttackDefensiveRates$HomeGoalsConceded[AttackDefensiveRates$Team == HomeTeam]
  ProbsMatrix = matrix(nrow = 11, ncol = 11)
  for (i in 0:10) {
    for (j in 0:10) {
      ProbsMatrix[i+1, j+1] = dtpois(i, lambda_home, b = 10) * dtpois(j, lambda_away, b = 10)
    }
  }
  ProbWinHome = sum(ProbsMatrix[lower.tri(ProbsMatrix)])
  ProbWinAway = sum(ProbsMatrix[upper.tri(ProbsMatrix)])
  ProbDraw = sum(diag(ProbsMatrix))
  return(c(ProbWinHome, ProbWinAway, ProbDraw))
}

#Run the previous function for all the remaining matches
ProbHomeWin = c()
ProbAwayWin = c()
ProbDraw = c()
for (i in 1:nrow(LaLiga2021_22_SecondHalf)) {
  ProbHomeWin = c(ProbHomeWin, ProbWinLoseDraw(LaLiga2021_22_SecondHalf$HomeTeam[i], 
                                               LaLiga2021_22_SecondHalf$AwayTeam[i])[1])
  ProbAwayWin = c(ProbAwayWin, ProbWinLoseDraw(LaLiga2021_22_SecondHalf$HomeTeam[i], 
                                               LaLiga2021_22_SecondHalf$AwayTeam[i])[2])
  ProbDraw = c(ProbDraw, ProbWinLoseDraw(LaLiga2021_22_SecondHalf$HomeTeam[i], 
                                         LaLiga2021_22_SecondHalf$AwayTeam[i])[3])
}
Probabilities = data.frame('Season' = LaLiga2021_22_SecondHalf$Season, 
                           'HomeTeam' = LaLiga2021_22_SecondHalf$HomeTeam, 
                           'AwayTeam' = LaLiga2021_22_SecondHalf$AwayTeam, 
                           'ProbHomeWins' = ProbHomeWin, 'ProbAwayWins' = ProbAwayWin, 
                           'ProbDraw' = ProbDraw, 
                           'Gameweek' = LaLiga2021_22_SecondHalf$GameWeek)
Probabilities$ProbHomeWins = round(Probabilities$ProbHomeWins, 3)
Probabilities$ProbAwayWins = round(Probabilities$ProbAwayWins, 3)
Probabilities$ProbDraw = round(Probabilities$ProbDraw, 3)

#We calculate how the league table would have end with the predicted results
##Function to calculate the points of each team
PointsCalculatorPredict = function(team){
  points = 0
  for (i in 1:nrow(LaLiga2021_22_SecondHalf_predict)) {
    if (LaLiga2021_22_SecondHalf_predict$HomeTeam[i] == team){
      if (LaLiga2021_22_SecondHalf_predict$HomeGoalsPredicted[i] > LaLiga2021_22_SecondHalf_predict$AwayGoalsPredicted[i]){ 
        points = points + 3
      }
      else if (LaLiga2021_22_SecondHalf_predict$HomeGoalsPredicted[i] == LaLiga2021_22_SecondHalf_predict$AwayGoalsPredicted[i]){
        points = points + 1
      }
    }
    if (LaLiga2021_22_SecondHalf_predict$AwayTeam[i] == team){
      if (LaLiga2021_22_SecondHalf_predict$HomeGoalsPredicted[i] < LaLiga2021_22_SecondHalf_predict$AwayGoalsPredicted[i]){ 
        points = points + 3
      }
      else if (LaLiga2021_22_SecondHalf_predict$HomeGoalsPredicted[i] == LaLiga2021_22_SecondHalf_predict$AwayGoalsPredicted[i]){
        points = points + 1
      }
    }
  }
  return(points)
}
##Create data frame with the classification
LaLiga2021_22_SecondHalf_predict$HomeTeam = factor(LaLiga2021_22_SecondHalf_predict$HomeTeam)
Team = c()
Points = c()
for (i in levels(LaLiga2021_22_SecondHalf_predict$HomeTeam)) {
  Team = c(Team, i)
  Points = c(Points, PointsCalculatorPredict(i))
}

Predicted_Classification_SecondRound = data.frame(Team, Points)
Predicted_Classification = data.frame('Team' = Predicted_Classification_SecondRound$Team, 
                                      'Points' = Predicted_Classification_SecondRound$Points + Classification_FirstRound_nonordered$Points)
Predicted_Classification =  Predicted_Classification[order(Predicted_Classification$Points, decreasing = TRUE), ]
Predicted_Classification$Position = 1:20
Predicted_Classification = Predicted_Classification[ , c(3,1,2)]

LaLiga2021_22_SecondHalf$HomeTeam = factor(LaLiga2021_22_SecondHalf$HomeTeam)
LaLiga2021_22_SecondHalf$AwayTeam = factor(LaLiga2021_22_SecondHalf$AwayTeam)
#Plot of the results of the second half from the season 2021/2022
ggplot(data = LaLiga2021_22_SecondHalf, aes(AwayTeam, HomeTeam, fill = HomeGoals - AwayGoals)) + geom_tile() + 
  geom_label(aes(label = paste(HomeGoals, AwayGoals, sep = "-")), fill = "white") + 
  scale_fill_gradient2(low = "red", high = "green", midpoint = 0, guide = FALSE) +
  scale_x_discrete(limits = levels(LaLiga2021_22_SecondHalf$AwayTeam), position = "top") + 
  scale_y_discrete(limits = rev(levels(LaLiga2021_22_SecondHalf$HomeTeam))) +
  theme_minimal() + 
  ggtitle ("Actual results of the second half of the season 2021/2022") +
  theme (plot.title = element_text(vjust=2, hjust = 0.5, face="bold", lineheight=1.5)) +
  labs(x = "Away Team", y = "Home Team") + 
  theme(axis.title.x = element_text(face="bold", vjust=1)) +
  theme(axis.title.y = element_text(face="bold", vjust=1.5))

#Plot of the predicted results of the second half from the season 2021/2022
ggplot(data = LaLiga2021_22_SecondHalf_predict, aes(AwayTeam, HomeTeam, fill = HomeGoalsPredicted - AwayGoalsPredicted)) + geom_tile() + 
  geom_label(aes(label = paste(HomeGoalsPredicted, AwayGoalsPredicted, sep = "-")), fill = "white") + 
  scale_fill_gradient2(low = "red", high = "green", midpoint = 0, guide = FALSE) +
  scale_x_discrete(limits = levels(LaLiga2021_22_SecondHalf_predict$AwayTeam), position = "top") + 
  scale_y_discrete(limits = rev(levels(LaLiga2021_22_SecondHalf_predict$HomeTeam))) +
  theme_minimal() + 
  ggtitle ("Predicted results of the second half of the season 2021/2022") +
  theme (plot.title = element_text(vjust=2, hjust = 0.5, face="bold", lineheight=1.5)) +
  labs(x = "Away Team", y = "Home Team") + 
  theme(axis.title.x = element_text(face="bold", vjust=1)) +
  theme(axis.title.y = element_text(face="bold", vjust=1.5))

#Number of matches that are exactly well predicted
HomeGoalsDifference = abs(LaLiga2021_22_SecondHalf$HomeGoals - LaLiga2021_22_SecondHalf_predict$HomeGoalsPredicted)
AwayGoalsDifference = abs(LaLiga2021_22_SecondHalf$AwayGoals - LaLiga2021_22_SecondHalf_predict$AwayGoalsPredicted)
sum(HomeGoalsDifference + AwayGoalsDifference == 0)

#Plot of the predicted probabilities
Probabilities$HomeTeam = factor(Probabilities$HomeTeam)
Probabilities$AwayTeam = factor(Probabilities$AwayTeam)
##Plot of the predicted probabilities for the home team winning
ggplot(data = Probabilities, aes(AwayTeam, HomeTeam, fill = ProbHomeWins)) + geom_tile() + 
  geom_label(aes(label = ProbHomeWins), fill = "white") + 
  scale_fill_gradient2(low = "red", high = "green", midpoint = 0.334, guide = FALSE) +
  scale_x_discrete(limits = levels(Probabilities$AwayTeam), position = "top") + 
  scale_y_discrete(limits = rev(levels(Probabilities$HomeTeam))) +
  theme_minimal() + 
  ggtitle ("Predicted probabilities for the home team winning") +
  theme (plot.title = element_text(vjust=2, hjust = 0.5, face="bold", lineheight=1.5)) +
  labs(x = "Away Team", y = "Home Team") + 
  theme(axis.title.x = element_text(face="bold", vjust=1)) +
  theme(axis.title.y = element_text(face="bold", vjust=1.5))
##Plot of the predicted probabilities for a draw
ggplot(data = Probabilities, aes(AwayTeam, HomeTeam, fill = ProbDraw)) + geom_tile() + 
  geom_label(aes(label = ProbDraw), fill = "white") + 
  scale_fill_gradient2(low = "red", high = "green", midpoint = 0.334, guide = FALSE) +
  scale_x_discrete(limits = levels(Probabilities$AwayTeam), position = "top") + 
  scale_y_discrete(limits = rev(levels(Probabilities$HomeTeam))) +
  theme_minimal() + 
  ggtitle ("Predicted probabilities of a draw") +
  theme (plot.title = element_text(vjust=2, hjust = 0.5, face="bold", lineheight=1.5)) +
  labs(x = "Away Team", y = "Home Team") + 
  theme(axis.title.x = element_text(face="bold", vjust=1)) +
  theme(axis.title.y = element_text(face="bold", vjust=1.5))
##Plot of the predicted probabilities for the away team winning
ggplot(data = Probabilities, aes(AwayTeam, HomeTeam, fill = ProbAwayWins)) + geom_tile() + 
  geom_label(aes(label = ProbAwayWins), fill = "white") + 
  scale_fill_gradient2(low = "red", high = "green", midpoint = 0.334, guide = FALSE) +
  scale_x_discrete(limits = levels(Probabilities$AwayTeam), position = "top") + 
  scale_y_discrete(limits = rev(levels(Probabilities$HomeTeam))) +
  theme_minimal() + 
  ggtitle ("Predicted probabilities for the away team winning") +
  theme (plot.title = element_text(vjust=2, hjust = 0.5, face="bold", lineheight=1.5)) +
  labs(x = "Away Team", y = "Home Team") + 
  theme(axis.title.x = element_text(face="bold", vjust=1)) +
  theme(axis.title.y = element_text(face="bold", vjust=1.5))

#Change the odds established by Bet365 to probabilities
LaLiga2021_22_BettingOdds_SecondRound = LaLiga2021_22_BettingOdds[191:380, ]
colnames(LaLiga2021_22_BettingOdds_SecondRound) = c("HomeTeam", 
                                                    "AwayTeam", 
                                                    "Bet365HomeOdds", 
                                                    "Bet365DrawOdds", 
                                                    "Bet365AwayOdds", 
                                                    "GameWeek")
LaLiga2021_22_BettingOdds_SecondRound$Bet365HomeProbs = 
  round(1/LaLiga2021_22_BettingOdds_SecondRound$Bet365HomeOdds, 3)
LaLiga2021_22_BettingOdds_SecondRound$Bet365DrawProb = 
  round(1/LaLiga2021_22_BettingOdds_SecondRound$Bet365DrawOdds, 3)
LaLiga2021_22_BettingOdds_SecondRound$Bet365AwayProb = 
  round(1/LaLiga2021_22_BettingOdds_SecondRound$Bet365AwayOdds, 3)

#Calculate the difference between the predicted probabilities and the ones set by Bet365
DifferenceProbsBet365 = data.frame('HomeTeam' = LaLiga2021_22_BettingOdds_SecondRound$HomeTeam, 
                                   'AwayTeam' = LaLiga2021_22_BettingOdds_SecondRound$AwayTeam, 
                                   'DifHomeProbs' = round(LaLiga2021_22_BettingOdds_SecondRound$Bet365HomeProbs - Probabilities$ProbHomeWins, 3), 
                                   'DifDrawProbs' =  round(LaLiga2021_22_BettingOdds_SecondRound$Bet365DrawProb - Probabilities$ProbDraw,3), 
                                   'DifAwayProbs' = round(LaLiga2021_22_BettingOdds_SecondRound$Bet365AwayProb - Probabilities$ProbAwayWins, 3), 
                                   'AbsDifHomeProbs' = round(abs(LaLiga2021_22_BettingOdds_SecondRound$Bet365HomeProbs - Probabilities$ProbHomeWins), 3), 
                                   'AbsDifDrawProbs' =  round(abs(LaLiga2021_22_BettingOdds_SecondRound$Bet365DrawProb - Probabilities$ProbDraw), 3), 
                                   'AbsDifAwayProbs' = round(abs(LaLiga2021_22_BettingOdds_SecondRound$Bet365AwayProb - Probabilities$ProbAwayWins), 3))

#Plot of the difference between the predicted probabilities and the ones established by Bet365
DifferenceProbsBet365$HomeTeam = factor(DifferenceProbsBet365$HomeTeam)
DifferenceProbsBet365$AwayTeam = factor(DifferenceProbsBet365$AwayTeam)
##Plot of the difference for home team winning
ggplot(data = DifferenceProbsBet365, aes(AwayTeam, HomeTeam, fill = DifHomeProbs)) + geom_tile() + 
  geom_label(aes(label = AbsDifHomeProbs), fill = "white") + 
  scale_fill_gradient2(low = "green", high = "red", midpoint = 0, guide = FALSE) +
  scale_x_discrete(limits = levels(DifferenceProbsBet365$AwayTeam), position = "top") + 
  scale_y_discrete(limits = rev(levels(DifferenceProbsBet365$HomeTeam))) +
  theme_minimal() + 
  ggtitle ("Difference between the predicted and the Bet365 probabilities for the home team winning") +
  theme (plot.title = element_text(vjust=2, hjust = 0.5, face="bold", lineheight=1.5)) +
  labs(x = "Away Team", y = "Home Team") + 
  theme(axis.title.x = element_text(face="bold", vjust=1)) +
  theme(axis.title.y = element_text(face="bold", vjust=1.5))
##Plot of the difference for a draw
ggplot(data = DifferenceProbsBet365, aes(AwayTeam, HomeTeam, fill = DifDrawProbs)) + geom_tile() + 
  geom_label(aes(label = AbsDifDrawProbs), fill = "white") + 
  scale_fill_gradient2(low = "green", high = "red", midpoint = 0, guide = FALSE) +
  scale_x_discrete(limits = levels(DifferenceProbsBet365$AwayTeam), position = "top") + 
  scale_y_discrete(limits = rev(levels(DifferenceProbsBet365$HomeTeam))) +
  theme_minimal() + 
  ggtitle ("Difference between the predicted and the Bet365 probabilities for a draw") +
  theme (plot.title = element_text(vjust=2, hjust = 0.5, face="bold", lineheight=1.5)) +
  labs(x = "Away Team", y = "Home Team") + 
  theme(axis.title.x = element_text(face="bold", vjust=1)) +
  theme(axis.title.y = element_text(face="bold", vjust=1.5))
##Plot of the difference for away team winning
ggplot(data = DifferenceProbsBet365, aes(AwayTeam, HomeTeam, fill = DifAwayProbs)) + geom_tile() + 
  geom_label(aes(label = AbsDifAwayProbs), fill = "white") + 
  scale_fill_gradient2(low = "green", high = "red", midpoint = 0, guide = FALSE) +
  scale_x_discrete(limits = levels(DifferenceProbsBet365$AwayTeam), position = "top") + 
  scale_y_discrete(limits = rev(levels(DifferenceProbsBet365$HomeTeam))) +
  theme_minimal() + 
  ggtitle ("Difference between the predicted and the Bet365 probabilities for the away team winning") +
  theme (plot.title = element_text(vjust=2, hjust = 0.5, face="bold", lineheight=1.5)) +
  labs(x = "Away Team", y = "Home Team") + 
  theme(axis.title.x = element_text(face="bold", vjust=1)) +
  theme(axis.title.y = element_text(face="bold", vjust=1.5))

#Mean difference of predicted minus bet365
mean(DifferenceProbsBet365$AbsDifHomeProbs) #Home
mean(DifferenceProbsBet365$AbsDifDrawProbs) #Draw
mean(DifferenceProbsBet365$AbsDifAwayProbs) #Away
###############################################################################
