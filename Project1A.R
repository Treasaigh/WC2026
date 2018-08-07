if(!require(MASS)){install.packages("MASS")}
if(!require(corrplot)){install.packages("corrplot")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(ggfortify)){install.packages("ggfortify")}
if(!require(Hmisc)){install.packages("Hmisc")}
if(!require(e1071)){install.packages("e1071")}
if(!require(lattice)){install.packages("lattice")}
if(!require(caret)){install.packages("caret")}
if(!require(graphics)){install.packages("graphics")}
if(!require(AppliedPredictiveModeling)){install.packages("AppliedPredictiveModeling")}
if(!require(elasticnet)){install.packages("elasticnet")}
if(!require(forecast)){install.packages("forecast")}
if(!require(elo)){install.packages("elo")}

setwd("D:/Users/Tracy/Documents/Google Drive/Project")
eloData <-  read.csv(file = "ELO_1998_2018.csv", head = TRUE)

names(eloData)[1]<-"Year"
library(sqldf)
eloDataSm <- sqldf("SELECT Year, Team , Rating, Confederation   FROM eloData  WHERE Confederation <>'None'")

colnames(eloData)

head(eloData)
summary(eloData)

elo18 = subset(eloData, Year == "2018")
elo17 = subset(eloData, Year == "2017")
elo16 = subset(eloData, Year == "2016")
elo15 = subset(eloData, Year == "2015")

mean(eloData$Rating)
mean(elo18$Rating)
mean(elo17$Rating)
mean(elo16$Rating)
mean(elo15$Rating)

hist(eloData$Rating)
hist(elo18$Rating)
hist(elo17$Rating)
hist(elo16$Rating)
hist(elo15$Rating)

summary(eloData$Rating)
summary(elo18$Rating)
summary(elo17$Rating)
summary(elo16$Rating)
summary(elo15$Rating)

hist(eloData$Rating, breaks=25, main="Distribution of Rating, 1998 - 2018", xlab="Rating", ylab= NULL, col="darkorange2")
curve(dnorm(x, mean=mean(eloData$Rating), sd=sd(eloData$Rating)), add=TRUE, col="darkblue", lwd=2)

par(mfrow=c(2,2))
hist(eloData$Wins, breaks=25)
hist(eloData$Home , breaks=25)
hist(eloData$Goals.For , breaks=25)
hist(eloData$Goals.Against, breaks=25)
dev.off()

plot(eloData$Goals.For, eloData$Goals.Against)

colnames(eloData)
names(eloData)[6]<-"RankCh"
names(eloData)[7]<-"RatingCh"
names(eloData)[8]<-"AvgRank"
names(eloData)[9]<-"AvgRating"
names(eloData)[10]<-"YrChRank"
names(eloData)[11]<-"YrChRating"

eloDataCorr <- sqldf("SELECT Rank, Rating, RankCh, RatingCh, YrChRank, YrChRating, Total, Home, Away, Neutral, WIns FROM eloData")
responseY <- eloDataCorr[,1]
predictorsX <- eloDataCorr[,-1]


library(corrplot)
R <- cor(predictorsX) #saving the correlation matrix
corrplot(cor(R), order = "hclust")


library(ggplot2)

ggplot(eloDataSm, aes(x=Year, y=Rating, color=Confederation, fill = Confederation)) +
  geom_point(size=2, shape=23)

eloDataSm2 <- sqldf("SELECT Year, Team , Rating, Confederation   FROM eloData  WHERE Confederation ='CONMEBOL'")

ggplot(eloDataSm2, aes(x=Year, y=Rating, color=Team, fill=Team)) +
  geom_line(size=1, shape=21)

fit <- lm(Year ~ Rating, data=eloDataSm2)
summary(fit) # show results


fit <- lm(Year ~ Rating + Rank + Total + Home + Away + Wins + Losses + Goals.For + Goals.Against, data=eloData)
summary(fit) # show results



#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

library(dplyr)
matches <- read.csv(file = "WCQFGamesNoFriendlies.csv", head = TRUE, stringsAsFactors = FALSE)
colnames(matches)

matches1 <- matches

matches %>% select(Date, Home_Team, Away_Team, Home_Team_Goals, Away_Team_Goals) %>%
  mutate(Total_Goals = Home_Team_Goals + Away_Team_Goals) %>%
  arrange(-Total_Goals) %>%
  head(1)

teams <- data.frame(team = unique(c(matches$Home_Team, matches$Away_Team)))

teams <- teams %>%
  mutate(elo = 1500)

matches <- matches %>%
  mutate(result = if_else(Home_Team_Goals > Away_Team_Goals, 1,
                          if_else(Home_Team_Goals == Away_Team_Goals, 0.5, 0)))


matches <- matches %>%
  select(Date, Home_Team, Away_Team, result) %>%
  arrange(Date)

head(matches)

head(teams)

library(elo)
#We'll only be using one function from this package to create our rankings: elo.calc(). This function takes 4 arguments:
#wins.A: whether team A won or not. This is what we've created and stored in our result variable, with 3 possibles values (1, 0, 0.5);
#elo.A: the pre-match Elo value for team A;
#elo.B: the pre-match Elo value for team B;
#k: this is called the K-factor. This is basically how many Elo points are up for grabs in each match.

for (i in seq_len(nrow(matches))) {
  match <- matches[i, ]

  # Pre-match ratings
  teamA_elo <- subset(teams, team == match$Home_Team)$elo
  teamB_elo <- subset(teams, team == match$Away_Team)$elo

  # Let's update our ratings
  new_elo <- elo.calc(wins.A = match$result,
                      elo.A = teamA_elo,
                      elo.B = teamB_elo,
                      k = 30)

  # The results come back as a data.frame
  # with team A's new rating in row 1 / column 1
  # and team B's new rating in row 1 / column 2
  teamA_new_elo <- new_elo[1, 1]
  teamB_new_elo <- new_elo[1, 2]

  # We then update the ratings for teams A and B
  # and leave the other teams as they were
  teams <- teams %>%
    mutate(elo = if_else(team == match$Home_Team, teamA_new_elo,
                         if_else(team == match$Away_Team, teamB_new_elo, elo)))
}

Teams <- teams %>%
  arrange(-elo) %>%
  head


WC_teams <- teams %>%
  filter(team %in% c("Russia", "Germany", "Brazil", "Portugal", "Argentina", "Belgium",
                     "Poland", "France", "Spain", "Peru", "Switzerland", "England",
                     "Colombia", "Mexico", "Uruguay", "Croatia", "Denmark", "Iceland",
                     "Costa Rica", "Sweden", "Tunisia", "Egypt", "Senegal", "Iran",
                     "Serbia", "Nigeria", "Australia", "Japan", "Morocco", "Panama",
                     "South Korea", "Saudi Arabia")) %>%
  arrange(-elo)

print.data.frame(WC_teams)

teams %>%
  filter(elo > 1800, !team %in% WC_teams$team)


russia <- subset(WC_teams, team == "Russia")$elo
saudi_arabia <- subset(WC_teams, team == "Saudi Arabia")$elo
elo.prob(russia, saudi_arabia)

# A balanced match-up: France vs. Argentina
france <- subset(WC_teams, team == "France")$elo
argentina <- subset(WC_teams, team == "Argentina")$elo
elo.prob(france, argentina)

# A very un-balanced one: Brazil vs. Iceland
brazil <- subset(WC_teams, team == "Brazil")$elo
iceland <- subset(WC_teams, team == "Iceland")$elo
elo.prob(brazil, iceland)

#I won't show the details of this here because the code would be much longer, but essentially you can use the probability generated by elo.prob() to simulate the outcome of each match (using the sample() function and its prob argument to choose a random winner between Russia and Saudi Arabia, but with a 61% probability of choosing Russia), and update the Elo ratings throughout the competition.

#This way, you can simulate the entire competition all the way from the group stage to the final. And if you repeat this process many (thousands of) times, you will get detailed probabilities for each team to make it to the each stage of the competition. This is essentially what websites like FiveThirtyEight do for their sport predictions, with probabilities based on 100,000 simulations of the rest of the season.


make_tournament_dataset <- function(seed = NULL)
{
  set.seed(seed)

  all.teams <- c("Russia", "Germany", "Brazil", "Portugal", "Argentina", "Belgium",
                 "Poland", "France", "Spain", "Peru", "Switzerland", "England",
                 "Colombia", "Mexico", "Uruguay", "Croatia", "Denmark", "Iceland",
                 "Costa Rica", "Sweden", "Tunisia", "Egypt", "Senegal", "Iran",
                 "Serbia", "Nigeria", "Australia", "Japan", "Morocco", "Panama",
                 "South Korea", "Saudi Arabia")
  means <- c(18, 12, 17, 13, 16, 15.5, 14, 14.5)
  names(means) <- all.teams

  tournament <- expand.grid(team.Home = all.teams, team.Visitor = all.teams, stringsAsFactors = FALSE)
  tournament <- tournament[tournament$team.Home != tournament$team.Visitor, ]

  tournament$points.Home <- vapply(means[tournament$team.Home] + 3, stats::rpois, 0, n = 1)
  tournament$points.Visitor <- vapply(means[tournament$team.Visitor], stats::rpois, 0, n = 1)

  tournament$week <- 0
  tournament$week[1] <- 1
  for(i in 2:nrow(tournament))
  {
    t1 <- tournament$team.Home[i]
    t2 <- tournament$team.Visitor[i]
    idx <- 1:(i-1)
    wks <- tournament$week[idx]
    tm1 <- tournament$team.Home[idx]
    tm2 <- tournament$team.Visitor[idx]

    tournament$week[i] <- min(setdiff(1:14, wks[tm1 %in% c(t1, t2) | tm2 %in% c(t1, t2)]))
  }

  tournament$half <- ifelse(tournament$week < 8, "First Half of Season", "Second Half of Season")
  tournament <- tournament[order(tournament$week), ]
  rownames(tournament) <- NULL
  attr(tournament, "out.attrs") <- NULL

  tournament
}

WC_teams1 <- subset(matches1, Home_Team == c("Russia", "Germany", "Brazil", "Portugal", "Argentina", "Belgium",
                                             "Poland", "France", "Spain", "Peru", "Switzerland", "England",
                                             "Colombia", "Mexico", "Uruguay", "Croatia", "Denmark", "Iceland",
                                             "Costa Rica", "Sweden", "Tunisia", "Egypt", "Senegal", "Iran",
                                             "Serbia", "Nigeria", "Australia", "Japan", "Morocco", "Panama",
                                             "South Korea", "Saudi Arabia"))


as.matrix(elo.run(score(Home_Team_Goals , Away_Team_Goals) ~ Home_Team + Away_Team, data = WC_teams1, k = 20))

library(elo)
help(make_tournament_dataset)

