gdp <- read.csv("C:/Users/tlwil/Google Drive/Project/gdp_gen.csv")

gdp_gen_ctry <- read.csv("C:/Users/tlwil/Google Drive/Project/gdp_gen_ctry.csv")


plot(gdp_gen_ctry[1:2,], col="grey",
     main="GDP Argentina",
     xlab="", ylab="GDP")


lines(seasadj(fit),col="red",ylab="Adjusted")

library(MASS) ## for the qda() function > 
qdaModel <- qda(Country ~ ., data = gdp_gen_ctry)



matches <- read.csv("C:/Users/tlwil/Google Drive/Project/WCQF Games.csv")
summary(matches)
str(matches)

require(ggplot2)
require(sandwich)
require(msm)
require(sqldf)

str(matches)

p <- subset(matches, Tournament != 'Friendly', select = c("Home.Team","Away.Team","Home.Team.Goals", "Away.Team.Goals"))

summary(p)

devtools::install_github("dashee87/footballR") you may need to install footballR
library(footballR)
library(dplyr)
#you'll have to wait to find out the purpose of this mysterious package
library(skellam)
library(ggplot2)
library(purrr)
library(tidyr)
# abettor is an R wrapper for the Betfair API, 
# which we'll use to obtain betting odds
#devtools::install_github("phillc73/abettor")
library(abettor)
library(RCurl)

options(stringsAsFactors = FALSE)

# get id for 2016/17 EPL season
epl_id <-fdo_listComps(season = 2016,response = "minified") %>% filter(league=="PL") %>% .$id
# get all matches in 2016/17 EPL season
epl_data <- fdo_listCompFixtures(id = epl_id, response = "minified")$fixtures %>%
    jsonlite::flatten() %>% filter(status=="FINISHED") %>%
    rename(home=homeTeamName, away=awayTeamName, homeGoals=result.goalsHomeTeam,
           awayGoals=result.goalsAwayTeam) %>%
    select(home,away,homeGoals,awayGoals) %>%
    # some formatting of team names so that the names returned by footballR are
    # compatible with those returned by the Betfair API
    mutate(home=gsub(" FC| AFC|AFC |wich Albion|rystal| Hotspur","",home)) %>% 
    mutate(home=ifelse(home=="Manchester United","Man Utd",
                       ifelse(home=="Manchester City","Man City",
                              gsub(" City| United","",home)))) %>%
    mutate(away=gsub(" FC| AFC|AFC |wich Albion|rystal| Hotspur","",away)) %>% 
    mutate(away=ifelse(away=="Manchester United","Man Utd",
                       ifelse(away=="Manchester City","Man City",
                              gsub(" City| United","",away))))
head(epl_data)

