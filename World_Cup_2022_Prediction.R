#Tracy Wilson
#Team 17

library(dplyr)
library(randomForest)
library(caret)
library(ggplot2)

# Set working directory
setwd("~/Google Drive/Project")
#Increase Memory Limit
memory.limit(16000000)
#Use all cores
library(doParallel)
registerDoParallel(4)
setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
setSessionTimeLimit(cpu = Inf, elapsed = Inf)

# Load files
# world_cup_dataset <- read.csv("~/Google Drive/Project/fifaworldcup2018-master/World Cup 2018 Dataset.csv", stringsAsFactors = FALSE, header = TRUE)
# results <- read.csv("~/Google Drive/Project/fifaworldcup2018-master/results.csv", stringsAsFactors = FALSE, header = TRUE)
# team_rankings <- read.csv("~/Google Drive/Project/fifaworldcup2018-master/fifa_ranking.csv", stringsAsFactors = FALSE, header = TRUE)
# elo_ratings <- read.csv("~/Google Drive/Project/fifaworldcup2018-master/elo_ratings.csv", stringsAsFactors = FALSE, header = TRUE)
# gdp_data <- read.csv("~/Google Drive/Project/GDP_Country.csv", stringsAsFactors = FALSE, header = TRUE)
# gdp_per_capita <- read.csv("~/Google Drive/Project/GDP_per_Capita2.csv", stringsAsFactors = FALSE, header = TRUE)
# male_population <- read.csv("~/Google Drive/Project/male_population.csv", stringsAsFactors = FALSE, header = TRUE)
# WC_Appearances <- read.csv("~/Google Drive/Project/WC_Appearances.csv", stringsAsFactors = FALSE, header = TRUE)
#
# treeData <- read.csv("~/Google Drive/Project/Soccer_V3.csv", stringsAsFactors = FALSE, header = TRUE)
#
# select_string <- "select DISTINCT(p.Country) from  male_population p WHERE p.Country not in (Select Country from treeData) ORDER by Country"
# unmatchedCountry <- sqldf(select_string, stringsAsFactors = FALSE)
# #write.csv(unmatchedCountry, "unmatchedCountry.csv", row.names = FALSE)
# join_string <- "select DISTINCT(Country) from  treeData ORDER by Country"
# CountryList <- sqldf(join_string, stringsAsFactors = FALSE)
#write.csv(CountryList, "CountryList.csv", row.names = FALSE)

# Data Cleaning


# male_population <- searchAndReplace(male_population, "Bahamas, The", "Bermuda")
# male_population <- searchAndReplace(male_population, "Brunei Darussalam", "Delete")
# male_population <- searchAndReplace(male_population, "Cabo Verde", "Delete")
# male_population <- searchAndReplace(male_population, "Channel Islands", "Delete")
# male_population <- searchAndReplace(male_population, "Congo, Dem. Rep.", "Congo")
# male_population <- searchAndReplace(male_population, "Congo, Rep.", "Congo")
# male_population <- searchAndReplace(male_population, "Curacao", "Delete")
# male_population <- searchAndReplace(male_population, "Egypt, Arab Rep.", "Egypt")
# male_population <- searchAndReplace(male_population, "French Polynesia", "Delete")
# male_population <- searchAndReplace(male_population, "Gambia, The", "Gambia")
# male_population <- searchAndReplace(male_population, "Hong Kong SAR, China", "Delete")
# male_population <- searchAndReplace(male_population, "Iran, Islamic Rep.", "Iran")
# male_population <- searchAndReplace(male_population, "Korea, Dem. People?s Rep.", "North Korea")
# male_population <- searchAndReplace(male_population, "Korea, Rep.", "South Korea")
# male_population <- searchAndReplace(male_population, "Kyrgyz Republic", "Delete")
# male_population <- searchAndReplace(male_population, "Lao PDR", "Delete")
# male_population <- searchAndReplace(male_population, "Macao SAR, China", "Delete")
# male_population <- searchAndReplace(male_population, "Macedonia, FYR", "Macedonia")
# male_population <- searchAndReplace(male_population, "Micronesia, Fed. Sts.", "Delete")
# male_population <- searchAndReplace(male_population, "New Caledonia", "Delete")
# male_population <- searchAndReplace(male_population, "Sao Tome and Principe", "Delete")
# male_population <- searchAndReplace(male_population, "Slovak Republic", "Slovakia")
# male_population <- searchAndReplace(male_population, "St. Lucia", "Delete")
# male_population <- searchAndReplace(male_population, "Syrian Arab Republic", "Syria")
# male_population <- searchAndReplace(male_population, "Timor-Leste", "Delete")
# male_population <- searchAndReplace(male_population, "United Kingdom", "England")
# male_population <- searchAndReplace(male_population, "Venezuela, RB", "Venezuela")
# male_population <- searchAndReplace(male_population, "Virgin Islands (U.S.)", "Delete")
# male_population <- searchAndReplace(male_population, "Korea, Dem. People?s Rep.", "North Korea")
# male_population <- searchAndReplace(male_population, "    Virgin Islands (U.S.)", "Delete")

# select_string <- "select DISTINCT(p.Country) from  WC_Appearances p WHERE p.Country not in (Select Country from treeData) ORDER by Country"
# unmatchedCountry <- sqldf(select_string, stringsAsFactors = FALSE)
# write.csv(unmatchedCountry, "unmatchedCountry.csv", row.names = FALSE)
#
# gdp_per_capita <- searchAndReplace(gdp_per_capita, "Bahamas, The", "Delete")
# gdp_per_capita <- searchAndReplace(gdp_per_capita, "Brunei Darussalam", "Delete")
# gdp_per_capita <- searchAndReplace(gdp_per_capita, "Cabo Verde", "Delete")
# gdp_per_capita <- searchAndReplace(gdp_per_capita, "China, People's Republic of", "China")
# gdp_per_capita <- searchAndReplace(gdp_per_capita, "Congo of the", "Congo")
# gdp_per_capita <- searchAndReplace(gdp_per_capita, "ongoblic of ", "Congo")
# gdp_per_capita <- searchAndReplace(gdp_per_capita, "CCongo", "Congo")
# gdp_per_capita <- searchAndReplace(gdp_per_capita, "Curacao", "Delete")
# gdp_per_capita <- searchAndReplace(gdp_per_capita, "French Polynesia", "Delete")
# gdp_per_capita <- searchAndReplace(gdp_per_capita, "Gambia, The", "Gambia")
# gdp_per_capita <- searchAndReplace(gdp_per_capita, "Hong Kong SAR", "Delete")
# gdp_per_capita <- searchAndReplace(gdp_per_capita, "Ivory Coast", "Cote d'Ivoire")
# gdp_per_capita <- searchAndReplace(gdp_per_capita, "Korea, Dem. People?s Rep.", "North Korea")
# gdp_per_capita <- searchAndReplace(gdp_per_capita, "Korea, Rep.", "South Korea")
# gdp_per_capita <- searchAndReplace(gdp_per_capita, "Kyrgyz Republic", "Delete")
# gdp_per_capita <- searchAndReplace(gdp_per_capita, "Lao P.D.R.", "Delete")
# gdp_per_capita <- searchAndReplace(gdp_per_capita, "Macao SAR", "Delete")
# gdp_per_capita <- searchAndReplace(gdp_per_capita, "Marshall Islands", "Delete")
# gdp_per_capita <- searchAndReplace(gdp_per_capita, "FYR Macedonia", "Macedonia")
# gdp_per_capita <- searchAndReplace(gdp_per_capita, "Micronesia, Fed. States of", "Delete")
# gdp_per_capita <- searchAndReplace(gdp_per_capita, "Nauru", "Delete")
# gdp_per_capita <- searchAndReplace(gdp_per_capita, "Saint Kitts and Nevis", "Delete")
# gdp_per_capita <- searchAndReplace(gdp_per_capita, "Saint Lucia", "Delete")
# gdp_per_capita <- searchAndReplace(gdp_per_capita, "Saint Vincent and the Grenadines", "Delete")
# gdp_per_capita <- searchAndReplace(gdp_per_capita, "Slovak Republic", "Slovakia")
# gdp_per_capita <- searchAndReplace(gdp_per_capita, "St. Lucia", "Delete")
# gdp_per_capita <- searchAndReplace(gdp_per_capita, "South Sudan, Republic of", "South Sudan")
# gdp_per_capita <- searchAndReplace(gdp_per_capita, "Timor-Leste", "Delete")
# gdp_per_capita <- searchAndReplace(gdp_per_capita, "United Kingdom", "England")
# gdp_per_capita <- searchAndReplace(gdp_per_capita, "Venezuela, RB", "Venezuela")
# gdp_per_capita <- searchAndReplace(gdp_per_capita, "Virgin Islands (U.S.)", "Delete")
# gdp_per_capita <- searchAndReplace(gdp_per_capita, "Yemen", "Delete")
# gdp_per_capita <- searchAndReplace(gdp_per_capita, "Taiwan", "Delete")
# gdp_per_capita <- searchAndReplace(gdp_per_capita, "South Koreablic of", "South Korea")
# gdp_per_capita <- searchAndReplace(gdp_per_capita, "Congo, Dem. Rep. of the", "Congo")
# gdp_per_capita <- searchAndReplace(gdp_per_capita, "Congo, Republic of ", "Congo")
# gdp_per_capita <- searchAndReplace(gdp_per_capita, "Delete Province of China", "Delete")
#
# write.csv(gdp_per_capita, "gdp_per_capita2.csv", row.names = FALSE)

# # WC_Appearances <- searchAndReplace(WC_Appearances, "China PR", "China")
# # WC_Appearances <- searchAndReplace(WC_Appearances, "DR Congo", "Congo")
# # WC_Appearances <- searchAndReplace(WC_Appearances, "Ivory Coast", "Cote d'Ivoire")
# # WC_Appearances <- searchAndReplace(WC_Appearances, "North Korea", "North Korea")
# # WC_Appearances <- searchAndReplace(WC_Appearances, "Northern Ireland", "Delete")
# # WC_Appearances <- searchAndReplace(WC_Appearances, "Republic of Ireland", "Ireland")
# # WC_Appearances <- searchAndReplace(WC_Appearances, "Russia", "Russian Federation")
# # WC_Appearances <- searchAndReplace(WC_Appearances, "Scotland", "Delete")
# # WC_Appearances <- searchAndReplace(WC_Appearances, "Wales", "Delete")
#
# join_string <- "select t.* , p.Population from  male_population p left outer join treeData t ON p.Country = t.Country and p.Year = t.Year"
# wc_join_data <- sqldf(join_string, stringsAsFactors = FALSE)
# head(wc_join_data)
#
# join_string <- "select t.*, p.GDP_per_Capita from  gdp_per_capita p left outer join wc_join_data t ON p.Country = t.Country and p.Year = t.Year"
# wc_join_data <- sqldf(join_string, stringsAsFactors = FALSE)
# head(wc_join_data)
#
# join_string <- "select t.*, p.Strength from  WC_Appearances p left outer join wc_join_data t ON p.Country = t.Country"
# wc_join_data <- sqldf(join_string, stringsAsFactors = FALSE)
# head(wc_join_data)
# write.csv(wc_join_data, "~/Google Drive/Project/wc_join_data.csv", row.names = FALSE)
#


# library(sqldf)
# tracyQualRaw <- read.csv("~/Google Drive/Project/soccer_V2.csv", stringsAsFactors = FALSE, header = TRUE)
# meanPopChangeByCountry = sqldf("SELECT avg(X1yrPopROC) as avg1YrPop, avg(X4yrPopROC) as avg4YrPop,avg(X5yrPopROC) as avg5YrPop,avg(X8yrPopROC) as avg8YrPop,avg(X10yrPopROC) as avg10YrPop,
# avg(X1yrGDPROC) as avg1YrGDP, avg(X4yrGDPROC) as avg4YrGDP, avg(X5yrGDPROC) as avg5YrGDP, avg(X8yrGDPROC) as avg8YrGDP, avg(X10yrGDPROC) as avg10YrGDP,
# avg(X1yrGDPPerROC) as avg1YrGDPPer, avg(X4yrGDPPerROC) as avg4YrGDPPer, avg(X5yrGDPPerROC) as avg5YrGDPPer, avg(X8yrGDPPerROC) as avg8YrGDPPer, avg(X10yrGDPPerROC) as avg10YrGDPPer,
# avg(X1yrELOROC) as avg1YrELO, avg(X4yrELOROC) as avg4YrELO, avg(X5yrELOROC) as avg5YrELO, avg(X8yrELOROC) as avg8YrELO, avg(X10yrELOROC) as avg10YrELO,Country
#                       FROM tracyQualRaw
#                       GROUP BY Country")

#
# join_string <- "select p.Country , t.Country from  male_population p left outer join treeData t ON p.Country = t.Country"
# x <- sqldf(join_string, stringsAsFactors = FALSE)
#
# join_string <- "select t.Country, p.Country from  treeData t left outer join male_population p ON p.Country = t.Country"
# x <- sqldf(join_string, stringsAsFactors = FALSE)
# names(x)
# x1 <- sqldf("SELECT Country from x where 'Country..2' = ''", stringsAsFactors = FALSE)
#

# Meta data
# 1. World Cup 2018 Data set has playing nations and match fixtures
# 2. Team Rankings dataset has FIFA rankings of teams from 1993 to 2018
# 3. Results dataset has results of football matches of all football nations from 1930 onwards
# 4. Elo ratings of all football nations. Elo rating system, developed by Dr. Arpad Elo is an useful rating
# 5. World GDP data and projections from OECD
# 6. Population data is from World Bank
#    for a zero sum game such as football. Link: https://www.eloratings.net/about



# library(sqldf)
#
# WC <- sqldf("select year as Year, country_full as Country, elo_rating as 'Elo.Rating' from latest_team_rankings where elo_rating > 1500 group by year, country_full")
#
# names(WC)

#wcQualRaw <- read.csv("~/Google Drive/Project/Soccer_V2.csv", stringsAsFactors = FALSE, header = TRUE)
#wcQualTop8 <- sqldf("Select Rank, Place from wcQualRaw WHERE Place > 0")
#ggplot(wcQualTop8, aes(x = wcQualTop8$Rank, y = wcQualTop8$Place), ylim(1,8)) + geom_point()

WC_final <- read.csv("~/Google Drive/Project/final_data_set.csv", stringsAsFactors = FALSE, header = TRUE)
#write.csv(WC_Join, "~/Google Drive/Project/final_data_set.csv", row.names = FALSE)
WC_final$GDP <- as.numeric(WC_final$GDP)
summary(WC_final)
names(WC_final)


ggplot(WC_final, aes(x = Rank, y = Elo)) +
  geom_point(aes(color = Country)) +
  theme_bw() +
  theme(legend.position=c(.15, .25))

ggplot(WC_final, aes(x = Elo, y = Population)) +
  geom_point(aes(color = Country)) +
  theme_bw() +
  theme(legend.position=c(.2, .6))

ggplot(WC_final, aes(x = Elo, y = GDP)) +
  geom_point(aes(color = Country)) +
  theme_bw() +
  theme(legend.position=c(.2, .6))

ggplot(WC_final, aes(x = Elo, y = GDP_per_Capita)) +
  geom_point(aes(color = Country)) +
  theme_bw() +
  theme(legend.position=c(.2, .6))

str_WC <- sqldf("SELECT * FROM WC_final WHERE Year == 2018")
ggplot(str_WC, aes(x = Elo, y = Strength)) +
  geom_point(aes(color = Country)) +
  theme_bw() +
  theme(legend.position=c(.2, .6))



soccer= read.csv('~/Google Drive/Project/soccer_V2.csv',header = TRUE, na.strings = ".")
str(soccer)

#make spare dataset identical to first
soccer2 = read.csv('~/Google Drive/Project/soccer_V2.csv',header = TRUE, na.strings = ".")

#change binary variables to factors
soccer$WCYEAR = as.factor(soccer$WCYEAR)
soccer$WCQUALIFY = as.factor(soccer$WCQUALIFY)
soccer$WCPREVYEAR = as.factor(soccer$WCPREVYEAR)
soccer$WCPREVYEARQUAL = as.factor(soccer$WCPREVYEARQUAL)
soccer$FAVSPORT = as.factor(soccer$FAVSPORT)
soccer$GENPLAYERYN = as.factor(soccer$GENPLAYERYN)

#apply same changes to backup dataset
soccer2$WCYEAR = as.factor(soccer2$WCYEAR)
soccer2$WCQUALIFY = as.factor(soccer2$WCQUALIFY)
soccer2$WCPREVYEAR = as.factor(soccer2$WCPREVYEAR)
soccer2$WCPREVYEARQUAL = as.factor(soccer2$WCPREVYEARQUAL)
soccer2$FAVSPORT = as.factor(soccer2$FAVSPORT)
soccer2$GENPLAYERYN = as.factor(soccer2$GENPLAYERYN)

ggplot(soccer, aes(x = Rank, y = Elo)) +
  geom_point(aes(color = Country)) +
  theme_bw() +
  theme(legend.position = "Top")

ggplot(soccer, aes(x = Elo, y = Population)) +
  geom_point(aes(color = Country)) +
  theme_bw() +
  theme(legend.position=c(.2, .6))

ggplot(soccer, aes(x = Elo, y = GDP)) +
  geom_point(aes(color = Country)) +
  theme_bw() +
  theme(legend.position=c(.2, .6))


Smaller_Plot <- sqldf("SELECT GDPPER, Country, Elo from soccer where Rank < 30")
ggplot(Smaller_Plot, aes(x = Elo, y = GDPPER)) + geom_text(aes(label = Country, color = Country), hjust = 0, nudge_x = 0.05)
#  geom_point(aes(color = Country)) + geom_text(aes(label = Country), hjust = 0, nudge_x = 0.05)
#  theme_bw())
#  theme(legend.position=c(.2, .6))

str_WC <- sqldf("SELECT * FROM soccer WHERE Year == 2018")
ggplot(str_WC, aes(x = Elo, y = Strength)) +
  geom_point(aes(color = Country)) +
  theme_bw() +
  theme(legend.position=c(.2, .6))


library(MASS)

set.seed(1)
summary(WC_final)
names(WC_final)
colnames(WC_final)[11] <- "MalePop"
#WCLM <- sqldf("SELECT Elo, GDP, Population, MalePop, GDP_per_Capita, Strength FROM WC_final")
WCLM <- sqldf("SELECT Elo, GDP, Population, FAVSPORT, GENPLAYERNUM, GENPLAYERYN, MalePop, GDP_per_Capita, Strength FROM WC_final")
names(WCLM)

library(corrplot)
corrplot(cor(WCLM), order = "hclust")

n = nrow(WCLM)
train <- sample(1:n, n*.7, replace = FALSE)
WC.train = WCLM[train,]
WC.test = WCLM[-train,]

#write.csv(WC.train, "~/Google Drive/Project/WCtrain.csv", row.names = FALSE)
#write.csv(WC.test, "~/Google Drive/Project/WCtest.csv", row.names = FALSE)

model.train.all <- lm(WC.train$Elo ~ ., data = WC.train)
model.train.all.step <- stepAIC(model.train.all, trace = FALSE)
model.train.all.step$anova

#WC.train$Elo ~ Population + FAVSPORT + GENPLAYERNUM + GENPLAYERYN + MalePop + GDP_per_Capita + Strength

WC.pred.all <- predict(model.train.all, newdata = WC.test, type = "response")

model.train.opt <- lm(Elo ~ Population + FAVSPORT + GENPLAYERNUM + GENPLAYERYN + MalePop + GDP_per_Capita + Strength, data = WC.train)

WC.pred.opt <- predict(model.train.opt, newdata = WC.test, type = "response")

PredGRAll <- mean(WC.pred.all)
PredGROpt <- mean(WC.pred.opt)

plot(model.train.opt)


r2All <- summary(model.train.all)$r.squared #56.23291%
r2Opt <- summary(model.train.opt)$r.squared #56.22791%

RMSE(WC.pred.opt, WC.test$Elo, na.rm = TRUE) #113.076
R2(WC.pred.opt, WC.test$Elo, na.rm = TRUE) # 53.11364%


##########################
# PCA
##########################


pca.events <-  prcomp(WCLM[,-1], center = TRUE, scale. = TRUE)
summary(pca.events)

head(pca.events)

loadings <- as.data.frame(pca.events$rotation)

library(reshape2)
library(ggplot2)
head(loadings)
loadings

v <- rownames(loadings)

lds1 <- ggplot(data = loadings , aes(x = v, y = loadings$PC1  )) +
  geom_bar(stat = "identity")

lds1


lds2 <- ggplot(data = loadings , aes(x = v, y = loadings$PC2  )) +
  geom_bar(stat = "identity")

lds2

max(abs(pca.events$rotation[, 1]))
abs(pca.events$rotation[, 1])


max(abs(pca.events$rotation[, 2]))
abs(pca.events$rotation[, 2])

biplot(pca.events)

x1 <- ggplot(WCLM, aes(x = WCLM[,2], y = WCLM[,1])) + geom_point() + theme_grey() #GDP
x2 <- ggplot(WCLM, aes(x = WCLM[,3], y = WCLM[,1])) + geom_point() + theme_grey() #Population
x3 <- ggplot(WCLM, aes(x = WCLM[,8], y = WCLM[,1])) + geom_point() + theme_grey() #GSP per Capita
x4 <- ggplot(WCLM, aes(x = WCLM[,9], y = WCLM[,1])) + geom_point() + theme_grey() #Strength

plot_grid(x1  +
            theme(axis.text.x = element_text(angle = 70, vjust = 0.5)), x2 +
            theme(axis.text.x = element_text(angle = 70, vjust = 0.5)), x3 +
            theme(axis.text.x = element_text(angle = 70, vjust = 0.5)), x4 +
            theme(axis.text.x = element_text(angle = 70, vjust = 0.5)),
          labels = c('GDP', 'Population', 'GDP per Capita', 'Strength'))


X <- WCLM[,1]
Y <- pca.events$x[, 1]
qplot(X, Y, main="Scatterplot of PC1 vs. Elo",
      xlab="Elo", ylab="PC1")


library(caret)

trans <- preProcess(WCLM[,-1], method = c("BoxCox", "center", "scale"))
trans

transformed <- predict(trans, WCLM)

head(transformed)

##########################
# K Nearest Neighbor
##########################
cv.knn <-
  function(dataY, dataX, kn = 1:200, K = 10, seed = 1) {

    n <- nrow(dataX)
    set.seed(seed)
    library(class)

    #partition the data into K subsets
    f <- ceiling(n/K)
    s <- sample(rep(1L:K, f), n)
    #generate indices 1:10 and sample n of them
    # K fold cross-validated error

    cv = NULL; PvsO = NULL;
    dataX = scale(dataX)

    for (i in 1:K) {
      test.index <- seq_len(n)[(s == i)] #test data
      train.index <- seq_len(n)[(s != i)] #training data

      train.X <- dataX[train.index,]
      test.X <- dataX[test.index,]
      train.y <- dataY[train.index]
      test.y <- dataY[test.index]
      #predicted test set y
      knn.pred = knn(train.X, test.X, train.y, k = kn)
      #observed - predicted on test data
      error = mean(knn.pred != test.y)
      #error rates
      cv = c(cv,mean(error))
      predvsobs = data.frame(knn.pred,test.y)
      PvsO = rbind(PvsO,predvsobs)
    }

    #Output
    list(k = K, error = mean(cv), confusion = table(PvsO[,1],PvsO[,2]), seed = seed)
  }

cv.error = NULL

for (i in 1:200) {
  cv.error[i] <-  cv.knn(dataY = WC.train[,1], dataX = WC.train[,-1], kn = i, K = 10, seed = 1)$error
}

cv.error

k = which(cv.error == min(cv.error))
print(k)

err.knn.train <- cv.knn(dataY = WC.train[,1], dataX = WC.train[,-1], kn = 1, K = 10, seed = 1)$error
print(err.knn.train)

#What is the 10-fold cross-validated error rate for the knn for the best k on training data.  0.9926974

##########################
# LDA Linear Discriminant Analysis
##########################

cv.lda <- function(data, model, yname, K=nrow(data), seed = 1) {
  n <- nrow(data)
  set.seed(seed)
  datay = data[,yname] #response variable
  library(MASS)
  #partition the data into K subsets
  f <- ceiling(n/K)
  s <- sample(rep(1L:K, f), n)
  #generate indices 1:10 and sample n of them
  # K fold cross-validated error

  CV = NULL

  for (i in 1:K) { #i=1
    j.out <- seq_len(n)[(s == i)] #test data
    j.in <- seq_len(n)[(s != i)] #training data

    #model with training data
    lda.fit = lda(model, data = data[j.in,])
    #observed test set y
    lda.y <- datay[j.out]
    #predicted test set y
    lda.predy = predict(lda.fit, data[j.out,])$class

    #observed - predicted on test data
    error = mean(lda.y != lda.predy)
    #error rates
    CV = c(CV,mean(error))
  }

  #Output
  list(call = model, K = K, error = mean(CV), seed = seed)

}



err.lda <-  cv.lda(data = WC.train, model = Elo ~ Population + FAVSPORT + GENPLAYERNUM + GENPLAYERYN + MalePop + GDP_per_Capita + Strength,
                   yname = 'Elo', K = 10, seed = 25)

err.lda$error

err.lda.test <-  cv.lda(data = WC.test, model = Elo ~ Population + FAVSPORT + GENPLAYERNUM + GENPLAYERYN + MalePop + GDP_per_Capita + Strength,
                        yname = 'Elo', K = 10, seed = 25)

err.lda.test$error
##########################
# Random Forest Model
##########################

# Random Forest model with all features
randomForestModel <- randomForest(WC.train$Elo ~ ., WC.train, ntree = 30000, mtry = 5, nodesize = 0.01 * nrow(WC.train))
predict_WC <- predict(randomForestModel, WC.test)

pred_Elo <- predict_WC
trueElo <- WC.train$Elo
#Can't get this to work
#confusionMatrix(pred_Elo, trueElo)

# Let us use feature engineering to improve accuracy and balance the sensitivity and specificity values

# Feature Engineering
# Which are the important variables to predict match outcome?
varImpPlot(randomForestModel, main = "Features from Random Forest Model")

# Important features after feature engineering
selected_features <- "Elo ~ Population +  MalePop + GDP_per_Capita + Strength"

formula_1 <- as.formula(selected_features)

# Random Forest with selected features
randomForestModel_1 <- randomForest(formula_1, WC.train, ntree = 30000, mtry = 2, nodesize = 0.01 * nrow(train))
predict_WC <- predict(randomForestModel_1, WC.test)
confusionMatrix(WC.test$Elo, predict_WC)
summary(table(predict_WC))
summary(table(WC.test$Elo))
