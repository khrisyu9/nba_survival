# load library
library(plyr)
library(data.table)
library(broom)
library(rvest)
library(tidyverse)
library(readr)
library(ggplot2)
library(splines)
library(stringr)
library(factoextra)
library(reReg)
library(Rcpp)
library(pglm)
library(lmtest)
library(MASS)
library(ggrepel)

# read and filter players with enough game attendance and minutes
#read all .csv documents
setwd("E:/Bayes_copula/data/2018-2019gamedata/csv")
temp = list.files(pattern="*.csv")

timeplayed <- seq(from=1, to=82, by=1)
gamedate <- seq(from=1, to=82, by=1)
playername <- "Player_Name"

for (i in 1:length(temp)) {
  df <- read.csv(temp[i], na.strings=c(""," ","NA"))
  names(df)[1] <- "Label"
  # first remember the column names
  coln <- df$Label
  # transpose all but the first column (name)
  df <- as.data.frame(t(df[,-1]))
  colnames(df) <- coln
  if (ncol(df) >= 82) {
    df_ <- df[,1:82]
    playername <- append(playername, temp[i])
    timeplayed <- rbind(timeplayed, df_[9,])
    gamedate <- rbind(gamedate, df_[2,])
  }
}

playername <- sub('.csv', '', playername)

timeplayed <- cbind(playername, timeplayed)
gamedate <- cbind(playername, gamedate)
#row.names(timeplayed) <- playername

timeplayed <- timeplayed[-1,]
gamedate <- gamedate[-1,]

nbastats <- read.csv(file = "E:/Bayes_copula/data/2018-2019gamedata/nbastats2019.csv", na.strings=c(""," ","NA"))

nbastats2 <- read.csv(file = "E:/Bayes_copula/data/nbastats2018-2019info.csv", na.strings=c(""," ","NA"))

nbastats$playername <- gsub(" ", "_", nbastats$Player)
nbastats2$playername <- gsub(" ", "_", nbastats2$Name)

nbastats3 <- nbastats2[c(29,2,3)]

combo <- merge(x = timeplayed, y = nbastats, by = "playername", all.x = TRUE)

combo1.5 <- merge(x = combo, y = nbastats3, by = "playername", all.x = TRUE)

#only consider 6 players with highest minutes each team
combo2 <- combo1.5 %>% filter(MP>25, na.rm = TRUE)

combo3 <- combo2[!duplicated(combo2$playername),]

covmatrix <- combo3[,c(1,113,114,85:90)]

combo4 <- combo3[,2:83]

combo5 <- combo4

combo5[is.na(combo5)] <- "0"

#write.csv(combo5, file = "E:/Bayes_copula/data/2018-2019gamedata/timestats2019.csv")

combo6 <- combo5

for (i in 1:ncol(combo6)){
  for (j in 1:nrow(combo6)){
    combo6[j,i] <- gsub("[: -]", "" , combo5[j,i], perl=TRUE)
  } 
}

combo7 <- data.frame(as.matrix(combo6))

for (i in 1:ncol(combo7)){
  for (j in 1:nrow(combo7)){
    if (floor(log10(as.numeric(combo7[j,i]))) + 1 == 3){
      combo7[j,i] <- as.numeric(substr(combo6[j,i], 1, 1))
      decimal <- round(2, 0.0167*as.numeric(substr(combo6[j,i], 2, 3)))
      combo7[j,i] <- as.numeric(combo7[j,i]) + decimal
    }
    else if (floor(log10(as.numeric(combo7[j,i]))) + 1 == 4){
      combo7[j,i] <- as.numeric(substr(combo6[j,i], 1, 2))
      decimal <- round(2, 0.0167*as.numeric(substr(combo6[j,i], 3, 4)))
      combo7[j,i] <- as.numeric(combo7[j,i]) + decimal
    }
    else if (floor(log10(as.numeric(combo7[j,i]))) + 1 == 6){
      combo7[j,i] <- as.numeric(substr(combo6[j,i], 1, 2))
      decimal <- round(2, 0.0167*as.numeric(substr(combo6[j,i], 3, 4)))
      combo7[j,i] <- as.numeric(combo7[j,i]) + decimal
    }
    else if (combo7[j,i] == 0){
      combo7[j,i] <- 0
    }
  } 
}

#write.csv(combo7, file = "E:/Bayes_copula/data/2018-2019gamedata/timeinteger.csv")

combo8 <- read.csv(file = "E:/Bayes_copula/data/2018-2019gamedata/timeinteger.csv")

decimal <- combo8

combo9 <- combo8

for (i in 1:ncol(combo8)){
  for (j in 1:nrow(combo8)){
    if (floor(log10(as.numeric(combo6[j,i]))) + 1 == 3){
      decimal[j,i] <- round(0.0167*as.numeric(substr(combo6[j,i], 2, 3)),2)
      combo9[j,i] <- combo8[j,i] + decimal[j,i]
    }
    else if (floor(log10(as.numeric(combo6[j,i]))) + 1 == 4){
      decimal[j,i] <- round(0.0167*as.numeric(substr(combo6[j,i], 3, 4)),2)
      combo9[j,i] <- combo8[j,i] + decimal[j,i]
    }
    else if (floor(log10(as.numeric(combo6[j,i]))) + 1 == 6){
      decimal[j,i] <- round(0.0167*as.numeric(substr(combo6[j,i], 3, 4)),2)
      combo9[j,i] <- combo8[j,i] + decimal[j,i]
    }
    else if (combo6[j,i] == 0){
      combo9[j,i] <- 0
    }
  } 
}


#transfer duration data to values

# time independent variables: age/experience/position, etc... 
# time dependent variables: total minutes till the game, average minutes till the game, game played, etc.


# group survival model

combo9 <- cbind(covmatrix, combo9)

#prepare the time matrix by game
playername <- "ABC"
gamenumber <- 0
minutesplayed <- 0
cumulativegameplayed <- 0
cumulativegamemissed <- 0
consecutivegameplayed <- 0
consecutivegamemissed <- 0

injurytime <- 0
cumulativeminutesplayed <- 0
consecutiveminutesplayed <- 0

onegamerest <- 0

timematrixbygame <- data.frame(playername, gamenumber, minutesplayed, cumulativegameplayed, cumulativegamemissed, consecutivegameplayed, consecutivegamemissed, injurytime, cumulativeminutesplayed, consecutiveminutesplayed, onegamerest)

for (i in 1:nrow(combo9)){
  gamenumber <- 0
  minutesplayed <- 0
  cumulativegameplayed <- 0
  cumulativegamemissed <- 0
  consecutivegameplayed <- 0
  consecutivegamemissed <- 0
  injurytime <- 0
  cumulativeminutesplayed <- 0
  consecutiveminutesplayed <- 0
  onegamerest <- 0
  playername <- combo9[i,1]
  for (j in 10:ncol(combo9)){
    gamenumber <- j-9
    minutesplayed <- combo9[i,j]
    cumulativeminutesplayed <- cumulativeminutesplayed + minutesplayed
    if (minutesplayed != 0){
      cumulativegameplayed <- cumulativegameplayed + 1
      consecutivegameplayed <- consecutivegameplayed + 1
      consecutivegamemissed <- 0
      consecutiveminutesplayed <- consecutiveminutesplayed + minutesplayed
    }
    if (minutesplayed == 0){
      cumulativegamemissed <- cumulativegamemissed + 1
      consecutivegamemissed <- consecutivegamemissed + 1
      consecutivegameplayed <- 0
      consecutiveminutesplayed <- 0
      if (consecutivegamemissed == 1){
        injurytime <- injurytime + 1
      }
      if (gamenumber >= 2 & gamenumber <= 81){
        if (combo9[i,j-1] != 0 & combo9[i,j+1] != 0){
          onegamerest <- onegamerest + 1 
        }
      }
      if (gamenumber == 82){
        if (combo9[i,j-1] != 0){
          onegamerest <- onegamerest + 1
        }
      }     
    }
    timematrixbygame <- rbind(timematrixbygame,c(playername, gamenumber, minutesplayed, cumulativegameplayed, cumulativegamemissed, consecutivegameplayed, consecutivegamemissed, injurytime, cumulativeminutesplayed, consecutiveminutesplayed, onegamerest))
  }
}

timematrixbygame <- timematrixbygame[-1,]

databygame <- merge(x = timematrixbygame, y = covmatrix, by = "playername", all.x = TRUE)

databygame$gamenumber <- as.numeric(databygame$gamenumber)
databygame$minutesplayed <- as.numeric(databygame$minutesplayed)
databygame$cumulativegameplayed <- as.numeric(databygame$cumulativegameplayed)
databygame$cumulativegamemissed <- as.numeric(databygame$cumulativegamemissed)
databygame$consecutivegameplayed <- as.numeric(databygame$consecutivegameplayed)
databygame$consecutivegamemissed <- as.numeric(databygame$consecutivegamemissed)
databygame$injurytime <- as.numeric(databygame$injurytime)
databygame$cumulativeminutesplayed <- as.numeric(databygame$cumulativeminutesplayed)
databygame$consecutiveminutesplayed <- as.numeric(databygame$consecutiveminutesplayed)
databygame$onegamerest <- as.numeric(databygame$onegamerest)
databygame$minutesplayed <- as.numeric(databygame$minutesplayed)
databygame$Height <- as.numeric(databygame$Height)
databygame$Weight <- as.numeric(databygame$Weight)
databygame$Age <- as.numeric(databygame$Age)
databygame$cumulativeMP <- as.numeric(databygame$cumulativeminutesplayed/databygame$cumulativegameplayed)
databygame$consecutiveMP <- as.numeric(databygame$consecutiveminutesplayed/databygame$consecutivegameplayed)
databygame$consecutiveMP[is.nan(databygame$consecutiveMP)] <- 0


##check last game for each player
lastgame <- filter(databygame, gamenumber == 82)
onegamerestper <- sum(lastgame$onegamerest)/sum(lastgame$injurytime)
mean(lastgame$injurytime)
onegamerestper
##53.61% missed intervals are one game rest among selected 145 players 

#combine team schedule data
##added pre/post all star weekend information 05282022
teamschedule <- read.csv(file = "E:/Bayes_copula/data/2018-2019gamedata/processedteamschedule.csv", na.strings=c(""," ","NA"))

databygame1 <- merge(databygame, teamschedule[2:6], by=c("Tm","gamenumber"), all.x = T)

databygame1 <- databygame1[order(databygame1$playername),]

summary(teamschedule$gamegap)

#drop those players played for multiple teams
databygame1 <- filter(databygame1, !is.na(gamegap))

for (i in 1:nrow(databygame1)){
  databygame1$secondbacktoback[i] <- 0
  if (databygame1$gamegap[i] == 1){
    databygame1$secondbacktoback[i] <- 1
  }
}


#write.csv(databygame1, file = "E:/Bayes_copula/data/playermatrixbygame_1819.csv")


##optim() algorithm variables proprocessing below
databygame2 <- databygame1

###covariates below
##minutesplayed
databygame2$minutesplayed <- scale(databygame2$minutesplayed)
x1_long <- databygame2[c(2,3,4)]
x1_wide <- spread(x1_long, playername, minutesplayed)
any(is.na(x1_wide))
x1 <- x1_wide[-1]
any(is.na(x1))

##onegamerest
databygame2$onegamerest <- scale(databygame2$onegamerest)
x2_long <- databygame2[c(2,3,12)]
x2_wide <- spread(x2_long, playername, onegamerest)
any(is.na(x2_wide))
x2 <- x2_wide[-1]
any(is.na(x2))

##Height
databygame2$Height <- scale(databygame2$Height)
x3_long <- databygame2[c(2,3,13)]
x3_wide <- spread(x3_long, playername, Height)
any(is.na(x3_wide))
x3 <- x3_wide[-1]
any(is.na(x3))

##Weight
databygame2$Weight <- scale(databygame2$Weight)
x4_long <- databygame2[c(2,3,14)]
x4_wide <- spread(x4_long, playername, Weight)
any(is.na(x4_wide))
x4 <- x4_wide[-1]
any(is.na(x4))

##Age
databygame2$Age <- scale(databygame2$Age)
x5_long <- databygame2[c(2,3,16)]
x5_wide <- spread(x5_long, playername, Age)
any(is.na(x5_wide))
x5 <- x5_wide[-1]
any(is.na(x5))

##consecutiveMP
databygame2$consecutiveMP <- scale(databygame2$consecutiveMP)
x6_long <- databygame2[c(2,3,21)]
x6_wide <- spread(x6_long, playername, consecutiveMP)
any(is.na(x6_wide))
x6 <- x6_wide[-1]
any(is.na(x6))

##gamegap
databygame2$gamegap <- scale(databygame2$gamegap)
x7_long <- databygame2[c(2,3,22)]
x7_wide <- spread(x7_long, playername, gamegap)
any(is.na(x7_wide))
x7 <- x7_wide[-1]
any(is.na(x7))

##homegame
databygame2$homegame <- scale(databygame2$gamegap)
x8_long <- databygame2[c(2,3,23)]
x8_wide <- spread(x8_long, playername, homegame)
any(is.na(x8_wide))
x8 <- x8_wide[-1]
any(is.na(x8))

##secondbacktoback
databygame2$secondbacktoback <- scale(databygame2$secondbacktoback)
x9_long <- databygame2[c(2,3,25)]
x9_wide <- spread(x9_long, playername, secondbacktoback)
any(is.na(x9_wide))
x9 <- x9_wide[-1]
any(is.na(x9))

##cumulativegameplayed
databygame2$cumulativegameplayed <- scale(databygame2$cumulativegameplayed)
x10_long <- databygame2[c(2,3,5)]
x10_wide <- spread(x10_long, playername, cumulativegameplayed)
any(is.na(x10_wide))
x10 <- x10_wide[-1]
any(is.na(x10))

##gamenumberspline1-5
x <- 1:82
knots <- c(30, 58)
theta = c(0.2, 0.4, 0.1, 0.9, 0.6)

#quadratic spline(degree=2) with two cut-points (three regions)
basis <- bs(x = x, knots = knots, degree = 2,
            Boundary.knots = c(1,82), intercept = TRUE)

y.spline <- basis %*% theta

dtbasis <- as.data.table(basis)
dtbasis[, x := seq(0, 1, length.out = .N)]

#plot selected basis
dtmelt <- melt(data = dtbasis, id = "x", 
               variable.name = "basis", variable.factor = TRUE)
ggplot(data=dtmelt, aes(x=x, y=value, group = basis)) +
  geom_line(aes(color=basis), size = 1) +
  theme(legend.position = "none") +
  scale_x_continuous(limits = c(0, 1), 
                     breaks = c(0, knots, 1)) +
  theme(panel.grid.minor = element_blank())

##basis1
databygame2$gamenumberspline1 <- rep(basis[,1], 140)
x11_long <- databygame2[c(2,3,26)]
x11_wide <- spread(x11_long, playername, gamenumberspline1)
any(is.na(x11_wide))
x11 <- x11_wide[-1]
any(is.na(x11))

##basis2
databygame2$gamenumberspline2 <- rep(basis[,2], 140)
x12_long <- databygame2[c(2,3,27)]
x12_wide <- spread(x12_long, playername, gamenumberspline2)
any(is.na(x12_wide))
x12 <- x12_wide[-1]
any(is.na(x11))

##basis3
databygame2$gamenumberspline3 <- rep(basis[,3], 140)
x13_long <- databygame2[c(2,3,28)]
x13_wide <- spread(x13_long, playername, gamenumberspline3)
any(is.na(x13_wide))
x13 <- x13_wide[-1]
any(is.na(x13))

##basis4
databygame2$gamenumberspline4 <- rep(basis[,4], 140)
x14_long <- databygame2[c(2,3,29)]
x14_wide <- spread(x14_long, playername, gamenumberspline4)
any(is.na(x14_wide))
x14 <- x14_wide[-1]
any(is.na(x14))

##basis5
databygame2$gamenumberspline5 <- rep(basis[,5], 140)
x15_long <- databygame2[c(2,3,30)]
x15_wide <- spread(x15_long, playername, gamenumberspline5)
any(is.na(x15_wide))
x15 <- x15_wide[-1]
any(is.na(x15))


##injurygame
y_long <- databygame1[c(2,3,8)]
y_wide <- spread(y_long, playername, consecutivegamemissed)

y_injury <- y_wide

for (j in 2:ncol(y_wide)){
  for (i in 1:nrow(y_wide)){
    if (i==1 & y_wide[i,j]==1){
      y_injury[i,j] <- 0
    }
    if (i>1 & y_wide[i,j]==1){
      y_injury[i,j] <- 0
      y_injury[i-1,j] <- 1
    } 
    if (y_wide[i,j]>1){
      y_injury[i,j] <- 0
    }
  }
}
any(is.na(y_injury))

y_list <- as.list(y_injury[2:ncol(y_injury)])

y_injury <- y_injury[-1]


##hazard game
z_long <- databygame1[c(2,3,8)]
z_wide <- spread(y_long, playername, consecutivegamemissed)

z_hazard <- z_wide

for (j in 2:ncol(z_wide)){
  for (i in 1:nrow(z_wide)){
    if (z_wide[i,j]==0){
      z_hazard[i,j] <- -1
    }
    if (z_wide[i,j]>=1){
      z_hazard[i,j] <- 0
    } 
  }
}
any(is.na(z_hazard))

z_hazard <- z_hazard[-1]
z_hazard <- -1*z_hazard

ncol(y_injury)

##likelihood function
likelihood <- function(beta){
  lambda_D <- matrix(0,82,ncol(y_injury))
  lambda <- matrix(0,82,ncol(y_injury))
  for (j in 1:ncol(y_injury)){
    for (i in 1:82){
      lambda_D[i,j] <- exp(beta[1]*x1[i,j]+beta[2]*x2[i,j]+beta[3]*x3[i,j]+beta[4]*x4[i,j]+beta[5]*x5[i,j]
                           +beta[6]*x6[i,j]+beta[7]*x7[i,j]+beta[8]*x8[i,j]+beta[9]*x9[i,j]+beta[10]*x10[i,j]
                           +beta[11]*x11[i,j]+beta[12]*x12[i,j]+beta[13]*x13[i,j]+beta[14]*x14[i,j]+beta[15]*x15[i,j])*z_hazard[i,j]
      lambda[i,j] <- exp(beta[1]*x1[i,j]+beta[2]*x2[i,j]+beta[3]*x3[i,j]+beta[4]*x4[i,j]+beta[5]*x5[i,j]
                         +beta[6]*x6[i,j]+beta[7]*x7[i,j]+beta[8]*x8[i,j]+beta[9]*x9[i,j]+beta[10]*x10[i,j]
                         +beta[11]*x11[i,j]+beta[12]*x12[i,j]+beta[13]*x13[i,j]+beta[14]*x14[i,j]+beta[15]*x15[i,j])*y_injury[i,j]
    }
  } 
  s_ll <- sum(lambda_D)
  ll_m <- sum(log(lambda[lambda>0]))
  return(ll_m - s_ll)
}

##optim function, with all starting beta = 1
opt_out <- optim(par = rep(1,15), fn = likelihood, method = "BFGS")
##converged betas
opt_out$par

## coefficient results:
#minutesplayed 5.6445485 
#onegamerest -1.1939356  
#height 5.6744417  
#weight 7.5955558  
#age 6.9580485  
#consecutiveMP 6.1093977 
#gamegap 35.0283946 
#homegame 35.0283946 
#secondbacktoback -0.6692039
#cumulativegameplayed -4.9490297 
#gamenumberspline -3.1569464

##EDA: only rest for one game proportion, etc.
#one game rest proportion:
playersummary <- filter(databygame1, gamenumber == 82)
playersummary$ogr_prop <- playersummary$onegamerest/playersummary$injurytime


ggplot(playersummary, aes(injurytime, ogr_prop, label = playername)) +    # ggplot2 with some labels
  geom_point(color = "dodgerblue", size = 2) +
  geom_text_repel(aes(label = ifelse(ogr_prop >= 0.6 & ogr_prop < 1 & injurytime >= 8, playername, "")), color = "orange")

#y_injury row sum
injury_sum <- rowSums(y_injury)
injury_df <- data.frame(cbind("game" = seq(1:82), injury_sum))
#plot the injury sum time series
ggplot(injury_df, aes(x=game, y=injury_sum)) +
  geom_line() + 
  geom_line(color="#fdb927")

#05172022 see if pre/post all star weekend injury (check)
## add back to y_injury to databygame2
y_injury_long <- y_injury %>% gather(playername, injured)
y_injury_long$gamenumber <- databygame2$gamenumber
databygame3 <- merge(databygame1, y_injury_long, by=c("playername", "gamenumber"), all.x = T)
databygame3$preasw_injured <- 0
databygame3$postasw_injured <- 0
for (i in 1:nrow(databygame3)){
  if (databygame3$injured[i] == 1){
    if (databygame3$postasw[i] == 1){
      databygame3$postasw_injured[i] <- 1
    }
    else {databygame3$preasw_injured[i] <- 1}
  }
}

total_preasw_injury <- sum(databygame3$preasw_injured)
total_postasw_injury <- sum(databygame3$postasw_injured)
pre_post_asw_injury_ratio <- total_preasw_injury/total_postasw_injury
total_postasw_games <- sum(databygame3$postasw)
total_preasw_games <- nrow(databygame3) - total_postasw_games
pre_post_asw_games_ratio <- total_preasw_games/total_postasw_games 

pre_post_asw_injury_ratio #1.489247
pre_post_asw_games_ratio #2.390431
#injury ratio is smaller than games ratio--it's more likely to have post all-star weekend injury?
#or just the end of regular season rest for major players

##ZIP data frame pre-processing
injury_period_df <- databygame1
injury_period_df$period <- 0

for (i in 1:nrow(databygame1)){
  if (databygame1$gamenumber[i]<82){
    if (databygame1$consecutivegamemissed[i] != 0 & databygame1$consecutivegamemissed[i+1] == 0){
      injury_period_df$period[i] <- 1
    }
  }
  if (databygame1$gamenumber[i]==82){
    if (databygame1$consecutivegamemissed[i] != 0){
      injury_period_df$period[i] <- 1
    }
  }
}

ip_df <- filter(injury_period_df, period == 1)

##inflated proportion?
sum(ip_df$consecutivegamemissed == 1)/nrow(ip_df)
#54.70% injury period are one game rest

ggplot(ip_df, aes(consecutivegamemissed)) + geom_histogram()

##ZIP coefficient
ip_df2 <- ip_df
##standardize variables
ip_df2$Height <- scale(ip_df2$Height)
ip_df2$Weight <- scale(ip_df2$Weight)
ip_df2$Age <- scale(ip_df2$Age)
ip_df2$gamenumber <- scale(ip_df2$gamenumber)
ip_df2$cumulativegameplayed <- scale(ip_df2$cumulativegameplayed)
ip_df2$cumulativeMP <- scale(ip_df2$cumulativeMP)
ip_df2$injurytime <- scale(ip_df2$injurytime)

library(pscl)
#ZIP model
m1 <- zeroinfl(consecutivegamemissed-1 ~ Height + Weight + Age + gamenumber + cumulativegameplayed + cumulativeMP + injurytime, 
                       data = ip_df2)
summary(m1)

#most coefficients not significant?

##05172022write down likelihood function of ZIP, check inflated pi value



##assume that all players share the same inflated pi
##model the period of games injured


### add variable: game number for that time (check)

### use spline to create basis (0-82 games) (3-4 knots, create covariates)
y_rowmean <- rowMeans(y_injury)


### zero-inflated poisson likelihood to model injury number of games (injury games -1 ~ zero-inflated poisson)
### write down the log-likelihood function


### nimble R package to implement (check) (play around tutorial)
### New England Statistics Symposium (check)







### multi-group log-Gaussian Cox Process

###point process to analyze the injury process
###zero-inflated poisson to model recover process (consecutivemissedgames - 1) 

#check one game rest interval (done) 02/24/2024

sum(databygame$consecutivegamemissed > 2)
sum(databygame$consecutivegamemissed == 1)
#zero inflated poisson

#optim() function to estimate local minimum
#home/away indicator (done)
#whether the second game of back-to-back games (done)

likelihood_fun <- function(X, Y, beta){
  lambda <- rep(0,length(X))
  for (i in 1:length(X)){
    lambda[i] <- exp(X[i])/Y[i]   
  }
  likelihood <- prod(lambda)/sum(exp(lambda))
  return(likelihood)
}


optim_output <- optim(par = c(0, 1),
                      fn = likelihood_fun,
                      X = databygame1[c(12:14, 16)],
                      Y = databygame1[6])


#log-likelihood function for poisson
log.lklh.poisson <- function(x, lambda){ 
  -sum(x * log(lambda) - log(factorial(x)) - lambda) 
}

Xt <- databygame1[c(12:14, 16)]

optim(par = rep(1,4), log.lklh.poisson, x = Xt)


res1 <- pglm(injurytime ~ cumulativegameplayed + cumulativeminutesplayed + consecutivegameplayed + consecutiveminutesplayed + Height + Weight + age, family = poisson, data = databygame, effect = "individual", model="within", index = "playername")
summary(res1)


#logistic regression

##output <- glm(injurytime ~ cumulativegameplayed + cumulativeminutesplayed + consecutivegameplayed + consecutiveminutesplayed, data = databygame,
##             family = poisson)
##print(summary(output))

#prepare the time matrix

playername <- "ABC"
tstart <- 0
tstop <- 0
status <- 0

event <- 0
ttlmp <- 0
avgmp <- 0


timematrix <- data.frame(playername, tstart, tstop, status, event, ttlmp, avgmp)

for (i in 1:nrow(combo9)){
  event <- 0
  tstart <- 0
  ttlmp <- 0
  ttlgp <- 0
  playername <- combo9[i,1]
  for (j in 8:ncol(combo9)){
    ttlmp <- ttlmp + combo9[i,j]
    ttlgp <- ttlgp + 1
    if (combo9[i,j]==0){
        tstop <- j-7
        status <- 1
        ttlgp <- ttlgp - 1
        avgmp <- ttlmp/ttlgp
        if (tstart != tstop){
          event <- event+1
          timematrix <- rbind(timematrix,c(playername, tstart, tstop, status, event, ttlmp, avgmp))             
        }
        ttlmp <- 0
        ttlgp <- 0
    }
    if (j==ncol(combo9) & combo9[i,j] != 0){
      event <- event+1
      tstop <- 82
      status <- 0
      avgmp <- ttlmp/ttlgp     
      timematrix <- rbind(timematrix,c(playername, tstart, tstop, status, event, ttlmp, avgmp))
    }
    tstart <- tstop+1
  }
}

timematrix1=timematrix

timematrix1$avgmp <- round(as.numeric(timematrix1$avgmp),2)

timematrix1[timematrix1 == "83"] <- "1"
timematrix1 <- timematrix1[-1,]

timematrix2 <- timematrix1

timematrix2$tstart <- as.numeric(timematrix2$tstart)

timematrix2$tstop <- as.numeric(timematrix2$tstop)
timematrix2$status <- as.numeric(timematrix2$status)
timematrix2$event <- as.numeric(timematrix2$event)
timematrix2$ttlmp <- as.numeric(timematrix2$ttlmp)

timedata <- merge(x = timematrix2, y = covmatrix, by = "playername", all.x = TRUE)

#recategorize player position
table(timedata$Pos)

timedata_c <- filter(timedata, Pos == "C")
timedata_pf <- filter(timedata, Pos == "PF")
timedata_sf <- filter(timedata, Pos == "SF")
timedata_sg <- filter(timedata, Pos == "SG")
timedata_pg <- filter(timedata, Pos == "PG")
####################################################
library(survival)
library(survminer)

#clustered position survival model

mean(timedata$status)

#nonparametric estimation function by position
#Poisson process regression
#recurrent event data regression
#Cook and Lawless
#proportional means and rates model
unique(timedata$playername)

timedata2 = timedata
timedata2$status2 = timedata2$status
timedata2$event2 = timedata2$event

for (i in 1:nrow(timedata2)){
  if (timedata2$tstop[i] == 82){
    timedata2$event2[i] = 0
    if (timedata2$status[i] == 1){
      timedata2$status2[i] = 1
    }
    else {
      timedata2$status2[i] = 0
      }
  }
  else if (timedata2$tstop[i] != 82){
    timedata2$status2[i] = 0
    timedata2$event2[i] = 1    
  }
}

timedata2$row_num = seq.int(nrow(timedata2))

library(data.table)
timedata3 = data.table(timedata2, key="playername")
timedata4 = timedata3[, .SD[which.max(tstop), ], by="playername"]

timedata2$event2[which(timedata2$row_num %in% timedata4$row_num)] = 0
timedata2$status2[which(timedata2$row_num %in% timedata4$row_num)] = 1

#11/23/2021
#don't consider terminal event 
#only treat players with 82 games as censored
#add covariates: heights, weights, playing time, game played last season, etc.
#12/17/2021
#add one more indicator: if games is the second back-to-back game
#and the event that only missing one game
#write down the model on paper 
#Analysis of Episodic Data with Application to Recurrent Pulmonary Exacerbations in
#Cystic Fibrosis Patients

for (i in 1:nrow(timedata2)){
  if (timedata2$tstop[i] == 82){
    timedata2$event2[i] = 0
    #if (timedata2$status[i] == 1){
    timedata2$status2[i] = 1
    #}
    #else {
    #  timedata2$status2[i] = 0
    #}
  }
}


timedata2 = na.omit(timedata2)

timedata2$avgmp2 = timedata2$avgmp/sd(timedata2$avgmp)
timedata2$Age2 = timedata2$Age/sd(timedata2$Age) 
timedata2$Height2 = timedata2$Height/sd(timedata2$Height) 
timedata2$Weight2 = timedata2$Weight/sd(timedata2$Weight)



#Model 1: Cox Regression
model1.cox = reReg(Recur(tstart %to% tstop, playername, event2, status2) ~ avgmp2 + Age2 + Height2 + Weight2, 
                B = 145, data = timedata2, model = "cox|cox")
summary(model1.cox)
plot(model1.cox)

# record home/away games win shares and basic statistics


##differences between positions
####C####
mean(timedata_c$status)

#nonparametric estimation function by position
#Poisson process regression
#recurrent event data regression
#Cook and Lawless
#proportional means and rates model
length(unique(timedata_c$playername))

timedata2 = timedata_c
timedata2$status2 = timedata2$status
timedata2$event2 = timedata2$event

for (i in 1:nrow(timedata2)){
  if (timedata2$tstop[i] == 82){
    timedata2$event2[i] = 0
    if (timedata2$status[i] == 1){
      timedata2$status2[i] = 1
    }
    else {
      timedata2$status2[i] = 0
    }
  }
  else if (timedata2$tstop[i] != 82){
    timedata2$status2[i] = 0
    timedata2$event2[i] = 1    
  }
}

timedata2$row_num = seq.int(nrow(timedata2))

library(data.table)
timedata3 = data.table(timedata2, key="playername")
timedata4 = timedata3[, .SD[which.max(tstop), ], by="playername"]

timedata2$event2[which(timedata2$row_num %in% timedata4$row_num)] = 0
timedata2$status2[which(timedata2$row_num %in% timedata4$row_num)] = 1

for (i in 1:nrow(timedata2)){
  if (timedata2$tstop[i] == 82){
    timedata2$event2[i] = 0
    #if (timedata2$status[i] == 1){
    timedata2$status2[i] = 1
    #}
    #else {
    #  timedata2$status2[i] = 0
    #}
  }
}


timedata2 = na.omit(timedata2)

timedata2$avgmp2 = timedata2$avgmp/sd(timedata2$avgmp)

timedata2$Age2 = timedata2$Age/sd(timedata2$Age) 

#Model 1: Cox Regression
model1.cox.c = reReg(Recur(tstart %to% tstop, playername, event2, status2) ~ avgmp2 + Age2, 
                   B = 23, data = timedata2, model = "cox|cox")
summary(model1.cox.c)
plot(model1.cox.c)

####PG####
mean(timedata_pg$status)

#nonparametric estimation function by position
#Poisson process regression
#recurrent event data regression
#Cook and Lawless
#proportional means and rates model
length(unique(timedata_pg$playername))

timedata2 = timedata_pg
timedata2$status2 = timedata2$status
timedata2$event2 = timedata2$event

for (i in 1:nrow(timedata2)){
  if (timedata2$tstop[i] == 82){
    timedata2$event2[i] = 0
    if (timedata2$status[i] == 1){
      timedata2$status2[i] = 1
    }
    else {
      timedata2$status2[i] = 0
    }
  }
  else if (timedata2$tstop[i] != 82){
    timedata2$status2[i] = 0
    timedata2$event2[i] = 1    
  }
}

timedata2$row_num = seq.int(nrow(timedata2))

library(data.table)
timedata3 = data.table(timedata2, key="playername")
timedata4 = timedata3[, .SD[which.max(tstop), ], by="playername"]

timedata2$event2[which(timedata2$row_num %in% timedata4$row_num)] = 0
timedata2$status2[which(timedata2$row_num %in% timedata4$row_num)] = 1

for (i in 1:nrow(timedata2)){
  if (timedata2$tstop[i] == 82){
    timedata2$event2[i] = 0
    #if (timedata2$status[i] == 1){
    timedata2$status2[i] = 1
    #}
    #else {
    #  timedata2$status2[i] = 0
    #}
  }
}


timedata2 = na.omit(timedata2)

timedata2$avgmp2 = timedata2$avgmp/sd(timedata2$avgmp)

timedata2$Age2 = timedata2$Age/sd(timedata2$Age) 

#Model 1: Cox Regression
model1.cox.pg = reReg(Recur(tstart %to% tstop, playername, event2, status2) ~ avgmp2 + Age2, 
                      B = 34, data = timedata2, model = "cox|cox")
summary(model1.cox.pg)
plot(model1.cox.pg)

####SF####
mean(timedata_sf$status)

#nonparametric estimation function by position
#Poisson process regression
#recurrent event data regression
#Cook and Lawless
#proportional means and rates model
length(unique(timedata_sf$playername))

timedata2 = timedata
timedata2$status2 = timedata2$status
timedata2$event2 = timedata2$event

for (i in 1:nrow(timedata2)){
  if (timedata2$tstop[i] == 82){
    timedata2$event2[i] = 0
    if (timedata2$status[i] == 1){
      timedata2$status2[i] = 1
    }
    else {
      timedata2$status2[i] = 0
    }
  }
  else if (timedata2$tstop[i] != 82){
    timedata2$status2[i] = 0
    timedata2$event2[i] = 1    
  }
}

timedata2$row_num = seq.int(nrow(timedata2))

library(data.table)
timedata3 = data.table(timedata2, key="playername")
timedata4 = timedata3[, .SD[which.max(tstop), ], by="playername"]

timedata2$event2[which(timedata2$row_num %in% timedata4$row_num)] = 0
timedata2$status2[which(timedata2$row_num %in% timedata4$row_num)] = 1

for (i in 1:nrow(timedata2)){
  if (timedata2$tstop[i] == 82){
    timedata2$event2[i] = 0
    #if (timedata2$status[i] == 1){
    timedata2$status2[i] = 1
    #}
    #else {
    #  timedata2$status2[i] = 0
    #}
  }
}


timedata2 = na.omit(timedata2)

timedata2$avgmp2 = timedata2$avgmp/sd(timedata2$avgmp)

timedata2$Age2 = timedata2$Age/sd(timedata2$Age) 

#Model 1: Cox Regression
model1.cox.sf = reReg(Recur(tstart %to% tstop, playername, event2, status2) ~ avgmp2 + Age2, 
                     B = 23, data = timedata2, model = "cox|cox")
summary(model1.cox.sf)
plot(model1.cox.sf)

#######################################################################################


#Model 2: Joint accelerated mean model
model2.am = reReg(Recur(tstart %to% tstop, playername, event2, status2) ~ avgmp2 + Age2, 
               B = 145, data = timedata2, model = "am|am")
summary(model2.am)
plot(model2.am)

#Model 3: Joint Cox/accelerated rate model
model3.CoxAr = reReg(Recur(tstart %to% tstop, playername, event2, status2) ~ ttlmp + avgmp + Age,
                  B = 145, data = timedata2, model = "cox|ar")
summary(model3.CoxAr)
plot(model3.CoxAr)

# terminal events
# non-time dependent covariate (age, weight, etc.)
# gap time model for recurrent events
# differences between positions

#********************test for reGeg regression below****************************
#Cox regression (baseline)
set.seed(11022021)
datCox = simGSC(200, summary = TRUE)
fit.cox = reReg(Recur(t.start %to% t.stop, id, event, status) ~ x1 + x2, 
                 B = 200, data = datCox, model = "cox|cox")
summary(fit.cox)
plot(fit.cox)

#Joint accelerated mean model of Xu
par0 = list(alpha = c(1, 1), beta = c(1, 1), eta = -c(1, 1), theta = -c(1, 1))
datam = simGSC(200, par = par0, summary = TRUE)
fit.am = reReg(Recur(t.start %to% t.stop, id, event, status) ~ x1 + x2, 
               B = 200, data = datam, model = "am|am")
summary(fit.am)
plot(fit.am)

#Joint Cox/accelerated rate model
par0 = list(eta = c(1, 1), theta = c(0, 0))
datCoxAr = simGSC(200, par = par0, summary = TRUE)
fit.CoxAr = reReg(Recur(t.start %to% t.stop, id, event, status) ~ x1 + x2,
                  B = 200, data = datCoxAr, model = "cox|ar")
summary(fit.CoxAr)
plot(fit.CoxAr)

#*****************************************************************

#Andersen-Gill (AG) Marginal means and rates model:
model.1 = coxph(Surv(tstart,tstop,status) ~ Age + cluster(playername), method="breslow", data = timedata)
summary(model.1)

model.1_c = coxph(Surv(tstart,tstop,status) ~ Age + MP + cluster(playername), method="breslow", data = timedata_c)
summary(model.1_c)

model.1_pf = coxph(Surv(tstart,tstop,status) ~ Age + MP + cluster(playername), method="breslow", data = timedata_pf)
summary(model.1_pf)

model.1_sf = coxph(Surv(tstart,tstop,status) ~ Age + MP + cluster(playername), method="breslow", data = timedata_sf)
summary(model.1_sf)

model.1_sg = coxph(Surv(tstart,tstop,status) ~ Age + MP + cluster(playername), method="breslow", data = timedata_sg)
summary(model.1_sg)

model.1_pg = coxph(Surv(tstart,tstop,status) ~ Age + MP + cluster(playername), method="breslow", data = timedata_pg)
summary(model.1_pg)


#PWP-TT model:
model.2 = coxph(Surv(tstart,tstop,status) ~ Age + MP + Pos + cluster(playername) + strata(event), method="breslow", data = timedata)
summary(model.2)

model.2_c = coxph(Surv(tstart,tstop,status) ~ Age + MP + cluster(playername) + strata(event), method="breslow", data = timedata_c)
summary(model.2_c)

model.2_pf = coxph(Surv(tstart,tstop,status) ~ Age + MP + cluster(playername) + strata(event), method="breslow", data = timedata_pf)
summary(model.2_pf)

model.2_sf = coxph(Surv(tstart,tstop,status) ~ Age + MP + cluster(playername) + strata(event), method="breslow", data = timedata_sf)
summary(model.2_sf)

model.2_sg = coxph(Surv(tstart,tstop,status) ~ Age + MP + cluster(playername) + strata(event), method="breslow", data = timedata_sg)
summary(model.2_sg)

model.2_pg = coxph(Surv(tstart,tstop,status) ~ Age + MP + cluster(playername) + strata(event), method="breslow", data = timedata_pg)
summary(model.2_pg)

#Frailty models:
model.frailty=coxph(Surv(tstop, status) ~ Age + MP + Pos + frailty(playername), data=timedata)
summary(model.frailty)

model.frailty_c=coxph(Surv(tstop, status) ~ Age + MP + frailty(playername), data=timedata_c)
summary(model.frailty_c)

model.frailty_pf=coxph(Surv(tstop, status) ~ Age + MP + frailty(playername), data=timedata_pf)
summary(model.frailty_pf)

model.frailty_sf=coxph(Surv(tstop, status) ~ Age + MP + frailty(playername), data=timedata_sf)
summary(model.frailty_sf)

model.frailty_sg=coxph(Surv(tstop, status) ~ Age + MP + frailty(playername), data=timedata_sg)
summary(model.frailty_sg)

model.frailty_pg=coxph(Surv(tstop, status) ~ Age + MP + frailty(playername), data=timedata_pg)
summary(model.frailty_pg)


