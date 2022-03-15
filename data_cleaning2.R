# load library
library(plyr)
library(rvest)
library(tidyverse)
library(readr)
library(ggplot2)
library (splines)
library(stringr)
library(factoextra)
library(reReg)
library(Rcpp)
library(pglm)
library(lmtest)
library(MASS)

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

nbastats3 <- nbastats2[c(28,2,3)]

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

##optim() algorithm


###point process to analyze the injury process
###zero-inflated poisson to model recover process (consecutivemissedgames - 1) 



###schedule data processing
schedule <- read.csv(file = "E:/Bayes_copula/Survival_analysis/2018-2019gamedata/schedule/schedule.csv", na.strings=c(""," ","NA"))
#filter by team
##ATL
ATL <- filter(schedule, VisitorABBR == "ATL" | HomeABBR == "ATL")

for (i in 1:nrow(ATL)){
  ATL$Tm <- "ATL"
  ATL$dateformat[i] <- substring(ATL$Date[i], 6)
  ATL$dateformat[i] <- gsub(",", "", ATL$dateformat[i])
  ATL$dateformat1[i] <- as.Date(ATL$dateformat[i], format="%b %d %Y")
  ATL$Tm[i] <- "ATL"
  ATL$gamegap[1] <- 100
  if (i >= 2){
    ATL$gamegap[i] <- ATL$dateformat1[i] - ATL$dateformat1[i-1]
  }
  if (ATL$VisitorABBR[i] == "ATL"){
    ATL$homegame[i] <- 0
  }
  if (ATL$HomeABBR[i] == "ATL"){
    ATL$homegame[i] <- 1
  }
  ATL$gamenumber[i] <- i
}

teamschedule <- ATL[c(11,14,15,16)]

##BOS
BOS <- filter(schedule, VisitorABBR == "BOS" | HomeABBR == "BOS")

for (i in 1:nrow(BOS)){
  BOS$Tm <- "BOS"
  BOS$dateformat[i] <- substring(BOS$Date[i], 6)
  BOS$dateformat[i] <- gsub(",", "", BOS$dateformat[i])
  BOS$dateformat1[i] <- as.Date(BOS$dateformat[i], format="%b %d %Y")
  BOS$Tm[i] <- "BOS"
  BOS$gamegap[1] <- 100
  if (i >= 2){
    BOS$gamegap[i] <- BOS$dateformat1[i] - BOS$dateformat1[i-1]
  }
  if (BOS$VisitorABBR[i] == "BOS"){
    BOS$homegame[i] <- 0
  }
  if (BOS$HomeABBR[i] == "BOS"){
    BOS$homegame[i] <- 1
  }
  BOS$gamenumber[i] <- i
}

teamschedule <- rbind(teamschedule, BOS[c(11,14,15,16)])

##BRK
BRK <- filter(schedule, VisitorABBR == "BRK" | HomeABBR == "BRK")

for (i in 1:nrow(BRK)){
  BRK$Tm <- "BRK"
  BRK$dateformat[i] <- substring(BRK$Date[i], 6)
  BRK$dateformat[i] <- gsub(",", "", BRK$dateformat[i])
  BRK$dateformat1[i] <- as.Date(BRK$dateformat[i], format="%b %d %Y")
  BRK$Tm[i] <- "BRK"
  BRK$gamegap[1] <- 100
  if (i >= 2){
    BRK$gamegap[i] <- BRK$dateformat1[i] - BRK$dateformat1[i-1]
  }
  if (BRK$VisitorABBR[i] == "BRK"){
    BRK$homegame[i] <- 0
  }
  if (BRK$HomeABBR[i] == "BRK"){
    BRK$homegame[i] <- 1
  }
  BRK$gamenumber[i] <- i
}

teamschedule <- rbind(teamschedule, BRK[c(11,14,15,16)])

##CHI
CHI <- filter(schedule, VisitorABBR == "CHI" | HomeABBR == "CHI")

for (i in 1:nrow(CHI)){
  CHI$Tm <- "CHI"
  CHI$dateformat[i] <- substring(CHI$Date[i], 6)
  CHI$dateformat[i] <- gsub(",", "", CHI$dateformat[i])
  CHI$dateformat1[i] <- as.Date(CHI$dateformat[i], format="%b %d %Y")
  CHI$Tm[i] <- "CHI"
  CHI$gamegap[1] <- 100
  if (i >= 2){
    CHI$gamegap[i] <- CHI$dateformat1[i] - CHI$dateformat1[i-1]
  }
  if (CHI$VisitorABBR[i] == "CHI"){
    CHI$homegame[i] <- 0
  }
  if (CHI$HomeABBR[i] == "CHI"){
    CHI$homegame[i] <- 1
  }
  CHI$gamenumber[i] <- i
}

teamschedule <- rbind(teamschedule, CHI[c(11,14,15,16)])

##CHO
CHO <- filter(schedule, VisitorABBR == "CHO" | HomeABBR == "CHO")

for (i in 1:nrow(CHO)){
  CHO$Tm <- "CHO"
  CHO$dateformat[i] <- substring(CHO$Date[i], 6)
  CHO$dateformat[i] <- gsub(",", "", CHO$dateformat[i])
  CHO$dateformat1[i] <- as.Date(CHO$dateformat[i], format="%b %d %Y")
  CHO$Tm[i] <- "CHO"
  CHO$gamegap[1] <- 100
  if (i >= 2){
    CHO$gamegap[i] <- CHO$dateformat1[i] - CHO$dateformat1[i-1]
  }
  if (CHO$VisitorABBR[i] == "CHO"){
    CHO$homegame[i] <- 0
  }
  if (CHO$HomeABBR[i] == "CHO"){
    CHO$homegame[i] <- 1
  }
  CHO$gamenumber[i] <- i
}

teamschedule <- rbind(teamschedule, CHO[c(11,14,15,16)])

##CLE
CLE <- filter(schedule, VisitorABBR == "CLE" | HomeABBR == "CLE")

for (i in 1:nrow(CLE)){
  CLE$Tm <- "CLE"
  CLE$dateformat[i] <- substring(CLE$Date[i], 6)
  CLE$dateformat[i] <- gsub(",", "", CLE$dateformat[i])
  CLE$dateformat1[i] <- as.Date(CLE$dateformat[i], format="%b %d %Y")
  CLE$Tm[i] <- "CLE"
  CLE$gamegap[1] <- 100
  if (i >= 2){
    CLE$gamegap[i] <- CLE$dateformat1[i] - CLE$dateformat1[i-1]
  }
  if (CLE$VisitorABBR[i] == "CLE"){
    CLE$homegame[i] <- 0
  }
  if (CLE$HomeABBR[i] == "CLE"){
    CLE$homegame[i] <- 1
  }
  CLE$gamenumber[i] <- i
}

teamschedule <- rbind(teamschedule, CLE[c(11,14,15,16)])

##DAL
DAL <- filter(schedule, VisitorABBR == "DAL" | HomeABBR == "DAL")

for (i in 1:nrow(DAL)){
  DAL$Tm <- "DAL"
  DAL$dateformat[i] <- substring(DAL$Date[i], 6)
  DAL$dateformat[i] <- gsub(",", "", DAL$dateformat[i])
  DAL$dateformat1[i] <- as.Date(DAL$dateformat[i], format="%b %d %Y")
  DAL$Tm[i] <- "DAL"
  DAL$gamegap[1] <- 100
  if (i >= 2){
    DAL$gamegap[i] <- DAL$dateformat1[i] - DAL$dateformat1[i-1]
  }
  if (DAL$VisitorABBR[i] == "DAL"){
    DAL$homegame[i] <- 0
  }
  if (DAL$HomeABBR[i] == "DAL"){
    DAL$homegame[i] <- 1
  }
  DAL$gamenumber[i] <- i
}

teamschedule <- rbind(teamschedule, DAL[c(11,14,15,16)])

##DEN
DEN <- filter(schedule, VisitorABBR == "DEN" | HomeABBR == "DEN")

for (i in 1:nrow(DEN)){
  DEN$Tm <- "DEN"
  DEN$dateformat[i] <- substring(DEN$Date[i], 6)
  DEN$dateformat[i] <- gsub(",", "", DEN$dateformat[i])
  DEN$dateformat1[i] <- as.Date(DEN$dateformat[i], format="%b %d %Y")
  DEN$Tm[i] <- "DEN"
  DEN$gamegap[1] <- 100
  if (i >= 2){
    DEN$gamegap[i] <- DEN$dateformat1[i] - DEN$dateformat1[i-1]
  }
  if (DEN$VisitorABBR[i] == "DEN"){
    DEN$homegame[i] <- 0
  }
  if (DEN$HomeABBR[i] == "DEN"){
    DEN$homegame[i] <- 1
  }
  DEN$gamenumber[i] <- i
}

teamschedule <- rbind(teamschedule, DEN[c(11,14,15,16)])

##DET
DET <- filter(schedule, VisitorABBR == "DET" | HomeABBR == "DET")

for (i in 1:nrow(DET)){
  DET$Tm <- "DET"
  DET$dateformat[i] <- substring(DET$Date[i], 6)
  DET$dateformat[i] <- gsub(",", "", DET$dateformat[i])
  DET$dateformat1[i] <- as.Date(DET$dateformat[i], format="%b %d %Y")
  DET$Tm[i] <- "DET"
  DET$gamegap[1] <- 100
  if (i >= 2){
    DET$gamegap[i] <- DET$dateformat1[i] - DET$dateformat1[i-1]
  }
  if (DET$VisitorABBR[i] == "DET"){
    DET$homegame[i] <- 0
  }
  if (DET$HomeABBR[i] == "DET"){
    DET$homegame[i] <- 1
  }
  DET$gamenumber[i] <- i
}

teamschedule <- rbind(teamschedule, DET[c(11,14,15,16)])

##GSW
GSW <- filter(schedule, VisitorABBR == "GSW" | HomeABBR == "GSW")

for (i in 1:nrow(GSW)){
  GSW$Tm <- "GSW"
  GSW$dateformat[i] <- substring(GSW$Date[i], 6)
  GSW$dateformat[i] <- gsub(",", "", GSW$dateformat[i])
  GSW$dateformat1[i] <- as.Date(GSW$dateformat[i], format="%b %d %Y")
  GSW$Tm[i] <- "GSW"
  GSW$gamegap[1] <- 100
  if (i >= 2){
    GSW$gamegap[i] <- GSW$dateformat1[i] - GSW$dateformat1[i-1]
  }
  if (GSW$VisitorABBR[i] == "GSW"){
    GSW$homegame[i] <- 0
  }
  if (GSW$HomeABBR[i] == "GSW"){
    GSW$homegame[i] <- 1
  }
  GSW$gamenumber[i] <- i
}

teamschedule <- rbind(teamschedule, GSW[c(11,14,15,16)])

##HOU
HOU <- filter(schedule, VisitorABBR == "HOU" | HomeABBR == "HOU")

for (i in 1:nrow(HOU)){
  HOU$Tm <- "HOU"
  HOU$dateformat[i] <- substring(HOU$Date[i], 6)
  HOU$dateformat[i] <- gsub(",", "", HOU$dateformat[i])
  HOU$dateformat1[i] <- as.Date(HOU$dateformat[i], format="%b %d %Y")
  HOU$Tm[i] <- "HOU"
  HOU$gamegap[1] <- 100
  if (i >= 2){
    HOU$gamegap[i] <- HOU$dateformat1[i] - HOU$dateformat1[i-1]
  }
  if (HOU$VisitorABBR[i] == "HOU"){
    HOU$homegame[i] <- 0
  }
  if (HOU$HomeABBR[i] == "HOU"){
    HOU$homegame[i] <- 1
  }
  HOU$gamenumber[i] <- i
}

teamschedule <- rbind(teamschedule, HOU[c(11,14,15,16)])

##IND
IND <- filter(schedule, VisitorABBR == "IND" | HomeABBR == "IND")

for (i in 1:nrow(IND)){
  IND$Tm <- "IND"
  IND$dateformat[i] <- substring(IND$Date[i], 6)
  IND$dateformat[i] <- gsub(",", "", IND$dateformat[i])
  IND$dateformat1[i] <- as.Date(IND$dateformat[i], format="%b %d %Y")
  IND$Tm[i] <- "IND"
  IND$gamegap[1] <- 100
  if (i >= 2){
    IND$gamegap[i] <- IND$dateformat1[i] - IND$dateformat1[i-1]
  }
  if (IND$VisitorABBR[i] == "IND"){
    IND$homegame[i] <- 0
  }
  if (IND$HomeABBR[i] == "IND"){
    IND$homegame[i] <- 1
  }
  IND$gamenumber[i] <- i
}

teamschedule <- rbind(teamschedule, IND[c(11,14,15,16)])

##LAC
LAC <- filter(schedule, VisitorABBR == "LAC" | HomeABBR == "LAC")

for (i in 1:nrow(LAC)){
  LAC$Tm <- "LAC"
  LAC$dateformat[i] <- substring(LAC$Date[i], 6)
  LAC$dateformat[i] <- gsub(",", "", LAC$dateformat[i])
  LAC$dateformat1[i] <- as.Date(LAC$dateformat[i], format="%b %d %Y")
  LAC$Tm[i] <- "LAC"
  LAC$gamegap[1] <- 100
  if (i >= 2){
    LAC$gamegap[i] <- LAC$dateformat1[i] - LAC$dateformat1[i-1]
  }
  if (LAC$VisitorABBR[i] == "LAC"){
    LAC$homegame[i] <- 0
  }
  if (LAC$HomeABBR[i] == "LAC"){
    LAC$homegame[i] <- 1
  }
  LAC$gamenumber[i] <- i
}

teamschedule <- rbind(teamschedule, LAC[c(11,14,15,16)])

##LAL
LAL <- filter(schedule, VisitorABBR == "LAL" | HomeABBR == "LAL")

for (i in 1:nrow(LAL)){
  LAL$Tm <- "LAL"
  LAL$dateformat[i] <- substring(LAL$Date[i], 6)
  LAL$dateformat[i] <- gsub(",", "", LAL$dateformat[i])
  LAL$dateformat1[i] <- as.Date(LAL$dateformat[i], format="%b %d %Y")
  LAL$Tm[i] <- "LAL"
  LAL$gamegap[1] <- 100
  if (i >= 2){
    LAL$gamegap[i] <- LAL$dateformat1[i] - LAL$dateformat1[i-1]
  }
  if (LAL$VisitorABBR[i] == "LAL"){
    LAL$homegame[i] <- 0
  }
  if (LAL$HomeABBR[i] == "LAL"){
    LAL$homegame[i] <- 1
  }
  LAL$gamenumber[i] <- i
}

teamschedule <- rbind(teamschedule, LAL[c(11,14,15,16)])

##MEM
MEM <- filter(schedule, VisitorABBR == "MEM" | HomeABBR == "MEM")

for (i in 1:nrow(MEM)){
  MEM$Tm <- "MEM"
  MEM$dateformat[i] <- substring(MEM$Date[i], 6)
  MEM$dateformat[i] <- gsub(",", "", MEM$dateformat[i])
  MEM$dateformat1[i] <- as.Date(MEM$dateformat[i], format="%b %d %Y")
  MEM$Tm[i] <- "MEM"
  MEM$gamegap[1] <- 100
  if (i >= 2){
    MEM$gamegap[i] <- MEM$dateformat1[i] - MEM$dateformat1[i-1]
  }
  if (MEM$VisitorABBR[i] == "MEM"){
    MEM$homegame[i] <- 0
  }
  if (MEM$HomeABBR[i] == "MEM"){
    MEM$homegame[i] <- 1
  }
  MEM$gamenumber[i] <- i
}

teamschedule <- rbind(teamschedule, MEM[c(11,14,15,16)])

##MIA
MIA <- filter(schedule, VisitorABBR == "MIA" | HomeABBR == "MIA")

for (i in 1:nrow(MIA)){
  MIA$Tm <- "MIA"
  MIA$dateformat[i] <- substring(MIA$Date[i], 6)
  MIA$dateformat[i] <- gsub(",", "", MIA$dateformat[i])
  MIA$dateformat1[i] <- as.Date(MIA$dateformat[i], format="%b %d %Y")
  MIA$Tm[i] <- "MIA"
  MIA$gamegap[1] <- 100
  if (i >= 2){
    MIA$gamegap[i] <- MIA$dateformat1[i] - MIA$dateformat1[i-1]
  }
  if (MIA$VisitorABBR[i] == "MIA"){
    MIA$homegame[i] <- 0
  }
  if (MIA$HomeABBR[i] == "MIA"){
    MIA$homegame[i] <- 1
  }
  MIA$gamenumber[i] <- i
}

teamschedule <- rbind(teamschedule, MIA[c(11,14,15,16)])

##MIL
MIL <- filter(schedule, VisitorABBR == "MIL" | HomeABBR == "MIL")

for (i in 1:nrow(MIL)){
  MIL$Tm <- "MIL"
  MIL$dateformat[i] <- substring(MIL$Date[i], 6)
  MIL$dateformat[i] <- gsub(",", "", MIL$dateformat[i])
  MIL$dateformat1[i] <- as.Date(MIL$dateformat[i], format="%b %d %Y")
  MIL$Tm[i] <- "MIL"
  MIL$gamegap[1] <- 100
  if (i >= 2){
    MIL$gamegap[i] <- MIL$dateformat1[i] - MIL$dateformat1[i-1]
  }
  if (MIL$VisitorABBR[i] == "MIL"){
    MIL$homegame[i] <- 0
  }
  if (MIL$HomeABBR[i] == "MIL"){
    MIL$homegame[i] <- 1
  }
  MIL$gamenumber[i] <- i
}

teamschedule <- rbind(teamschedule, MIL[c(11,14,15,16)])

##MIN
MIN <- filter(schedule, VisitorABBR == "MIN" | HomeABBR == "MIN")

for (i in 1:nrow(MIN)){
  MIN$Tm <- "MIN"
  MIN$dateformat[i] <- substring(MIN$Date[i], 6)
  MIN$dateformat[i] <- gsub(",", "", MIN$dateformat[i])
  MIN$dateformat1[i] <- as.Date(MIN$dateformat[i], format="%b %d %Y")
  MIN$Tm[i] <- "MIN"
  MIN$gamegap[1] <- 100
  if (i >= 2){
    MIN$gamegap[i] <- MIN$dateformat1[i] - MIN$dateformat1[i-1]
  }
  if (MIN$VisitorABBR[i] == "MIN"){
    MIN$homegame[i] <- 0
  }
  if (MIN$HomeABBR[i] == "MIN"){
    MIN$homegame[i] <- 1
  }
  MIN$gamenumber[i] <- i
}

teamschedule <- rbind(teamschedule, MIN[c(11,14,15,16)])

##NOP
NOP <- filter(schedule, VisitorABBR == "NOP" | HomeABBR == "NOP")

for (i in 1:nrow(NOP)){
  NOP$Tm <- "NOP"
  NOP$dateformat[i] <- substring(NOP$Date[i], 6)
  NOP$dateformat[i] <- gsub(",", "", NOP$dateformat[i])
  NOP$dateformat1[i] <- as.Date(NOP$dateformat[i], format="%b %d %Y")
  NOP$Tm[i] <- "NOP"
  NOP$gamegap[1] <- 100
  if (i >= 2){
    NOP$gamegap[i] <- NOP$dateformat1[i] - NOP$dateformat1[i-1]
  }
  if (NOP$VisitorABBR[i] == "NOP"){
    NOP$homegame[i] <- 0
  }
  if (NOP$HomeABBR[i] == "NOP"){
    NOP$homegame[i] <- 1
  }
  NOP$gamenumber[i] <- i
}

teamschedule <- rbind(teamschedule, NOP[c(11,14,15,16)])

##NYK
NYK <- filter(schedule, VisitorABBR == "NYK" | HomeABBR == "NYK")

for (i in 1:nrow(NYK)){
  NYK$Tm <- "NYK"
  NYK$dateformat[i] <- substring(NYK$Date[i], 6)
  NYK$dateformat[i] <- gsub(",", "", NYK$dateformat[i])
  NYK$dateformat1[i] <- as.Date(NYK$dateformat[i], format="%b %d %Y")
  NYK$Tm[i] <- "NYK"
  NYK$gamegap[1] <- 100
  if (i >= 2){
    NYK$gamegap[i] <- NYK$dateformat1[i] - NYK$dateformat1[i-1]
  }
  if (NYK$VisitorABBR[i] == "NYK"){
    NYK$homegame[i] <- 0
  }
  if (NYK$HomeABBR[i] == "NYK"){
    NYK$homegame[i] <- 1
  }
  NYK$gamenumber[i] <- i
}

teamschedule <- rbind(teamschedule, NYK[c(11,14,15,16)])

##OKC
OKC <- filter(schedule, VisitorABBR == "OKC" | HomeABBR == "OKC")

for (i in 1:nrow(OKC)){
  OKC$Tm <- "OKC"
  OKC$dateformat[i] <- substring(OKC$Date[i], 6)
  OKC$dateformat[i] <- gsub(",", "", OKC$dateformat[i])
  OKC$dateformat1[i] <- as.Date(OKC$dateformat[i], format="%b %d %Y")
  OKC$Tm[i] <- "OKC"
  OKC$gamegap[1] <- 100
  if (i >= 2){
    OKC$gamegap[i] <- OKC$dateformat1[i] - OKC$dateformat1[i-1]
  }
  if (OKC$VisitorABBR[i] == "OKC"){
    OKC$homegame[i] <- 0
  }
  if (OKC$HomeABBR[i] == "OKC"){
    OKC$homegame[i] <- 1
  }
  OKC$gamenumber[i] <- i
}

teamschedule <- rbind(teamschedule, OKC[c(11,14,15,16)])

##ORL
ORL <- filter(schedule, VisitorABBR == "ORL" | HomeABBR == "ORL")

for (i in 1:nrow(ORL)){
  ORL$Tm <- "ORL"
  ORL$dateformat[i] <- substring(ORL$Date[i], 6)
  ORL$dateformat[i] <- gsub(",", "", ORL$dateformat[i])
  ORL$dateformat1[i] <- as.Date(ORL$dateformat[i], format="%b %d %Y")
  ORL$Tm[i] <- "ORL"
  ORL$gamegap[1] <- 100
  if (i >= 2){
    ORL$gamegap[i] <- ORL$dateformat1[i] - ORL$dateformat1[i-1]
  }
  if (ORL$VisitorABBR[i] == "ORL"){
    ORL$homegame[i] <- 0
  }
  if (ORL$HomeABBR[i] == "ORL"){
    ORL$homegame[i] <- 1
  }
  ORL$gamenumber[i] <- i
}

teamschedule <- rbind(teamschedule, ORL[c(11,14,15,16)])

##PHI
PHI <- filter(schedule, VisitorABBR == "PHI" | HomeABBR == "PHI")

for (i in 1:nrow(PHI)){
  PHI$Tm <- "PHI"
  PHI$dateformat[i] <- substring(PHI$Date[i], 6)
  PHI$dateformat[i] <- gsub(",", "", PHI$dateformat[i])
  PHI$dateformat1[i] <- as.Date(PHI$dateformat[i], format="%b %d %Y")
  PHI$Tm[i] <- "PHI"
  PHI$gamegap[1] <- 100
  if (i >= 2){
    PHI$gamegap[i] <- PHI$dateformat1[i] - PHI$dateformat1[i-1]
  }
  if (PHI$VisitorABBR[i] == "PHI"){
    PHI$homegame[i] <- 0
  }
  if (PHI$HomeABBR[i] == "PHI"){
    PHI$homegame[i] <- 1
  }
  PHI$gamenumber[i] <- i
}

teamschedule <- rbind(teamschedule, PHI[c(11,14,15,16)])

##PHO
PHO <- filter(schedule, VisitorABBR == "PHO" | HomeABBR == "PHO")

for (i in 1:nrow(PHO)){
  PHO$Tm <- "PHO"
  PHO$dateformat[i] <- substring(PHO$Date[i], 6)
  PHO$dateformat[i] <- gsub(",", "", PHO$dateformat[i])
  PHO$dateformat1[i] <- as.Date(PHO$dateformat[i], format="%b %d %Y")
  PHO$Tm[i] <- "PHO"
  PHO$gamegap[1] <- 100
  if (i >= 2){
    PHO$gamegap[i] <- PHO$dateformat1[i] - PHO$dateformat1[i-1]
  }
  if (PHO$VisitorABBR[i] == "PHO"){
    PHO$homegame[i] <- 0
  }
  if (PHO$HomeABBR[i] == "PHO"){
    PHO$homegame[i] <- 1
  }
  PHO$gamenumber[i] <- i
}

teamschedule <- rbind(teamschedule, PHO[c(11,14,15,16)])

##POR
POR <- filter(schedule, VisitorABBR == "POR" | HomeABBR == "POR")

for (i in 1:nrow(POR)){
  POR$Tm <- "POR"
  POR$dateformat[i] <- substring(POR$Date[i], 6)
  POR$dateformat[i] <- gsub(",", "", POR$dateformat[i])
  POR$dateformat1[i] <- as.Date(POR$dateformat[i], format="%b %d %Y")
  POR$Tm[i] <- "POR"
  POR$gamegap[1] <- 100
  if (i >= 2){
    POR$gamegap[i] <- POR$dateformat1[i] - POR$dateformat1[i-1]
  }
  if (POR$VisitorABBR[i] == "POR"){
    POR$homegame[i] <- 0
  }
  if (POR$HomeABBR[i] == "POR"){
    POR$homegame[i] <- 1
  }
  POR$gamenumber[i] <- i
}

teamschedule <- rbind(teamschedule, POR[c(11,14,15,16)])

##SAC
SAC <- filter(schedule, VisitorABBR == "SAC" | HomeABBR == "SAC")

for (i in 1:nrow(SAC)){
  SAC$Tm <- "SAC"
  SAC$dateformat[i] <- substring(SAC$Date[i], 6)
  SAC$dateformat[i] <- gsub(",", "", SAC$dateformat[i])
  SAC$dateformat1[i] <- as.Date(SAC$dateformat[i], format="%b %d %Y")
  SAC$Tm[i] <- "SAC"
  SAC$gamegap[1] <- 100
  if (i >= 2){
    SAC$gamegap[i] <- SAC$dateformat1[i] - SAC$dateformat1[i-1]
  }
  if (SAC$VisitorABBR[i] == "SAC"){
    SAC$homegame[i] <- 0
  }
  if (SAC$HomeABBR[i] == "SAC"){
    SAC$homegame[i] <- 1
  }
  SAC$gamenumber[i] <- i
}

teamschedule <- rbind(teamschedule, SAC[c(11,14,15,16)])

##SAS
SAS <- filter(schedule, VisitorABBR == "SAS" | HomeABBR == "SAS")

for (i in 1:nrow(SAS)){
  SAS$Tm <- "SAS"
  SAS$dateformat[i] <- substring(SAS$Date[i], 6)
  SAS$dateformat[i] <- gsub(",", "", SAS$dateformat[i])
  SAS$dateformat1[i] <- as.Date(SAS$dateformat[i], format="%b %d %Y")
  SAS$Tm[i] <- "SAS"
  SAS$gamegap[1] <- 100
  if (i >= 2){
    SAS$gamegap[i] <- SAS$dateformat1[i] - SAS$dateformat1[i-1]
  }
  if (SAS$VisitorABBR[i] == "SAS"){
    SAS$homegame[i] <- 0
  }
  if (SAS$HomeABBR[i] == "SAS"){
    SAS$homegame[i] <- 1
  }
  SAS$gamenumber[i] <- i
}

teamschedule <- rbind(teamschedule, SAS[c(11,14,15,16)])

##TOR
TOR <- filter(schedule, VisitorABBR == "TOR" | HomeABBR == "TOR")

for (i in 1:nrow(TOR)){
  TOR$Tm <- "TOR"
  TOR$dateformat[i] <- substring(TOR$Date[i], 6)
  TOR$dateformat[i] <- gsub(",", "", TOR$dateformat[i])
  TOR$dateformat1[i] <- as.Date(TOR$dateformat[i], format="%b %d %Y")
  TOR$Tm[i] <- "TOR"
  TOR$gamegap[1] <- 100
  if (i >= 2){
    TOR$gamegap[i] <- TOR$dateformat1[i] - TOR$dateformat1[i-1]
  }
  if (TOR$VisitorABBR[i] == "TOR"){
    TOR$homegame[i] <- 0
  }
  if (TOR$HomeABBR[i] == "TOR"){
    TOR$homegame[i] <- 1
  }
  TOR$gamenumber[i] <- i
}

teamschedule <- rbind(teamschedule, TOR[c(11,14,15,16)])

##UTA
UTA <- filter(schedule, VisitorABBR == "UTA" | HomeABBR == "UTA")

for (i in 1:nrow(UTA)){
  UTA$Tm <- "UTA"
  UTA$dateformat[i] <- substring(UTA$Date[i], 6)
  UTA$dateformat[i] <- gsub(",", "", UTA$dateformat[i])
  UTA$dateformat1[i] <- as.Date(UTA$dateformat[i], format="%b %d %Y")
  UTA$Tm[i] <- "UTA"
  UTA$gamegap[1] <- 100
  if (i >= 2){
    UTA$gamegap[i] <- UTA$dateformat1[i] - UTA$dateformat1[i-1]
  }
  if (UTA$VisitorABBR[i] == "UTA"){
    UTA$homegame[i] <- 0
  }
  if (UTA$HomeABBR[i] == "UTA"){
    UTA$homegame[i] <- 1
  }
  UTA$gamenumber[i] <- i
}

teamschedule <- rbind(teamschedule, UTA[c(11,14,15,16)])

##WAS
WAS <- filter(schedule, VisitorABBR == "WAS" | HomeABBR == "WAS")

for (i in 1:nrow(WAS)){
  WAS$Tm <- "WAS"
  WAS$dateformat[i] <- substring(WAS$Date[i], 6)
  WAS$dateformat[i] <- gsub(",", "", WAS$dateformat[i])
  WAS$dateformat1[i] <- as.Date(WAS$dateformat[i], format="%b %d %Y")
  WAS$Tm[i] <- "WAS"
  WAS$gamegap[1] <- 100
  if (i >= 2){
    WAS$gamegap[i] <- WAS$dateformat1[i] - WAS$dateformat1[i-1]
  }
  if (WAS$VisitorABBR[i] == "WAS"){
    WAS$homegame[i] <- 0
  }
  if (WAS$HomeABBR[i] == "WAS"){
    WAS$homegame[i] <- 1
  }
  WAS$gamenumber[i] <- i
}

teamschedule <- rbind(teamschedule, WAS[c(11,14,15,16)])

#write.csv(teamschedule, file = "E:/Bayes_copula/data/2018-2019gamedata/processedteamschedule.csv")

databygame1 <- merge(databygame, teamschedule, by=c("Tm","gamenumber"), all.x = T)

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

#check one game rest interval 02/24/2024

sum(databygame$consecutivegamemissed > 2)
sum(databygame$consecutivegamemissed == 1)
#zero inflated poisson

#optim() function to estimate local minimum
#home/away indicator
#whether the second game of back-to-back games

function(){
  
}



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

timedata2$injuryindicater = 


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


