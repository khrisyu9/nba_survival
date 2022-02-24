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
timeplayed <- 0
cumulativegameplayed <- 0
cumulativegamemissed <- 0
consecutivegameplayed <- 0
consecutivegamemissed <- 0
injurytime <- 0
cumulativeminutesplayed <- 0
consecutiveminutesplayed <- 0

timematrixbygame <- data.frame(playername, gamenumber, minutesplayed, cumulativegameplayed, cumulativegamemissed, consecutivegameplayed, consecutivegamemissed, injurytime, cumulativeminutesplayed, consecutiveminutesplayed)

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
    }
    timematrixbygame <- rbind(timematrixbygame,c(playername, gamenumber, minutesplayed, cumulativegameplayed, cumulativegamemissed, consecutivegameplayed, consecutivegamemissed, injurytime, cumulativeminutesplayed, consecutiveminutesplayed))
  }
}


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


