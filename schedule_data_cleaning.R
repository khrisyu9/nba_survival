library(dplyr)

# read and filter players with enough game attendance and minutes
#read all .csv documents
setwd("E:/Bayes_copula/Survival_analysis/2018-2019gamedata/schedule")
oct <- read.csv(file = "oct_schedule.csv", na.strings=c(""," ","NA"))
nov <- read.csv(file = "nov_schedule.csv", na.strings=c(""," ","NA"))
dec <- read.csv(file = "dec_schedule.csv", na.strings=c(""," ","NA"))
jan <- read.csv(file = "jan_schedule.csv", na.strings=c(""," ","NA"))
feb <- read.csv(file = "feb_schedule.csv", na.strings=c(""," ","NA"))
mar <- read.csv(file = "mar_schedule.csv", na.strings=c(""," ","NA"))
apr <- read.csv(file = "apr_schedule.csv", na.strings=c(""," ","NA"))

schedule <- rbind(oct, nov)
schedule <- rbind(schedule, dec)
schedule <- rbind(schedule, jan)
schedule <- rbind(schedule, feb)
schedule <- rbind(schedule, mar)
schedule <- rbind(schedule, apr)

names(schedule)[1] <- 'Date'
names(schedule)[4] <- 'PTS.Visitor'
names(schedule)[6] <- 'PTS.Home'

schedule$VisitorABBR <- "NA"
schedule$HomeABBR <- "NA"

# write.csv(schedule, file = "schedule.csv")
schedule_new <- read.csv(file = "schedule.csv")
schedule_new$Date1 <- as.Date(schedule_new$Date, "%a, %b %d, %Y")

#calculate time difference by teams
#ATL
ATL_schedule <- schedule_new %>% filter(VisitorABBR == "ATL" | HomeABBR == "ATL")
ATL_schedule$timediff <- 0

for (i in 2:82){
  ATL_schedule$timediff[i] <- julian(ATL_schedule$Date1[i], ATL_schedule$Date1[i-1])
}

#BOS
BOS_schedule <- schedule_new %>% filter(VisitorABBR == "BOS" | HomeABBR == "BOS")
BOS_schedule$timediff <- 0

for (i in 2:82){
  BOS_schedule$timediff[i] <- julian(BOS_schedule$Date1[i], BOS_schedule$Date1[i-1])
}

