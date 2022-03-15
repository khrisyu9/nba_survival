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