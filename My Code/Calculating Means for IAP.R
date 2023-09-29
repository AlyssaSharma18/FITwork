library(dplyr)
library(tidyverse)

IAPavg <- read.csv("IAPData.csv")
#View(IAPavg)

length(IAPavg$Location)

idcol2 <- character(106)
for (i in 1:106) {
  idcol2[i] <- paste(IAPavg$Location[i],IAPavg$Mat[i],IAPavg$Season[i])
}
idcol2

IAPavg <- cbind(IAPavg,idcol2)
View(IAPavg)

IAPavg_calc <- IAPavg %>%
  group_by(idcol2) %>%
  summarise(season = Season, meanbarnacle = mean(Barnacles), meanencrust = mean(Enc..Bryo.), meanoyster = mean(Oysters))

View(unique(IAPavg_calc))

unique(IAPavg_calc) %>%
  group_by(season) %>%
  summarise(finalmeanbarnacle = mean(meanbarnacle), finalmeanencrusting = mean(meanencrust), finalmeanoyster = mean(meanoyster))

#Now we will do the same thing for A1A
A1Aavg <- read.csv("A1A Means Data.csv")
View(A1Aavg)

length(A1Aavg$Dock)

idcol3 <- character(102)
for (i in 1:102) {
  idcol3[i] <- paste(A1Aavg$Dock[i],A1Aavg$Mat[i],A1Aavg$Season[i])
}
idcol3

A1Aavg <- cbind(A1Aavg,idcol3)


A1Aavg <- A1Aavg[,c(4,6,10,23,24,25)]
View(A1Aavg)

A1Aavg_calc <- A1Aavg %>%
  group_by(idcol3) %>%
  summarise(season = Season, meanbarnacle = mean(Barnacles),
            meanencrust = mean(Enc..Bryo.),
            meanoyster = mean(Oysters))

View(unique(A1Aavg_calc))

unique(A1Aavg_calc) %>%
  group_by(season) %>%
  summarise(finalmeanbarnacle = mean(meanbarnacle), finalmeanencrusting = mean(meanencrust), finalmeanoyster = mean(meanoyster))

#Now calculate means for aquarina
Aquarinaavg <- read.csv("Aquarina Mean Set.csv")
View(Aquarinaavg)

length(Aquarinaavg$Dock)

idcol4 <- character(72)
for (i in 1:72) {
  idcol4[i] <- paste(Aquarinaavg$Dock[i],Aquarinaavg$Mat[i],Aquarinaavg$Season[i])
}
idcol4

Aquarinaavg <- cbind(Aquarinaavg,idcol4)


Aquarinaavg <- Aquarinaavg[,c(4,6,10,23,24,25)]
View(Aquarinaavg)

Aquarinaavg_calc <- Aquarinaavg %>%
  group_by(idcol4) %>%
  summarise(season = Season, meanbarnacle = mean(Barnacles),
            meanencrust = mean(Enc..Bryo.),
            meanoyster = mean(Oysters))

View(unique(Aquarinaavg_calc))

unique(Aquarinaavg_calc) %>%
  group_by(season) %>%
  summarise(finalmeanbarnacle = mean(meanbarnacle), finalmeanencrusting = mean(meanencrust), finalmeanoyster = mean(meanoyster))

#Now calculate means for Lighthouse Cove
LHavg <- read.csv("Lighthouse Cove Mean Set.csv")
View(LHavg)

length(LHavg$Dock)

idcol5 <- character(99)
for (i in 1:99) {
  idcol5[i] <- paste(LHavg$Dock[i],LHavg$Mat[i],LHavg$Season[i])
}
idcol5

LHavg <- cbind(LHavg,idcol5)


LHavg <- LHavg[,c(4,6,10,23,24,25)]
View(LHavg)

LHavg_calc <- LHavg %>%
  group_by(idcol5) %>%
  summarise(season = Season, meanbarnacle = mean(Barnacles),
            meanencrust = mean(Enc..Bryo.),
            meanoyster = mean(Oysters))

View(unique(LHavg_calc))

unique(LHavg_calc) %>%
  group_by(season) %>%
  summarise(finalmeanbarnacle = mean(meanbarnacle), finalmeanencrusting = mean(meanencrust), finalmeanoyster = mean(meanoyster))

#Now calculate means for Melbourne Beach Pier
getwd()
MBPavg <- read.csv("MBP Mean Set.csv")
View(MBPavg)

length(MBPavg$Dock)

idcol6 <- character(102)
for (i in 1:102) {
  idcol6[i] <- paste(MBPavg$Dock[i],MBPavg$Mat[i],MBPavg$Season[i])
}
idcol6

MBPavg <- cbind(MBPavg,idcol6)


MBPavg <- MBPavg[,c(4,6,10,23,24,25)]
View(MBPavg)

MBPavg_calc <- MBPavg %>%
  group_by(idcol6) %>%
  summarise(season = Season, meanbarnacle = mean(Barnacles),
            meanencrust = mean(Enc..Bryo.),
            meanoyster = mean(Oysters))

View(unique(MBPavg_calc))

unique(MBPavg_calc) %>%
  group_by(season) %>%
  summarise(finalmeanbarnacle = mean(meanbarnacle), finalmeanencrusting = mean(meanencrust), finalmeanoyster = mean(meanoyster))

#Now calculate means for Melbourne Shores
getwd()
MSavg <- read.csv("Melbourne Shores Mean Set.csv")
View(MSavg)



MSavg <- na.omit(MSavg)
length(MSavg$Dock)

idcol7 <- character(70)

for (i in 1:70) {
  idcol7[i] <- paste(MSavg$Dock[i],MSavg$Mat[i],MSavg$Season[i])
}
idcol7

MSavg <- cbind(MSavg,idcol7)


MSavg <- MSavg[,c(4,6,10,23,24,25)]
View(MSavg)

MSavg_calc <- MSavg %>%
  group_by(idcol7) %>%
  summarise(season = Season, meanbarnacle = mean(Barnacles),
            meanencrust = mean(Enc..Bryo.),
            meanoyster = mean(Oysters))

View(unique(MSavg_calc))

unique(MSavg_calc) %>%
  group_by(season) %>%
  summarise(finalmeanbarnacle = mean(meanbarnacle), finalmeanencrusting = mean(meanencrust), finalmeanoyster = mean(meanoyster))

#Now calculate means for Sebastian
getwd()
Sebastianavg <- read.csv("Sebastian Mean Set.csv")
View(Sebastianavg)



Sebastianavg <- na.omit(Sebastianavg)
length(Sebastianavg$Dock)

idcol8 <- character(57)

for (i in 1:57) {
  idcol8[i] <- paste(Sebastianavg$Dock[i],Sebastianavg$Mat[i],Sebastianavg$Season[i])
}
idcol8

Sebastianavg <- cbind(Sebastianavg,idcol8)


Sebastianavg <- Sebastianavg[,c(4,6,10,23,24,25)]
View(Sebastianavg)

Sebastianavg_calc <- Sebastianavg %>%
  group_by(idcol8) %>%
  summarise(season = Season, meanbarnacle = mean(Barnacles),
            meanencrust = mean(Enc..Bryo.),
            meanoyster = mean(Oysters))

View(unique(Sebastianavg_calc))

unique(Sebastianavg_calc) %>%
  group_by(season) %>%
  summarise(finalmeanbarnacle = mean(meanbarnacle), finalmeanencrusting = mean(meanencrust), finalmeanoyster = mean(meanoyster))

#Now calculate means for Wingate
getwd()
Wingateavg <- read.csv("Wingate Mean Set.csv")
View(Wingateavg)



Wingateavg <- na.omit(Wingateavg)
length(Wingateavg$Dock)

idcol9 <- character(72)

for (i in 1:72) {
  idcol9[i] <- paste(Wingateavg$Dock[i],Wingateavg$Mat[i],Wingateavg$Season[i])
}
idcol9

Wingateavg <- cbind(Wingateavg,idcol9)


Wingateavg <- Wingateavg[,c(4,6,10,23,24,25)]
View(Wingateavg)

Wingateavg_calc <- Wingateavg %>%
  group_by(idcol9) %>%
  summarise(season = Season, meanbarnacle = mean(Barnacles),
            meanencrust = mean(Enc..Bryo.),
            meanoyster = mean(Oysters))

View(unique(Wingateavg_calc))

unique(Wingateavg_calc) %>%
  group_by(season) %>%
  summarise(finalmeanbarnacle = mean(meanbarnacle), finalmeanencrusting = mean(meanencrust), finalmeanoyster = mean(meanoyster))

