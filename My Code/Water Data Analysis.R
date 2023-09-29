library(lubridate)
library(dplyr)
library(ggplot2)
library(xlsx)



getwd()

#First we get time series for Turbidity at egallie location, a water quality factor
egalturb <- read.csv("EgallieTurbFull.csv")
#View(egalturb)

str(egalturb)
egalturb <- egalturb %>% 
  rename(Date  = Timestamp..UTC.05.00.,  FNU = Value..Formazin.Nephelometric.Units.)
head(egalturb)
egalturb$Date <- as.character(egalturb$Date)
egalturb$Date<- mdy_hm(egalturb$Date)

str(egalturb)

min(egalturb$FNU)
max(egalturb$FNU)

length(na.omit(egalturb$FNU))
length(egalturb$FNU)


min(na.omit(egalturb$FNU))
max(na.omit(egalturb$FNU))


attach(egalturb)
plot(Date,FNU, main = "Turbidity", ylim=c(0, ymax))
detach(egalturb)

#Now we will get dissolved oxygen for egallie location, a water quality factor
egaldisso <- read.csv("EgalDissoFull.csv")
#View(egaldisso)

str(egaldisso)
egaldisso <- egaldisso %>% 
  rename(Date  = Timestamp..UTC.05.00.,  MilligramPerLitre = Value..Milligram.per.litre.)
head(egaldisso)
egaldisso$Date <- as.character(egaldisso$Date)
egaldisso$Date<- mdy_hm(egaldisso$Date)

str(egaldisso)




attach(egaldisso)
plot(Date,MilligramPerLitre, main = "Dissolved Oxygen Egallie")
detach(egaldisso)

#Now we will get water temperature for egallie location, a climate factor
egalwatertemp <- read.csv("EgalWaterTempFull.csv")
#View(egalwatertemp)

str(egalwatertemp)
egalwatertemp <- egalwatertemp %>% 
  rename(Date  = Timestamp..UTC.05.00.,  Celcius = Value..Celsius.)
head(egalwatertemp)
egalwatertemp$Date <- as.character(egalwatertemp$Date)
egalwatertemp$Date<- mdy_hm(egalwatertemp$Date)

str(egalwatertemp)

attach(egalwatertemp)
plot(Date,Celcius, main = "Water Temperature Egallie")
detach(egalwatertemp)

#Now we will get salinity for egallie location, a climate factor
egalsal <- read.csv("EgalSalFull.csv")
#View(egalsal)

str(egalsal)
egalsal <- egalsal %>% 
  rename(Date  = Timestamp..UTC.05.00.,  PartsPerThousand = Value..Parts.per.thousand.)
head(egalsal)
egalsal$Date <- as.character(egalsal$Date)
egalsal$Date<- mdy_hm(egalsal$Date)

str(egalsal)

attach(egalsal)
plot(Date,PartsPerThousand, main = "Salinity Egallie")
detach(egalsal)

#Now let's merge all the egallie data into 1 dataset

egaldisso <- egaldisso[,c(1,2)]
egalturb <- egalturb[,c(1,2)]
egaldwatertemp <- egalwatertemp[,c(1,2)]
egalsal <- egalsal[,c(1,2)]
#egalph <- egalph[,c(1,2)]


egaldissoturb <- merge(egaldisso,egalturb,by = 'Date')
egaltempsal <- merge(egalwatertemp,egalsal,by = 'Date')
egalall <- merge(egaldissoturb,egaltempsal,by = 'Date')

#View(egalall)


egalall <- egalall[,-c(5,6,7,8)]

egalallfull <- egalall %>%
  rename(Salinity_PartsPerThousand = PartsPerThousand,
         WaterTemp_Celcius = Celcius,
         Turbidity_FNU = FNU,
         DissOxy_MilligramPerLitre = MilligramPerLitre)

#plot(egalallfull$Date,egalallfull$Turbidity_FNU, main = "Egallie Turbidity", ylim = c(0,150))
plot(egalallfull$Date,egalallfull$DissOxy_MilligramPerLitre, main = "Egallie pH",ylab = "pH Scientific Units")
getwd()
egalallfull <- na.omit(egalallfull)
write.csv(egalallfull, file = "C:/Users/alyss/OneDrive/Documents/egalallfull.csv")

#Now we will get turbidity for banana river location, a water quality factor
bananaturb <- read.csv("BananaTurbFull_actual.csv")
#View(bananaturb)

str(bananaturb)
bananaturb <- bananaturb %>% 
  rename(Date  = Timestamp..UTC.05.00., FNU = Value..Formazin.Nephelometric.Units.)
head(bananaturb)
bananaturb$Date <- as.character(bananaturb$Date)
bananaturb$Date<- mdy_hm(bananaturb$Date)

str(bananaturb)

attach(bananaturb)
plot(Date,FNU, main = "Banana River Turbidity")
detach(bananaturb)

#Now we will get dissolved oxygen for banana river location, a water quality factor
bananadisso <- read.csv("BananaDissoFull.csv")
#View(bananadisso)

str(bananadisso)
bananadisso <- bananadisso %>% 
  rename(Date  = Timestamp..UTC.05.00., MilligramPerLitre = Value..Milligram.per.litre.)
head(bananadisso)
bananadisso$Date <- as.character(bananadisso$Date)
bananadisso$Date<- mdy_hm(bananadisso$Date)

str(bananadisso)

attach(bananadisso)
plot(Date,MilligramPerLitre, main = "Banana River Dissolved Oxygen")
detach(bananadisso)

#Now we will get water temperature for banana river, a climate factor
bananawatertemp <- read.csv("BananaWaterTempFull.csv")
#View(bananawatertemp)

str(bananawatertemp)
bananawatertemp <- bananawatertemp %>% 
  rename(Date  = Timestamp..UTC.05.00., Celcius = Value..Celsius.)
head(bananawatertemp)
bananawatertemp$Date <- as.character(bananawatertemp$Date)
bananawatertemp$Date<- mdy_hm(bananawatertemp$Date)

str(bananawatertemp)

attach(bananawatertemp)
plot(Date,Celcius, main = "Banana River Water Temperature")
detach(bananawatertemp)


#Now we will get salinity for banana river, a climate factor
bananasal<- read.csv("BananaSalFull.csv")
#View(bananasal)

str(bananasal)
bananasal <- bananasal %>% 
  rename(Date  = Timestamp..UTC.05.00., PartsPerThousand = Value..Parts.per.thousand.)
head(bananasal)
bananasal$Date <- as.character(bananasal$Date)
bananasal$Date<- mdy_hm(bananasal$Date)

str(bananasal)

attach(bananasal)
plot(Date,PartsPerThousand, main = "Banana River Salinity")
detach(bananasal)

#Now we will get pH for banana river, a climate factor
bananaph<- read.csv("BananaPhFull.csv")
#View(bananaph)

str(bananaph)
bananaph <- bananaph %>% 
  rename(Date  = Timestamp..UTC.05.00., ScientificUnits = Value..Scientific.Unit.)
head(bananaph)
bananaph$Date <- as.character(bananaph$Date)
bananaph$Date<- mdy_hm(bananaph$Date)

str(bananaph)

attach(bananaph)
plot(Date,ScientificUnits, main = "Ph Banana River")
detach(bananaph)

#Here we merge our Banana River climate factors into one dataset
bananadisso <- bananadisso[,c(1,2)]
bananaturb <- bananaturb[,c(1,2)]
bananadwatertemp <- bananawatertemp[,c(1,2)]
bananasal <- bananasal[,c(1,2)]
bananaph <- bananaph[,c(1,2)]


bananaphsal <- merge(bananaph,bananasal,by = 'Date')
bananatempturb <- merge(bananawatertemp,bananaturb,by = 'Date')
banananotdisso <- merge(bananaphsal,bananatempturb,by = 'Date')
bananaall <- merge(banananotdisso,bananadisso,by = 'Date')


bananaall <- bananaall[,-c(5,6,7,8)]

bananaall <- bananaall %>%
  rename(pH_ScientificUnits = ScientificUnits, 
         Salinity_PartsPerThousand = PartsPerThousand,
         WaterTemp_Celcius = Celcius,
         Turbidity_FNU = FNU,
         DissOxy_MilligramPerLitre = MilligramPerLitre)
#View(bananaall)


ggplot(data = bananaall, aes(x = Date)) + 
  geom_point(aes(y = pH_ScientificUnits, color = "pH_ScientificUnits")) +
  geom_point(aes(y = Salinity_PartsPerThousand, color = "Salinity_PartsPerThousand")) +
  geom_point(aes(y = WaterTemp_Celcius, color = "WaterTemp_Celcius")) 

#write.table(bananaall, file = "bananaall.txt", sep = "\t",
           # row.names = TRUE, col.names = NA)

getwd()
#Now we will get the Dissolved Oxygen Data for Vero Location
verodisso <- read.csv("DissoVeroFull.csv")
#View(verodisso)

str(verodisso)
verodisso <- verodisso %>% 
  rename(Date  = Timestamp..UTC.05.00.,  MilligramPerLitre = Value..Milligram.per.litre.)
head(verodisso)
verodisso$Date <- as.character(verodisso$Date)
verodisso$Date<- mdy_hm(verodisso$Date)

str(verodisso)


#Now we will get the Turbidity for Vero Location
veroturb <- read.csv("TurbidityVeroFull.csv")
#View(veroturb)

str(veroturb)
veroturb <- veroturb %>% 
  rename(Date  = Timestamp..UTC.05.00.,  FNU = Value..Formazin.Nephelometric.Units.)
head(veroturb)
veroturb$Date <- as.character(veroturb$Date)
veroturb$Date<- mdy_hm(veroturb$Date)

str(veroturb)

#Now we will get the Water Temperature for Vero Location
verowatertemp <- read.csv("VeroWaterTempFull.csv")
#View(verowatertemp)

str(verowatertemp)
verowatertemp <- verowatertemp %>% 
  rename(Date  = Timestamp..UTC.05.00.,  Celcius = Value..Celsius.)
head(verowatertemp)
verowatertemp$Date <- as.character(verowatertemp$Date)
verowatertemp$Date<- mdy_hm(verowatertemp$Date)

str(verowatertemp)

#Now we will get the Salinity for Vero Location
verosal <- read.csv("verosalfull.csv")
#View(verosal)

str(verosal)
verosal <- verosal %>% 
  rename(Date  = Timestamp..UTC.05.00.,  PartsPerThousand = Value..Parts.per.thousand.)
head(verosal)
verosal$Date <- as.character(verosal$Date)
verosal$Date<- mdy_hm(verosal$Date)

str(verosal)

#Lastly for our Vero Location we will get our pH data
veroph <- read.csv("VeroPhFull.csv")
#View(veroph)

str(veroph)
veroph <- veroph %>% 
  rename(Date  = Timestamp..UTC.05.00.,  ScientificUnits = Value..Scientific.Unit.)
head(veroph)
veroph$Date <- as.character(veroph$Date)
veroph$Date<- mdy_hm(veroph$Date)

str(veroph)

#Now let's make a Vero Location Climate dataset
verodisso <- verodisso[,c(1,2)]
veroturb <- veroturb[,c(1,2)]
verowatertemp <- verowatertemp[,c(1,2)]
verosal <- verosal[,c(1,2)]
veroph <- veroph[,c(1,2)]




verophsal <- merge(veroph,verosal,by = 'Date')
verotempturb <- merge(verowatertemp,veroturb,by = 'Date')
veronotdisso <- merge(verophsal,verotempturb,by = 'Date')
veroall <- merge(veronotdisso,verodisso,by = 'Date')

veroall <- veroall %>%
  rename(pH_ScientificUnits = ScientificUnits, 
         Salinity_PartsPerThousand = PartsPerThousand,
         WaterTemp_Celcius = Celcius,
         Turbidity_FNU = FNU,
         DissOxy_MilligramPerLitre = MilligramPerLitre)
#View(veroall)

attach(veroall)
plot(Date,veroall$pH_ScientificUnits, main = "Vero Ph")
plot(Date,veroall$Salinity_PartsPerThousand, main = "Vero Salinity")
plot(Date,veroall$WaterTemp_Celcius, main = "Vero Water Temperature")
plot(Date,veroall$Turbidity_FNU, main = "Vero Turbidity",ylim=c(0,30))
plot(Date,veroall$DissOxy_MilligramPerLitre, main = "Dissolved Oxygen Vero")
detach(veroall)

attach(veroall)
plot(Date,veroall$pH_ScientificUnits, main = "Vero Ph")
plot(Date,veroall$Salinity_PartsPerThousand, main = "Vero Salinity")
plot(Date,veroall$WaterTemp_Celcius, main = "Vero Water Temperature")
plot(Date,veroall$Turbidity_FNU, main = "Vero Turbidity",ylim=c(0,30))
plot(Date,veroall$DissOxy_MilligramPerLitre, main = "Dissolved Oxygen Vero")
detach(veroall)



#Now we will put our cemenation data into here, and convert the date column to dates, and create
#an id column
#cementation <- read.csv("Cementation, but right with dates.csv")
#str(cementation)

#cementation <- cementation[,c(1,2,4,5)]

#cementation <- na.omit(cementation)

#cementation$Date <- as.character(cementation$Date)
#cementation$Date<- mdy(cementation$Date)
#str(cementation)

#View(cementation)
#idcol <- character(136)
#for (i in 1:136) {
#  idcol[i] <- paste(cementation$Mat[i], cementation$Dock[i])
#}
#idcol

#cementation <- cbind(idcol,cementation[,c(3,4)])
#head(cementation)

#dateandtime <- character(3264)

#timeonly <- character(24)
#for(h in 1:24){
#  timeonly[h] <- paste(" ",h,":00:00")
#}
#timeonly

#dateonly <- as.character(cementation$Date)
#dateonly

#a <- character(24)
#b <- character(24)
#for (i in 1:3264) {
#  for(h in 1:24){
#    a[h] <- timeonly[h]
#    b[h] <- cementation$Date[i]
    
#    combin_ab <- paste(a[h],b[h])
#  }
#  dateandtime[i] <- 
#}

#Here we convert our weather data from hourly units to daily units

str(veroall)

veroalldaily <- veroall %>%
  mutate(date = floor_date(Date,unit = "day")) %>%
  group_by(date) %>%
  summarize(mean_salinity_partsperthousand = mean(Salinity_PartsPerThousand),
            mean_watertemp_celcius = mean(WaterTemp_Celcius),
            mean_turbidity_fnu = mean(Turbidity_FNU))

?floor_date()  
?date()

#View(veroalldaily)


#Here we will get our Settlement Data into r
shannonall <- read.csv("Shannon Wiener Merged All Data.csv")

shannonall <- shannonall[,c(1,2,3,4,6,10,23,24)]


idcol1 <- character(681)
for (i in 1:681) {
  idcol1[i] <- paste(shannonall$Dock[i],shannonall$Mat[i],shannonall$Shell[i])
}
idcol1

shannonall <- cbind(idcol1, shannonall)
#View(shannonall)
str(shannonall)
shannonall$Date <- as.character(shannonall$Date)
shannonall$Date<- mdy(shannonall$Date)
str(shannonall)

#shannonall <- shannonall[,-c(4)]


veroalldaily <- veroalldaily %>%
  rename(Date = date)
#View(veroalldaily)

veroalldaily$Date <- ymd(veroalldaily$Date)


shannonallwithvero <- merge(veroalldaily,shannonall,by = 'Date')
#View(shannonallwithvero)

plot(veroall$Date,veroall$Turbidity_FNU, ylim = c(0,200))
Q_veroturb <- quantile(veroall$Turbidity_FNU, probs=c(.0001, .99), na.rm = TRUE)
Q_veroturb

length(veroall$Turbidity_FNU)
mean(na.omit(veroall$Turbidity_FNU))
veroturbidity_nooutliers <- veroall$Turbidity_FNU

veroturbidity_nooutliers[is.na(veroturbidity_nooutliers)] <- 151
veroall$Turbidity_FNU
outliercount <- 0

for (i in 1:35036) {
  if(veroturbidity_nooutliers[i] > 150){
    veroturbidity_nooutliers[i] <- 7.078879
    outliercount <- 1 + outliercount
  }
}

outliercount
outliercount/35036
plot(veroturbidity_nooutliers)

veroall$Turbidity_FNU <- veroturbidity_nooutliers


hist(egalallfull$Turbidity_FNU, xlim = c(0,100), main = "Egallie Turbidity Histogram", breaks = 30, col = "darkmagenta")
hist(bananaallfull$Turbidity_FNU, xlim = c(0,100), main = "Banana River Turbidity Histogram", breaks = 15, col = "pink")
hist(veroall$Turbidity_FNU,main = "Vero Turbidity Histogram", col = "lightblue")

veroallnooutliers <- veroall
getwd()

write.csv(veroallnooutliers, "C:/Users/alyss/OneDrive/Documents/veroallnooutliers.csv")
