library(lubridate)
library(dplyr)

bananaallnew <- read.csv("bananaallfull.csv")
#View(bananaallnew)


#Here is the get trailing means function
bananaallnew <- bananaallnew[,-1]
bananaallnew$newdate = parse_date_time(bananaallnew$Date, orders = "ymdHMS")

#View(bananaallnew)

getTrailingMeans = function(dataframe, endDate, trailingDays) {
  
  df <- subset(dataframe, as.Date(newdate) <= as.Date(endDate)
               & as.Date(newdate) > as.Date(endDate) - trailingDays)
  
  df <- df[complete.cases(df), ]
  
  return(colMeans(df[,2:6]))
}

#Here we will try to apply the get trailing means function to banana river
Shannon_Wiener_Merged <- read.csv("Shannon Wiener Merged All Data.csv")

days = 30

Shannon_Wiener_Merged['pH-mean-days'] <- 0.0
Shannon_Wiener_Merged['salinity-mean-days'] <- 0.0
Shannon_Wiener_Merged['temp-mean-days'] <- 0.0
Shannon_Wiener_Merged['turbidity-mean-days'] <- 0.0
Shannon_Wiener_Merged['oxy-mean-days'] <- 0.0

for (row in 1:nrow(Shannon_Wiener_Merged)) {
  assessmentDate = parse_date_time(Shannon_Wiener_Merged[row, "Date"], orders = "m/d/y")
  
  means = getTrailingMeans(bananaallnew, assessmentDate, days)
  
  Shannon_Wiener_Merged[row, 'pH-mean-days'] = means[1]
  Shannon_Wiener_Merged[row, 'salinity-mean-days'] = means[2]
  Shannon_Wiener_Merged[row, 'temp-mean-days'] = means[3]
  Shannon_Wiener_Merged[row, 'turbidity-mean-days'] = means[4]
  Shannon_Wiener_Merged[row, 'oxy-mean-days'] = means[5]
}


#Because the above retrieves water data from Banana River, this only corresponds with our IAP Dock

IAPmergedwithwaterdata <- Shannon_Wiener_Merged %>%
  filter(Dock == "IAP")
#View(IAPmergedwithwaterdata)



#Here is the get trailing means function for egallie
egallieallnew <- read.csv("egalallfull.csv")

#egallieallnew
egallieallnew$newdate = parse_date_time(egallieallnew$Date, orders = "ymdHMS")

getTrailingMeans = function(dataframe, endDate, trailingDays) {
  
  df <- subset(dataframe, as.Date(newdate) <= as.Date(endDate)
               & as.Date(newdate) > as.Date(endDate) - trailingDays)
  
  df <- df[complete.cases(df), ]
  
  return(colMeans(df[,2:5]))
}


str(egallieallnew)

egallieallnew <- egallieallnew[,-1]




#Here we will try to apply the get trailing means function to egallie river
Shannon1_Wiener_Merged <- read.csv("Shannon Wiener Merged All Data.csv")

days = 30

Shannon1_Wiener_Merged['pH-mean-days'] <- 0.0
Shannon1_Wiener_Merged['salinity-mean-days'] <- 0.0
Shannon1_Wiener_Merged['temp-mean-days'] <- 0.0
Shannon1_Wiener_Merged['turbidity-mean-days'] <- 0.0
Shannon1_Wiener_Merged['oxy-mean-days'] <- 0.0

for (row in 1:nrow(Shannon1_Wiener_Merged)) {
  assessmentDate = parse_date_time(Shannon1_Wiener_Merged[row, "Date"], orders = "m/d/y")
  
  means = getTrailingMeans(egallieallnew, assessmentDate, days)
  
  Shannon1_Wiener_Merged[row, 'pH-mean-days'] = means[1]
  Shannon1_Wiener_Merged[row, 'salinity-mean-days'] = means[2]
  Shannon1_Wiener_Merged[row, 'temp-mean-days'] = means[3]
  Shannon1_Wiener_Merged[row, 'turbidity-mean-days'] = means[4]
  Shannon1_Wiener_Merged[row, 'oxy-mean-days'] = means[5]
}


stationsmergedwithwaterdata_egallie <- Shannon1_Wiener_Merged %>%
  filter(Dock != "IAP")

stationsmergedwithwaterdata_egallie <- stationsmergedwithwaterdata_egallie[,-29]
#stationsmergedwithwaterdata_egallie <- na.omit(stationsmergedwithwaterdata_egallie)


IAPmergedwithwaterdata <- IAPmergedwithwaterdata[,-29]

CompleteWaterandPercents <- rbind(stationsmergedwithwaterdata_egallie, IAPmergedwithwaterdata)


CompleteWaterandPercents_actual <- CompleteWaterandPercents %>%
  filter(Dock != "Sebastian")
CompleteWaterandPercents_actual <- CompleteWaterandPercents %>%
  filter(Dock != "Sebastian")

#View(CompleteWaterandPercents_actual)

#Now that we've removed the outliers from vero, we can retrieve those dates.

veroallnew <- read.csv("veroallnooutliers.csv")


veroallnew$newdate = parse_date_time(veroallnew$Date, orders = "ymdHMS")

getTrailingMeans = function(dataframe, endDate, trailingDays) {
  
  df <- subset(dataframe, as.Date(newdate) <= as.Date(endDate)
               & as.Date(newdate) > as.Date(endDate) - trailingDays)
  
  df <- df[complete.cases(df), ]
  
  return(colMeans(df[,2:5]))
}


str(veroallnew)

veroallnew <- veroallnew[,-1]




#Here we will try to apply the get trailing means function to vero
Shannon1_Wiener_Merged <- read.csv("Shannon Wiener Merged All Data.csv")

days = 7

Shannon1_Wiener_Merged['pH-mean-days'] <- 0.0
Shannon1_Wiener_Merged['salinity-mean-days'] <- 0.0
Shannon1_Wiener_Merged['temp-mean-days'] <- 0.0
Shannon1_Wiener_Merged['turbidity-mean-days'] <- 0.0
Shannon1_Wiener_Merged['oxy-mean-days'] <- 0.0

for (row in 1:nrow(Shannon1_Wiener_Merged)) {
  assessmentDate = parse_date_time(Shannon1_Wiener_Merged[row, "Date"], orders = "m/d/y")
  
  means = getTrailingMeans(veroallnew, assessmentDate, days)
  
  Shannon1_Wiener_Merged[row, 'pH-mean-days'] = means[1]
  Shannon1_Wiener_Merged[row, 'salinity-mean-days'] = means[2]
  Shannon1_Wiener_Merged[row, 'temp-mean-days'] = means[3]
  Shannon1_Wiener_Merged[row, 'turbidity-mean-days'] = means[4]
  
}

#View(Shannon1_Wiener_Merged)

CompleteWaterandPercents_vero <- Shannon1_Wiener_Merged %>%
  filter(Dock == "Sebastian")
CompleteWaterandPercents_vero <- CompleteWaterandPercents_vero[,-29]
CompleteWaterandPercents_actual1 <- rbind(CompleteWaterandPercents_actual, CompleteWaterandPercents_vero)
#View(CompleteWaterandPercents_actual1)
write.csv(CompleteWaterandPercents_actual1, "C:/Users/alyss/OneDrive/Documents/CompleteWaterandPercents_actual1.csv")

CompleteWaterandPercents_30 <- CompleteWaterandPercents
getwd()
write.csv(CompleteWaterandPercents_30,"C:/Users/alyss/OneDrive/Documents/CompleteWaterandPercents_30.csv")

#Now we'll do this for 6 week periods
#Now that we've removed the outliers from vero, we can retrieve those dates.

veroallnew <- read.csv("veroallnooutliers.csv")


veroallnew$newdate = parse_date_time(veroallnew$Date, orders = "ymdHMS")

getTrailingMeans = function(dataframe, endDate, trailingDays) {
  
  df <- subset(dataframe, as.Date(newdate) <= as.Date(endDate)
               & as.Date(newdate) > as.Date(endDate) - trailingDays)
  
  df <- df[complete.cases(df), ]
  
  return(colMeans(df[,2:5]))
}


str(veroallnew)

veroallnew <- veroallnew[,-1]




#Here we will try to apply the get trailing means function to vero
Shannon1_Wiener_Merged <- read.csv("Shannon Wiener Merged All Data.csv")

days = 42

Shannon1_Wiener_Merged['pH-mean-days'] <- 0.0
Shannon1_Wiener_Merged['salinity-mean-days'] <- 0.0
Shannon1_Wiener_Merged['temp-mean-days'] <- 0.0
Shannon1_Wiener_Merged['turbidity-mean-days'] <- 0.0
Shannon1_Wiener_Merged['oxy-mean-days'] <- 0.0

for (row in 1:nrow(Shannon1_Wiener_Merged)) {
  assessmentDate = parse_date_time(Shannon1_Wiener_Merged[row, "Date"], orders = "m/d/y")
  
  means = getTrailingMeans(veroallnew, assessmentDate, days)
  
  Shannon1_Wiener_Merged[row, 'pH-mean-days'] = means[1]
  Shannon1_Wiener_Merged[row, 'salinity-mean-days'] = means[2]
  Shannon1_Wiener_Merged[row, 'temp-mean-days'] = means[3]
  Shannon1_Wiener_Merged[row, 'turbidity-mean-days'] = means[4]
  
}

#View(Shannon1_Wiener_Merged)

CompleteWaterandPercents_vero_30 <- Shannon1_Wiener_Merged %>%
  filter(Dock == "Sebastian")
CompleteWaterandPercents_vero_30 <- CompleteWaterandPercents_vero[,-29]
CompleteWaterandPercents_actual_30 <- rbind(CompleteWaterandPercents_actual, CompleteWaterandPercents_vero)
#View(CompleteWaterandPercents_actual1)
write.csv(CompleteWaterandPercents_actual_30, "C:/Users/alyss/OneDrive/Documents/CompleteWaterandPercents_actual_30.csv")
