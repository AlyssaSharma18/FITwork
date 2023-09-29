
#Beta Regression
# betareg(formula, data, subset, na.action, weights, offset,
#         link = "logit", link.phi = NULL, control = betareg.control(...),
#         model = TRUE, y = TRUE, x = FALSE, ...)

library(betareg)
library(partykit)
library(ggplot2)
library(olsrr)

betawatertest_withvero <- read.csv("CompleteWaterandPercents_actual1.csv")
betawatertest <- read.csv("CompleteWaterandPercents.csv")
betawatertest_30 <- read.csv("CompleteWaterandPercents_30.csv")
matdatabeta <- read.csv("matdata3.csv")


View(matdatabeta)
betawatertest <- betawatertest[,c(2,3,4,5,7,11,24,25,26,27,28,29)]
betawatertest[,c(4,5,6)] <- betawatertest[,c(4,5,6)]/100
betawatertest[,c(4,5,6)]<- betawatertest[,c(4,5,6)] + .001

betawatertest_30 <- betawatertest_30[,c(2,3,4,5,7,11,24,25,26,27,28,29)]
betawatertest_30[,c(4,5,6)] <- betawatertest_30[,c(4,5,6)]/100
betawatertest_30[,c(4,5,6)]<- betawatertest_30[,c(4,5,6)] + .001

betawatertest_withvero <- betawatertest_withvero[,c(2,3,4,5,7,11,24,25,26,27,28,29)]
betawatertest_withvero[,c(4,5,6)] <- betawatertest_withvero[,c(4,5,6)]/100
betawatertest_withvero[,c(4,5,6)]<- betawatertest_withvero[,c(4,5,6)] + .001

hist(betawatertest$Barnacles)
hist(betawatertest$Enc..Bryo.)
hist(betawatertest$Oysters)

str(GasolineYield)
## Data
data("GasolineYield", package = "betareg")

#View(betawatertest)
str(betawatertest)

## Beta regression with log link 

bwt_log_all_barn <- betareg(Barnacles ~ Dock + Season + pH.mean.days + 
                               salinity.mean.days + temp.mean.days + 
                               turbidity.mean.days, data = betawatertest_30, link = "log")
summary(bwt_log_all_barn)
AIC(bwt_log_all_barn)

bwt_log_shortened_barn <- betareg(Barnacles ~ Dock + Season + 
                   salinity.mean.days +  
                   turbidity.mean.days, data = betawatertest, link = "log")
summary(bwt_log_shortened_barn)
AIC(bwt_log_shortened_barn)

bwt_log_all_oys <- betareg(Oysters ~ Dock + Season + pH.mean.days + 
                      salinity.mean.days + temp.mean.days + 
                      turbidity.mean.days, data = betawatertest, link = "log")
summary(bwt_log_all_oys)
AIC(bwt_log_all_oys)


bwt_log_shortened_oys <- betareg(Oysters ~ Dock , data = betawatertest, link = "log")
summary(bwt_log_shortened_oys)
AIC(bwt_log_shortened_oys)

bwt_log_weather_oys <- betareg(Oysters ~ pH.mean.days + 
                                    salinity.mean.days + temp.mean.days + 
                                    turbidity.mean.days, data = betawatertest, link = "log")
summary(bwt_log_weather_oys)
AIC(bwt_log_weather_oys)

bwt_log_all_barn_30 <- betareg(Barnacles ~ Dock + Season, data = betawatertest_30, link = "log")

summary(bwt_log_all_barn_30)
AIC(bwt_log_all_barn_30)

bwt_log_all_oys_30 <- betareg(Oysters ~ Dock + Season + pH.mean.days + 
                      salinity.mean.days + temp.mean.days + 
                      turbidity.mean.days, data = betawatertest_30, link = "log")
summary(bwt_log_all_oys_30)
AIC(bwt_log_all_oys_30)


enc_log_all <- betareg(Enc..Bryo. ~ Dock + Season + pH.mean.days + 
                                 salinity.mean.days + temp.mean.days + 
                                 turbidity.mean.days, data = betawatertest, link = "log")
summary(enc_log_all)
AIC(enc_log_all)

enc_log_short <- betareg(Enc..Bryo. ~ Dock + Season + pH.mean.days, data = betawatertest, link = "log")
summary(enc_log_short)
AIC(enc_log_short)

enc_log_all_30 <- betareg(Enc..Bryo. ~ Dock + Season + pH.mean.days+ 
                             salinity.mean.days + temp.mean.days + 
                             turbidity.mean.days, data = betawatertest_30, link = "log")
summary(enc_log_all_30)
AIC(enc_log_all_30)



#Here we start using the dataset with 422 cols

matdatabeta <- matdatabeta[,-1]
matdatabeta[,4:22] <- matdatabeta[,4:22]/100
matdatabeta[,4:22] <- matdatabeta[,4:22] + .001
#View(matdatabeta)


str(matdatabeta)

matdatabeta$Oysters

#bwt_log <- betareg(Enc..Bryo.~., data = matdatabeta)
lm <- lm(Oysters~., data = matdatabeta)
summary(lm)

## Beta regression with logit link

gy_logit <- betareg(yield ~ batch + temp, data = GasolineYield) #, link = "logit")
summary(gy_logit)

bwt_logit_all_barn <- betareg(Barnacles ~ Dock + Season + pH.mean.days + 
                               salinity.mean.days + temp.mean.days + 
                               turbidity.mean.days, data = betawatertest, link = "logit")
summary(bwt_logit_all_barn)
AIC(bwt_logit_all_barn)

bwt_logit_shortened_barn <- betareg(Barnacles ~ Dock + Season + 
                                     salinity.mean.days +  
                                     turbidity.mean.days, data = betawatertest, link = "logit")
summary(bwt_logit_shortened_barn)
AIC(bwt_logit_shortened_barn)

bwt_logit_all_oys <- betareg(Oysters ~ Dock + Season + pH.mean.days + 
                              salinity.mean.days + temp.mean.days + 
                              turbidity.mean.days, data = betawatertest, link = "logit")
summary(bwt_logit_all_oys)
AIC(bwt_logit_all_oys)


bwt_logit_shortened_oys <- betareg(Oysters ~ Dock , data = betawatertest, link = "logit")
summary(bwt_logit_shortened_oys)
AIC(bwt_logit_shortened_oys)

bwt_logit_weather_oys <- betareg(Oysters ~ pH.mean.days + 
                                  salinity.mean.days + temp.mean.days + 
                                  turbidity.mean.days, data = betawatertest, link = "logit")
summary(bwt_logit_weather_oys)
AIC(bwt_logit_weather_oys)

bwt_logit_all_barn_30 <- betareg(Barnacles ~ Dock + Season + pH.mean.days + 
                                  salinity.mean.days + temp.mean.days + 
                                  turbidity.mean.days, data = betawatertest_30, link = "logit")

summary(bwt_logit_all_barn_30)
AIC(bwt_logit_all_barn_30)

bwt_log_all_oys_30 <- betareg(Oysters ~ Dock + Season + pH.mean.days + 
                                 salinity.mean.days + temp.mean.days + 
                                 turbidity.mean.days, data = betawatertest_30, link = "logit")
summary(bwt_log_all_oys_30)
AIC(bwt_log_all_oys_30)


enc_logit_all <- betareg(Enc..Bryo. ~ Dock + Season + pH.mean.days + 
                          salinity.mean.days + temp.mean.days + 
                          turbidity.mean.days, data = betawatertest, link = "logit")
summary(enc_logit_all)
AIC(enc_logit_all)

enc_logit_short <- betareg(Enc..Bryo. ~ Dock + Season + pH.mean.days + temp.mean.days, data = betawatertest, link = "logit")
summary(enc_logit_short)
AIC(enc_logit_short)

enc_logit_all_30 <- betareg(Enc..Bryo. ~ Dock + Season + pH.mean.days+ 
                             salinity.mean.days + temp.mean.days + 
                             turbidity.mean.days, data = betawatertest_30, link = "logit")
summary(enc_logit_all_30)
AIC(enc_logit_all_30)
## Visualization of data and models

coef(bwt_log)
AIC(bwt_log)
pseudo.r.squared(bwt_log)
bwt_log$
#1 - as.vector(logLik(bwt_log))

par(mfrow = c(1, 1), mar = c(5.1, 4.1, 4.1, 2.1))

redblue <- hcl(c(0, 130, 260), 90, 40)

plot(Barnacles ~ temp.mean.days, data = betawatertest, type = "n",
     ylab = "Barnacle Percent Cover",
     xlab = "Sea Surface Temperature",
     main = "Barnacle Percent Cover using 7 day average Climate Data")

points(Barnacles ~ temp.mean.days, data = betawatertest, cex = 1.75, 
       pch = 19, col = as.numeric(Season))

points(Barnacles ~ temp.mean.days, data = betawatertest, cex = 1.75)

legend("topleft", as.character(1:10), title = "Season",
       col = rev(gray.colors(10)), pch = 19, bty = "n")

#legend("topleft", as.character(1:10), title = "Season",
  #     col = rev(gray.colors(10)), pch = 19, bty = "n")

#legend("topleft", as.character(1:10), title = "Season", pch = 1, bty = "n")
#legend("topleft", as.character(1:3), title = "Season", col)

lines(0:30, predict(bwt_log_temp, 
                       newdata = data.frame(temp.mean.days = 0:30, Season = "3")),
      col = redblue[3], lwd = 2, lty = 4)

lines(0:30, predict(bwt_logit_temp, 
                       newdata = data.frame(temp.mean.days = 0:30, Season = "3")),
      col = redblue[2], lwd = 2, lty = 2)

lines(0:30, predict(bwt_loglog_temp, 
                       newdata = data.frame(temp.mean.days = 0:30, Season = "3")),
      col = redblue[1], lwd = 2)

legend("bottomright", c("log", "log-log", "logit"),
       col = redblue, lty = c(4,2,1), lwd = 2, bty = "n")



#Let's try to visualize with ggplot instead

fit.ggplot.log <- data.frame(y=predict(bwt_log, newdata=betawatertest),x=betawatertest$temp.mean.days)
#View(fit.ggplot.log)

ggplot(data = betawatertest, aes(x = temp.mean.days, y = Barnacles, col = Season)) + 
   geom_point() #+ 
   #geom_line(data=fit.ggplot.log, aes(x=x,y=y))

#Let's compare models
AIC(bwt_log_shortened)
AIC(bwt_log_all)









#Let's redo everything using the vero station for sebastian
## Beta regression with log link 
#begin copy point


bwt_log_all_barn_withvero <- betareg(Barnacles ~ Dock + Season + pH.mean.days + 
                               salinity.mean.days + temp.mean.days + 
                               turbidity.mean.days, data = betawatertest_withvero, link = "log")
summary(bwt_log_all_barn_withvero)
AIC(bwt_log_all_barn_withvero)

withvero_log_shortened_barn <- betareg(Barnacles ~ Dock + Season, 
                                       data = betawatertest_withvero, link = "log")
summary(withvero_log_shortened_barn)
AIC(withvero_log_shortened_barn)

withvero_log_all_oys <- betareg(Oysters ~ Dock + Season + pH.mean.days + 
                              salinity.mean.days + temp.mean.days + 
                              turbidity.mean.days, data = betawatertest_withvero, link = "log")
summary(withvero_log_all_oys)
AIC(withvero_log_all_oys)


withvero_log_shortened_oys <- betareg(Oysters ~ Dock , data = betawatertest_withvero, link = "log")
summary(withvero_log_shortened_oys)
AIC(bwt_log_shortened_oys)

withvero_log_weather_oys <- betareg(Oysters ~ pH.mean.days + 
                                  salinity.mean.days + temp.mean.days + 
                                  turbidity.mean.days, data = betawatertest_withvero, link = "log")
summary(withvero_log_weather_oys)
AIC(withvero_log_weather_oys)

withvero_log_weather_barn <- betareg(Barnacles ~ pH.mean.days + 
                                       salinity.mean.days + temp.mean.days + 
                                       turbidity.mean.days, data = betawatertest_withvero, link = "log")
summary(withvero_log_weather_barn)
AIC(withvero_log_weather_barn)

bwt_log_all_barn_30 <- betareg(Barnacles ~ Dock + Season, data = betawatertest_30, link = "log")

summary(bwt_log_all_barn_30)
AIC(bwt_log_all_barn_30)

bwt_log_all_oys_30 <- betareg(Oysters ~ Dock + Season + pH.mean.days + 
                                 salinity.mean.days + temp.mean.days + 
                                 turbidity.mean.days, data = betawatertest_30, link = "log")
summary(bwt_log_all_oys_30)
AIC(bwt_log_all_oys_30)


enc_log_all_withvero <- betareg(Enc..Bryo. ~ Dock + Season + pH.mean.days + 
                          salinity.mean.days + temp.mean.days + 
                          turbidity.mean.days, data = betawatertest_withvero, link = "log")
summary(enc_log_all_withvero)
AIC(enc_log_all_withvero)

enc_log_short_withvero <- betareg(Enc..Bryo. ~ Dock + Season + pH.mean.days + temp.mean.days, data = betawatertest, link = "log")
summary(enc_log_short)
AIC(enc_log_short)

enc_log_all_30 <- betareg(Enc..Bryo. ~ Dock + Season + pH.mean.days+ 
                             salinity.mean.days + temp.mean.days + 
                             turbidity.mean.days, data = betawatertest_30, link = "log")
summary(enc_log_all_30)
AIC(enc_log_all_30)
#end copy point
#Here we start using the dataset with 422 cols

matdatabeta <- matdatabeta[,-1]
matdatabeta[,4:22] <- matdatabeta[,4:22]/100
matdatabeta[,4:22] <- matdatabeta[,4:22] + .001
#View(matdatabeta)


str(matdatabeta)

matdatabeta$Oysters

#bwt_log <- betareg(Enc..Bryo.~., data = matdatabeta)
lm <- lm(Oysters~., data = matdatabeta)
summary(lm)

## Beta regression with logit link

gy_logit <- betareg(yield ~ batch + temp, data = GasolineYield) #, link = "logit")
summary(gy_logit)

bwt_logit_all_barn <- betareg(Barnacles ~ Dock + Season + pH.mean.days + 
                                 salinity.mean.days + temp.mean.days + 
                                 turbidity.mean.days, data = betawatertest, link = "logit")
summary(bwt_logit_all_barn)
AIC(bwt_logit_all_barn)

bwt_logit_shortened_barn <- betareg(Barnacles ~ Dock + Season + 
                                       salinity.mean.days +  
                                       turbidity.mean.days, data = betawatertest, link = "logit")
summary(bwt_logit_shortened_barn)
AIC(bwt_logit_shortened_barn)

bwt_logit_all_oys <- betareg(Oysters ~ Dock + Season + pH.mean.days + 
                                salinity.mean.days + temp.mean.days + 
                                turbidity.mean.days, data = betawatertest, link = "logit")
summary(bwt_logit_all_oys)
AIC(bwt_logit_all_oys)


bwt_logit_shortened_oys <- betareg(Oysters ~ Dock , data = betawatertest, link = "logit")
summary(bwt_logit_shortened_oys)
AIC(bwt_logit_shortened_oys)

bwt_logit_weather_oys <- betareg(Oysters ~ pH.mean.days + 
                                    salinity.mean.days + temp.mean.days + 
                                    turbidity.mean.days, data = betawatertest, link = "logit")
summary(bwt_logit_weather_oys)
AIC(bwt_logit_weather_oys)

bwt_logit_all_barn_30 <- betareg(Barnacles ~ Dock + Season + pH.mean.days + 
                                    salinity.mean.days + temp.mean.days + 
                                    turbidity.mean.days, data = betawatertest_30, link = "logit")

summary(bwt_logit_all_barn_30)
AIC(bwt_logit_all_barn_30)

bwt_log_all_oys_30 <- betareg(Oysters ~ Dock + Season + pH.mean.days + 
                                 salinity.mean.days + temp.mean.days + 
                                 turbidity.mean.days, data = betawatertest_30, link = "logit")
summary(bwt_log_all_oys_30)
AIC(bwt_log_all_oys_30)


enc_logit_all <- betareg(Enc..Bryo. ~ Dock + Season + pH.mean.days + 
                            salinity.mean.days + temp.mean.days + 
                            turbidity.mean.days, data = betawatertest, link = "logit")
summary(enc_logit_all)
AIC(enc_logit_all)

enc_logit_short <- betareg(Enc..Bryo. ~ Dock + Season + pH.mean.days + temp.mean.days, data = betawatertest, link = "logit")
summary(enc_logit_short)
AIC(enc_logit_short)

enc_logit_all_30 <- betareg(Enc..Bryo. ~ Dock + Season + pH.mean.days+ 
                               salinity.mean.days + temp.mean.days + 
                               turbidity.mean.days, data = betawatertest_30, link = "logit")
summary(enc_logit_all_30)
AIC(enc_logit_all_30)

getwd()
write.csv(matdatabeta, "C:/Users/alyss/OneDrive/Documents/matdatabeta.csv")




