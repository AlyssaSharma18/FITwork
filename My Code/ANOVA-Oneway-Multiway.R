

###########################################
###########################################
# getwd()
# setwd("C:/Florida Tech/Courses/MTH 5401/Summer 2020/Lectures/Week 5/Session 1")

library(carData)
###########################################
###########################################
# Inspect the data
sample_n(Salaries, 3)
glimpse(Salaries)
head(Salaries)
names(Salaries)
attach(Salaries)

###########################################
library(ggplot2)
library(Rmisc)

smry = summarySE(Salaries,
                measurevar="salary",
                groupvars=c("sex","rank"))

smry

pd = position_dodge(.2)

ggplot(smry, aes(x=rank,
                y=salary,
                color=sex)) +
  geom_errorbar(aes(ymin=salary-se,
                    ymax=salary+se),
                width=.2, size=0.7, position=pd) +
  geom_point(shape=15, size=4, position=pd) +
  theme_bw() +
  theme(
    axis.title.y = element_text(vjust= 1.8),
    axis.title.x = element_text(vjust= -0.5),
    axis.title = element_text(face = "bold")) +
  scale_color_manual(values = c("black", "blue"))

###########################################
###########################################
levels(rank)
plot(salary~rank)
anvml<-aov(salary~rank)
summary(anvml)

pairwise.t.test(salary, rank, p.adjust="bonferroni")
TukeyHSD(anvml, conf.level = 0.95)

###########################################
levels(sex)
plot(salary~sex)
anvml<-aov(salary~sex)
summary(anvml)

pairwise.t.test(salary, sex, p.adjust="bonferroni")
TukeyHSD(anvml, conf.level = 0.95)

###########################################
levels(discipline)
plot(salary~discipline)
anvml<-aov(salary~discipline)
summary(anvml)

pairwise.t.test(salary, discipline, p.adjust="bonferroni")
TukeyHSD(anvml, conf.level = 0.95)

###########################################
anvml<-aov(salary~rank+discipline)
summary(anvml)

TukeyHSD(anvml, conf.level = 0.95)

###########################################
anvml<-aov(salary~rank+sex+discipline)
summary(anvml)

TukeyHSD(anvml, conf.level = 0.95)

###########################################
anvml<-aov(salary~rank+sex+discipline+rank*sex)
summary(anvml)

###########################################
boxplot(salary ~ rank,
        data = Salaries,
        xlab = "rank",
        ylab = "salary")

###########################################
boxplot(salary ~ rank:sex,
        data = Salaries,
        xlab = "Rank x Gender",
        ylab = "salary")



