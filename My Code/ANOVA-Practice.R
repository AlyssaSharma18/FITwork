
library(dplyr)
library("ggpubr")
library(multcomp)
library(car)
library(data.table)
library(Rmisc)
library(tidyr)


bethniccom <- read.csv("Shannon Wiener Merged Fixed.csv",header = TRUE)

bethniccom_practice <- bethniccom[,4:21]
bethniccom_practice <-bethniccom_practice %>% 
  pivot_longer(cols = !Season, names_to = "Organism",values_to = "PercentCover")

mode(bethniccom_practice)
bethniccom_practice$Season <- as.factor(bethniccom_practice$Season)
bethniccom_practice$Organism <- as.factor(bethniccom_practice$Organism)
bethniccom_practice$PercentCover <- as.numeric(bethniccom_practice$PercentCover)
str(bethniccom_practice)



# Show the levels
levels(bethniccom_practice$Organism)

bethniccom_practice$Organism <- ordered(bethniccom_practice$Organism,
                                        levels = c("Algae", "Amphipod.Tubes", "Anemones","Arb..Bryo.",
                                                   "Bare.Shell","Barnacles","Biofilm","Enc..Bryo.",
                                                   "Feather.Duster.Worm","Hydroids","Oysters",
                                                   "Ribbed.Mussel","Silt","Slippersnail","Sponges",
                                                   "Tubeworms","Tunicates"))

bethniccom_practice <- bethniccom_practice %>%
  group_by(Organism) %>%
  summarise(count = n(),
            mean = mean(PercentCover, na.rm = TRUE),
            sd = sd(PercentCover, na.rm = TRUE)
  )


library("ggpubr")
ggboxplot(bethniccom_practice, x = "Organism", y = "PercentCover", 
          color = "Organism", palette = c("#00AFBB", "#E7B800", "#FC4E07","#00AFBB", 
                                          "#E7B800", "#FC4E07","#00AFBB", "#E7B800",
                                          "#FC4E07","#00AFBB", "#E7B800", "#FC4E07",
                                          "#00AFBB", "#E7B800", "#FC4E07","#00AFBB", 
                                          "#E7B800"),
          order = c("Algae", "Amphipod.Tubes", "Anemones","Arb..Bryo.",
                    "Bare.Shell","Barnacles","Biofilm","Enc..Bryo.",
                    "Feather.Duster.Worm","Hydroids","Oysters",
                    "Ribbed.Mussel","Silt","Slippersnail","Sponges",
                    "Tubeworms","Tunicates"),
          ylab = "PercentCover", xlab = "Treatment")

ggline(bethniccom_practice, x = "Organism", y = "PercentCover", 
       add = c("mean_se", "jitter"), 
       order = c("Algae", "Amphipod.Tubes", "Anemones","Arb..Bryo.",
                 "Bare.Shell","Barnacles","Biofilm","Enc..Bryo.",
                 "Feather.Duster.Worm","Hydroids","Oysters",
                 "Ribbed.Mussel","Silt","Slippersnail","Sponges",
                 "Tubeworms","Tunicates"),
       ylab = "PercentCover", xlab = "Organism") + coord_flip()  

# Compute the analysis of variance
res.aov <- aov(PercentCover ~ Organism, data = bethniccom_practice)
# Summary of the analysis
summary(res.aov)

TukeyHSD(res.aov)

summary(glht(res.aov, linfct = mcp(group = "Tukey")))

#glht(model, lincft)

pairwise.t.test(bethniccom_practice$PercentCover, bethniccom_practice$Organism,
                p.adjust.method = "BH")

# 1. Homogeneity of variances
plot(res.aov, 1)
leveneTest(PercentCover ~ Organism, data = bethniccom_practice)

#Pairwise t-tests with no assumption of equal variances
pairwise.t.test(bethniccom_practice$PercentCover, bethniccom_practice$Organism,
                p.adjust.method = "BH", pool.sd = FALSE)

#Normality plot of residuals
# 2. Normality
plot(res.aov, 2)
plot(res.aov$residuals)
qqnorm(res.aov$residuals)
qqline(res.aov$residuals)

# Extract the residuals
aov_residuals <- residuals(object = res.aov )
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals )

# Extract the residuals
aov_residuals <- residuals(object = res.aov )

qqnorm(aov_residuals)
qqline(aov_residuals)

# Run Shapiro-Wilk test
#length(aov_residuals)
#shapiro.test(aov_residuals )

#Non-parametric alternative to test of Means
kruskal.test(PercentCover ~ Organism, data = bethniccom_practice)

attach(bethniccom_practice)
pairwise.wilcox.test()

pairwise.wilcox.test(PercentCover, Organism, p.adjust.method = "BH")
