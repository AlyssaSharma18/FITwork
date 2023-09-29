#Simple linear regression
x <- seq(-2,15)

#True B0=-3 True B1 = 2
y <- -3 + 2*x
plot(x,y, type = 'linear')

# epsilon has normal dist
set.seed(400)
yobs <- y + rnorm(length(y), 0,10)
points(x, yobs, col = "Blue")

mod1 <- lm(yobs~x)
result <- summary(mod1)
abline(mod1, col="Red")

# Access the estimated parameters (Statistics) By regression
mode(mod1)
names(mod1)
result <- summary(mod1)
result
