x_value = rnorm(n = 1000, mean = 10, sd = 1)
x_value
mean(x_value)
hist(x_value, breaks = 30)
t.test(x_value, mu = 10)

# Pr[Z < q]
Pr_less_than_1_96 = pnorm(1.96, mean = 0, sd = 1)
Pr_greater_than_1_96 = 1-pnorm(1.96, mean = 0, sd = 1)
Pr_less_than_minus_1_96 = pnorm(-1.96, mean = 0, sd = 1)

1-pnorm((3370-3339)/(573/sqrt(80)), mean = 0, sd = 1)
1-pnorm(0.48)

# temperature
heat <- read.csv("Data/chapter11/chap11e3Temperature.csv")
head(heat)
hist(heat$temperature)
str(heat)
mean(heat$temperature)

t.test(x = heat$temperature, mu = 98.6) # right or wrong???
#pnorm(-0.56065) + (1 - pnorm(0.56065)) # 0.5750362

# calculate t value
Y_bar = mean(heat$temperature)
mu0 = 98.6
SE_y = sd(heat$temperature)/sqrt(nrow(heat))
t_value = (Y_bar - mu0) / SE_y
t_value

t.test(x = heat$temperature, mu = 98.6)$conf.int

Y_bar = 98.25
mu0 = 98.6
SE_y = 0.733/sqrt(130)
t_value = (Y_bar - mu0) / SE_y
t_value

#-------------------------------------

# plot t distribution
curve(dt(x, df=1000), from=-4, to=4, col='darkgray')
curve(dt(x, df=10), from=-4, to=4, col='red', add=TRUE)
curve(dt(x, df=5), from=-4, to=4, col='blue', add=TRUE) 
abline(v = 1.96)
abline(v = -1.96)

# plot z distribution
x <- seq(-4, 4, length=100)

#create a vector of values that shows the height of the probability distribution
#for each value in x
y <- dnorm(x)

#plot x and y as a scatterplot with connected lines (type = "l") and add
#an x-axis with custom labels
plot(x,y, type = "l", lwd = 2, axes = FALSE, xlab = "", ylab = "")
axis(1, at = -3:3, labels = c("-3s", "-2s", "-1s", "mean", "1s", "2s", "3s"))
abline(v = 1.96)

# add t distribution
curve(dt(x, df=1000), from=-4, to=4, col='red', add=T)
curve(dt(x, df=5), from=-4, to=4, col='blue', add=T)

#---------------------------------------
t.test(extra ~ group, data = sleep)

blackbird <- read.csv("Data/chapter12/chap12e2BlackbirdTestosterone.csv")

blackbird$diff = blackbird$logAfterImplant -
  blackbird$logBeforeImplant
hist(blackbird$diff)

t.test(blackbird$diff)$conf.int
t.test(blackbird$diff)

t.test(blackbird$logAfterImplant,
       blackbird$logBeforeImplant,
       paired = TRUE)

#----------
# two sample t test
lizard <- read.csv("Data/chapter12/chap12e3HornedLizards.csv")
head(lizard)

#install.packages("ggplot2")
library(ggplot2)
ggplot(lizard, aes(x = squamosalHornLength )) +
  geom_histogram() +
  facet_wrap( ~ Survival, ncol=1, scales="free_y") +
  theme_classic()

t.test(extra ~ group, data = sleep)

# t.test(数量数值 ~ 分组信息, data = 数据)
t.test(? ~ ?, data = lizard)
t.test(squamosalHornLength ~ Survival, 
       data = lizard,
       var.equal = TRUE)

t.test(lizard$squamosalHornLength ~ lizard$Survival, 
       var.equal = TRUE)

var.test(squamosalHornLength ~ Survival, data = lizard)
