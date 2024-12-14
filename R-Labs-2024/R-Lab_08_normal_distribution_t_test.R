### R Lab 07 t-test
### Qin Li 20241120

# https://whitlockschluter3e.zoology.ubc.ca/RExamples/Rcode_Chapter_10.html
# ref: https://whitlockschluter3e.zoology.ubc.ca/RExamples/Rcode_Chapter_11.html

library(ggplot2)

#--------------------------------------
# generate a normal distribution: rnorm(n, mean=0, sd=1)
set.seed(123)
x_value = rnorm(n = 100, mean = 0, sd = 1)
x_value
hist(x_value)

# calculate the probability: Pr[X < q]
pnorm(q, mean=mu, sd=sigma)
pnorm(0, mean=0, sd=1) # calculate the probability
pnorm(-1.96, mean=0, sd=1)
1 - pnorm(1.96, mean=0, sd=1)

#Pr[-1.96 < Z < 1.96]
pnorm(1.96, mean=0, sd=1) - pnorm(-1.96, mean=0, sd=1)

# calculate Pr[Y_bar > 3370] (newborn body weight)
# 1- Pr[Y_bar < 3370]
1 - pnorm(q = 3370, mean = 3339, sd = 573/sqrt(80))
1 - pnorm(q = 3370, mean = 3339, sd = 64.1)

#--------------------------------------------------------------------------
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


#-----------------------------------------------------------
# t-test: Example 11.3. Body temperature
# Uses a one-sample t-test to compare body temperature in a random sample of people with the “expected” temperature 98.6 °F.

# Read and inspect data
# option 1: read from the website
# heat <- read.csv(url("https://whitlockschluter3e.zoology.ubc.ca/Data/chapter11/chap11e3Temperature.csv"), stringsAsFactors = FALSE)
# option 2: read local file
heat <- read.csv("R-Labs/chap11e3Temperature.csv")
head(heat)

# Histogram
hist(heat$temperature)
hist(heat$temperature, right = FALSE, 
     breaks = seq(97, 100.5, by = 0.5),
     col = "firebrick", las = 1, 
     xlab = "Body temperature (°F)", 
     ylab = "Frequency", main = "")

# or use ggplot
ggplot(heat, aes(x = temperature)) + 
    geom_histogram(fill = "firebrick", col = "black", binwidth = 0.5, 
                   boundary = 97, closed = "left") + 
    xlim(97, 100.5) +
    labs(x = "Body temperature (°F)", y = "Frequency") + 
    theme_classic()

# One-sample t-test
# Calculate using the R command t.test().
# Use the argument mu = to set the parameter value stated in the null hypothesis (true mean in the null hypothesis).

t.test(x = heat$temperature, mu = 98.6)
t = (mean(heat$temperature)-98.6)/(sd(heat$temperature)/sqrt(25))

temp_test = t.test(heat$temperature, mu = 98.6)
temp_test$p.value
temp_test$estimate
temp_test$conf.int
temp_test$statistic

# One Sample t-test
# 
# data:  heat$temperature
# t = -0.56065, df = 24, p-value = 0.5802
# alternative hypothesis: true mean is not equal to 98.6
# 95 percent confidence interval:
#     98.24422 98.80378
# sample estimates:
#     mean of x 
# 98.524 

#--------------------------------------------------------------------------
# t-test: Example 11.3. Body temperature with a larger sample n = 130
set.seed(123)
temp_new = rnorm(n = 130, mean = 98.25, sd = 0.733)
hist(temp_new)
mean(temp_new) # some difference
sd(temp_new) # some difference

t.test(temp_new, mu = 98.6)

# One Sample t-test
# 
# data:  heat$temperature
# t = -0.56065, df = 24, p-value = 0.5802
# alternative hypothesis: true mean is not equal to 98.6
# 95 percent confidence interval:
#     98.24422 98.80378
# sample estimates:
#     mean of x 
# 98.524


#--------------------------------------
# check the assumption of t-test: if the data matches a normal distribution
# (1) hist()
# (2) QQ plot
qqnorm(heat$temperature)
qqline(heat$temperature)

qqnorm(temp_new)
qqline(temp_new)

#--------------------------------------
# Calculate the probability from a normal distribution
# Example 10.4. Small step for man?
# The command pnorm(Y) gives the probability of obtaining a value less than Y under the normal distribution. 

# (1) men
# (1-1) Too short to be an astronaut: Pr[Height < 157.5]
pTooShort <- pnorm(157.5, mean = 177.6, sd = 9.7)
pTooShort

# (1-2) Too tall to be an astronaut: Pr[Height > 190.5]
pTooTall <- 1 - pnorm(190.5, mean = 177.6, sd = 9.7)
pTooTall

# the sum of too short and too tall for men: Pr[Height < 157.5 or Height > 190.5]
pTooShort + pTooTall
## [1] 0.1109012

# (2) women
# different mean and SD for the normal distribution of women height

# (1-1) Too short to be an astronaut: Pr[Height < 157.5]
pTooShort <- pnorm(157.5, mean = 163.2, sd = 10.1)

# (1-2) Too tall to be an astronaut: Pr[Height > 190.5]
pTooTall <- 1 - pnorm(190.5, mean = 163.2, sd = 10.1)

# sum for women
pTooShort + pTooTall
## [1] 0.2896919


women_height = rnorm(n = 1000, mean = 163.2, sd = 10.1)
men_height = rnorm(n = 1000, mean = 177.6, sd = 9.7)
height_df = data.frame(women_height = women_height,
                       men_height = men_height)

# plot women height
ggplot(height_df, aes(x = women_height)) + 
    geom_histogram(fill = "firebrick", col = "black") + 
    labs(x = "Height (cm)", y = "Frequency") + 
    theme_classic()

# add men height
ggplot(height_df, aes(x = women_height)) + 
    geom_histogram(fill = "firebrick", col = "black") + 
    geom_histogram(aes(x = men_height), fill = "gray", col = "black", alpha=0.5) + 
    labs(x = "Height (cm)", y = "Frequency") + 
    theme_classic()
