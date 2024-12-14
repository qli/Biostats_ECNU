### R Lab 05 Proportion and Frequency
### Qin Li 20241104

# 1. Binomial test: binom.test()
# 2. the χ2 goodness-of-fit test: chisq.test()
# 3. Contingency analysis: chisq.test(); fisher.test()

# ref1: https://whitlockschluter3e.zoology.ubc.ca/RLabs/R_tutorial_Frequency_data.html
# ref2: https://whitlockschluter3e.zoology.ubc.ca/RExamples/Rcode_Chapter_7.html
# ref3: https://whitlockschluter3e.zoology.ubc.ca/RExamples/Rcode_Chapter_8.html

dpois(x=3, lambda = 4.21)
round(dpois(x=0:20, lambda = 4.21)*76, 2)

binom.test(x = 14, n = 18, p = 0.5)

binom.test(x = 10, n = 25, p = 0.061)

chisq.test(c(14,4), p = c(0.5,0.5))

# Binomial distribution
choose(5,3)

# Binomial probability
# The probability of getting exactly 6 left-handed flowers when n=27 and p=0.25 is
dbinom(6, size = 27, prob = 0.25)

choose(27,6) * 0.25^6 * 0.75^(27-6) # choose(n,x)


xsuccesses <- c(0:27)
probx <- dbinom(xsuccesses, size = 27, prob = 0.25) 
probTable <- data.frame(xsuccesses, probx)
probTable

# Sampling distribution of a proportion: the number of success
successes10 <- rbinom(10000, size = 10, prob = 0.25)
proportion10 <- successes10 / 10

ggplot(data = data.frame(proportion10), aes(x = proportion10)) + 
    stat_bin(aes(y=..count../sum(..count..)), fill = "firebrick", col = "black",
             boundary = 0, closed = "left", binwidth = 0.1) +
    labs(x = "Sample proportion", y = "Probability") + 
    coord_cartesian(xlim = c(0, 1)) +
    theme_classic()

# increase sample size
successes100 <- rbinom(10000, size = 100, prob = 0.25)
proportion100 <- successes100 / 100

library(ggplot2)

ggplot(data = data.frame(proportion100), aes(x = proportion100)) + 
    stat_bin(aes(y=..count../sum(..count..)), fill = "firebrick", col = "black",
             boundary = 0, closed = "left", binwidth = 0.01) +
    labs(x = "Sample proportion", y = "Probability") + 
    coord_cartesian(xlim = c(0, 1)) +
    theme_classic()


# Binomial tests
# 1. right-handed toads
binom.test(x = 14, n = 18, p = 0.5)

# 2. right-handed flowers
binom.test(x = 6, n = 27, p = 0.25)
binom.test(x = 6, n = 27, p = 0.25)$conf.int

# Example 7.2. Sex and the X
binom.test(x=10,n=25,p=0.061)

mouseGenes <- read.csv("R-Labs/chap07e2SexAndX.csv")
#mouseGenes <- read.csv(file.choose())
head(mouseGenes)
# Tabulate number of genes
table(mouseGenes$onX)
# Calculate probabilities
xsuccesses = 0:25
x_prob = dbinom(x=xsuccesses, size = 25, prob = 0.061)
data.frame(xsuccesses, round(x_prob, 4))
Pr_x10 = sum(x_prob[11:26])
Pr_x10

xsuccesses <- 0:25
probx <- dbinom(xsuccesses, size = 25, prob = 0.061)
data.frame(xsuccesses, probx)

# Calculate P-value
2 * sum(probx[xsuccesses >= 10])

# R’s binomial test and P values
# a faster method with a different way to calculate the probability of extreme results at the “other” tail
# calculate CI with the Clopper-Pearson method
binom.test(10, n = 25, p = 0.061)
# sum(probx[xsuccesses >= 10])


# Confidence interval for a proportion
install.packages("binom", dependencies = TRUE)
library(binom)
binom.confint(x = 10, n = 25, method = "ac")
# x: the number of successes
# n: the total sample size
# method: “ac” for Agresti-Coull
#method  x  n mean     lower     upper
#1 agresti-coull 10 25  0.4 0.2336047 0.5930338

#----------------------------------------
### Example 8.1. No weekend getaway
# Read and inspect the data
# Each row represents a single birth, and shows the day of the week of birth.

# read from the website
#birthDay <- read.csv(url("https://whitlockschluter3e.zoology.ubc.ca/Data/chapter08/chap08e1DayOfBirth2016.csv"), stringsAsFactors = FALSE)

# read local file
birthDay <- read.csv("R-Labs/chap08e1DayOfBirth2016.csv")
head(birthDay)
str(birthDay)

# Put the days of the week in the correct order for tables and graphs.
birthDay$day <- factor(birthDay$day, 
                       levels = c("Sunday", "Monday",
                                  "Tuesday", "Wednesday",
                                  "Thursday","Friday","Saturday"))
# Frequency table
birthDayTable <- table(birthDay$day)
addmargins(birthDayTable) # add rows for "0" and "sum"

# Bar graph
# This produces a graph like that in Figure 8.1-1.
ggplot(birthDay, aes(x = day)) + 
    geom_bar(stat="count", fill = "firebrick") +
    labs(x = "", y = "Frequency") +
    theme_classic()


# χ2 goodness-of-fit test:
chisq.test(birthDayTable, p = c(52,52,52,52,52,53,53)/366)

chisq.test(birthDayTable, p = c(52,52,52,52,52,53,53)/366)$expected

#----------------------------------------
### Example 8.4. Mass extinctions

extinctData <- read.csv(file.choose(), stringsAsFactors = FALSE)

extinctData <- read.csv("R-Labs/chap08e4MassExtinctions.csv", stringsAsFactors = FALSE)
head(extinctData)

extinctData$nExtinctCategory <- factor(extinctData$numberOfExtinctions,levels = c(0:20))                                          
extinctTable <- table(extinctData$nExtinctCategory)
data.frame(addmargins(extinctTable))

#Mean number of extinctions
#The mean number of extinctions per time block must be estimated from the data.

meanExtinctions <- mean(extinctData$numberOfExtinctions)
meanExtinctions

# Expected frequencies
# Calculate expected frequencies under the Poisson distribution using the estimated mean. Don’t forget that the Poisson distribution also includes the number-of-extinctions categories 21, 22, 23, and so on.

# dpois(): generate probabilities falling in a Poisson distribution with a preset mean
# other probability distributions: dnorm() normal distribution

# if 3 success events
dpois(x = 3, lambda = meanExtinctions)

# for all possible X values
expectedProportion <- dpois(0:20, lambda = meanExtinctions)
expectedFrequency <- expectedProportion * 76


# Show the frequency distribution in a histogram (Figure 8.4-2). Superimpose a curve of the expected frequencies.
hist(extinctData$numberOfExtinctions, right = FALSE, breaks = seq(0, 21, 1),
     xlab = "Number of extinctions", col = "firebrick", las = 1)
lines(expectedFrequency ~ c(0:20 + 0.5), lwd = 2) 


# Table of frequencies
# Make a table of observed and expected frequencies, saving results in a data frame (Table 8.4-3).

extinctFreq <- data.frame(nExtinct = 0:20, obsFreq = as.vector(extinctTable), 
                          expFreq = expectedFrequency)
extinctFreq

# The low expected frequencies will violate the assumptions of the χ2 test, so we will need to group categories. Create a new variable that groups the extinctions into fewer categories.
extinctFreq$groups <- cut(extinctFreq$nExtinct, 
                          breaks = c(0, 2:8, 21), right = FALSE,
                          labels = c("0 or 1","2","3","4","5","6","7","8 or more"))
extinctFreq

# Then sum up the observed and expected frequencies within the new categories.
obsFreqGroup <- tapply(extinctFreq$obsFreq, extinctFreq$groups, sum)

expFreqGroup <- tapply(extinctFreq$expFreq, extinctFreq$groups, sum)
data.frame(obs = obsFreqGroup, prob = c(sum(expectedProportion[1:2]),expectedProportion[3:8],sum(expectedProportion[9:21])), exp = expFreqGroup)


saveChiTest <- chisq.test(obsFreqGroup, p = expFreqGroup/76)
pValue <- 1 - pchisq(saveChiTest$statistic, df = 6)
unname(pValue)





se1 = 3.96/sqrt(77)
se2 = 3.96/sqrt(77)
se3 = 3.96/sqrt(77)
se
