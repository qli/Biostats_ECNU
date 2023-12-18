### R Lab 08 two sample t-test
### Qin Li 20231126

# https://whitlockschluter3e.zoology.ubc.ca/RExamples/Rcode_Chapter_12.html

install.packages("car", dependencies = TRUE) # install only if necessary
library(ggplot2)
library(dplyr)
library(car)

#--------------------------------------
# 1. Paired t-test
# Example 12.2. Red-winged blackbirds

blackbird <- read.csv("R-Labs/chap12e2BlackbirdTestosterone.csv")

hist(blackbird$beforeImplant)
hist(blackbird$logBeforeImplant)
hist(blackbird$afterImplant)


# Histogram of differences
blackbird$d <- blackbird$logAfterImplant - blackbird$logBeforeImplant
head(blackbird)
hist(blackbird$d)
hist(blackbird$afterImplant - blackbird$beforeImplant)

ggplot(blackbird, aes(x = d)) + 
    geom_histogram(fill = "firebrick", col = "black", binwidth = 0.1, 
                   boundary = 0, closed = "left") + 
    labs(x = "Difference (after–before)", y = "Frequency") + 
    theme_classic()

# first, convert the wide data to long data
beforeData <- data.frame(blackbird = blackbird$blackbird, 
                         logAntibody = blackbird$logBeforeImplant,
                         time = "Before")
afterData <- data.frame(blackbird = blackbird$blackbird, 
                        logAntibody = blackbird$logAfterImplant,
                        time = "After")
blackbird2 <- rbind(beforeData, afterData)
blackbird2$time <- factor(blackbird2$time, levels = c("Before", "After"))

ggplot(blackbird2, aes(x = time, y = logAntibody)) +  
    geom_point(size = 5, col = "firebrick", alpha = 0.5) + 
    #geom_point(size = 5, aes(col = time), alpha = 0.5) + 
    geom_line(aes(group = blackbird)) +
    labs(x = "Implant treatment", y = "Antibody production rate (ln[mOD/min])") + 
    theme_classic()

# Paired t-test with one-sample test on difference
t.test(blackbird$d)
t.test(blackbird$d)$estimate

# or direct Paired t-test
t.test(blackbird$logAfterImplant, blackbird$logBeforeImplant, paired = TRUE)
## 
##  Paired t-test
## 
## data:  blackbird$logAfterImplant and blackbird$logBeforeImplant
## t = 1.2714, df = 12, p-value = 0.2277
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -0.04007695  0.15238464
## sample estimates:
## mean of the differences 
##              0.05615385

# 95% CI for mean difference
# 95% confidence intervals are part of the output of the t.test() function, viewed in isolation by adding $conf.int to the command.

t.test(blackbird$d)$conf.int
t.test(blackbird$logAfterImplant, blackbird$logBeforeImplant, 
       paired = TRUE)$conf.int

#--------------------------------------
# two independent sample t-test
# Example 12.3. Horned lizards

# Read and inspect data
lizard <- read.csv("R-Labs/chap12e3HornedLizards.csv")
head(lizard)

table(lizard$Survival)
nrow(lizard[lizard$Survival=="living",])

# Multiple histograms
# We begin by ordering the categories of the survival variable so that “living” comes before “killed” in the histogram. The plot command will deliver a warning to let you know that one case was dropped (the one lizard missing a horn length measurement).

str(lizard$Survival)
lizard$Survival <- factor(lizard$Survival, levels = c("living", "killed"))

ggplot(lizard, aes(x = squamosalHornLength)) + 
    geom_histogram(fill = "firebrick", col = "black", binwidth = 2, 
                   boundary = 0, closed = "left") +
    facet_wrap( ~ Survival, ncol = 1, scales = "free_y") +
    #facet_wrap( ~ Survival, nrow = 1, scales = "free_y") +
    labs(x = "Horn length (mm)", y = "Frequency") + 
    theme_classic()

ggplot(data=lizard[lizard$Survival=="living",], 
       aes(x = squamosalHornLength)) + 
    geom_histogram(fill = "firebrick", col = "black", binwidth = 2, 
                   boundary = 0, closed = "left") +
    geom_histogram(data=lizard[lizard$Survival=="killed",],
                   aes(x = squamosalHornLength),
                   fill = "gray", col = "black", 
                   binwidth = 2, boundary = 0, 
                   closed = "left", alpha=0.5) +
    labs(x = "Horn length (mm)", y = "Frequency") + 
    theme_classic()


# scatter plot

ggplot(lizard, aes(x = Survival, y = squamosalHornLength)) + 
    geom_point(color = "firebrick", size = 3, shape = 1) +
    stat_summary(fun.data = mean_se, geom = "errorbar", 
                 colour = "black", width = 0.05, 
                 position=position_nudge(x = 0.15)) +
    stat_summary(fun.y = mean, geom = "point", 
                 colour = "firebrick", size = 3, 
                 position=position_nudge(x = 0.15)) +
    labs(x = "Feeding status", y = "Horn length (mm)") + 
    theme_classic()

# Two-sample t-test
lizard_model <- t.test(squamosalHornLength ~ Survival, data = lizard, var.equal = TRUE)

t.test(lizard$squamosalHornLength ~ lizard$Survival, var.equal = TRUE)

# 95% CI for difference between means
t.test(squamosalHornLength ~ Survival, data = lizard, var.equal = TRUE)$conf.int

lizard_model$conf.int

## 
##  Two Sample t-test
## 
## data:  squamosalHornLength by Survival
## t = 4.3494, df = 182, p-value = 2.27e-05
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  1.253602 3.335402
## sample estimates:
## mean in group living mean in group killed 
##             24.28117             21.98667


#--------------------------------------
# Test on Normality
# Example 13.1. Marine reserves
# The normal quantile plot, Shapiro-Wilk test of normality, and the log transformation, investigating the ratio of biomass between marine reserves and non-reserve control areas.

# Read and inspect data
marine <- read.csv("R-Labs/chap13e1MarineReserve.csv")
head(marine)

# Histogram
# A histogram visualizes the frequency distribution of biomass ratio (Figure 13.1-4).

hist(marine$biomassRatio, right = FALSE, col = "firebrick", 
     las = 1, xlab = "Biomass ratio", main = "")

# ?ggplot with geom_histogram()
#ggplot(?) # try to write the code by yourself

# Normal quantile plot
# The line in the normal quantile plot of Figure 13.1-4 passes through the first and 3rd quartiles of the data.
qqnorm(marine$biomassRatio)
qqnorm(marine$biomassRatio, col = "firebrick", pch = 16, las = 1,
       datax = TRUE, xlab = "Biomass ratio")
qqline(marine$biomassRatio, datax = TRUE, lwd = 2)

# Shapiro-Wilk test
# A test of the goodness of fit of the normal distribution to data.

shapiro.test(marine$biomassRatio)
## 
##  Shapiro-Wilk normality test
## 
## data:  marine$biomassRatio
## W = 0.81751, p-value = 8.851e-05

# Natural log-transformation
marine$lnBiomassRatio <- log(marine$biomassRatio)

hist(marine$lnBiomassRatio, right = FALSE, col = "firebrick", 
     las = 1, xlab = "Log biomass ratio", main = "",
     breaks = seq(-0.25, 1.5, by = 0.25))

shapiro.test(marine$lnBiomassRatio)

qqnorm(marine$lnBiomassRatio, col = "firebrick", pch = 16, las = 1,
       datax = TRUE, xlab = "Biomass ratio")
qqline(marine$lnBiomassRatio, datax = TRUE, lwd = 2)
