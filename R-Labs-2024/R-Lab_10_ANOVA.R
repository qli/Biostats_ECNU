### R-Lab 10 ANOVA
### Qin Li 20241121

# Load required packages
library(ggplot2)
library(dplyr) # %>%

#------------------------------------------------------------------------
# Example 15.1. Knees who say night
# Analysis of variance, comparing phase shift in the circadian rhythm of melatonin production in participants given alternative light treatments. Also, the nonparametric Kruskal-Wallis test. Finally, we use the same data to demonstrate planned comparisons and unplanned comparisons (Tukey-Kramer tests).

#circadian <- read.csv(url("https://whitlockschluter3e.zoology.ubc.ca/Data/chapter15/chap15e1KneesWhoSayNight.csv"), stringsAsFactors = FALSE)

circadian <- read.csv("R-Labs/chap15e1KneesWhoSayNight.csv")

# Order groups
circadian$treatment <- factor(circadian$treatment, 
                              levels = c("control", "knee", "eyes")) 

# Statistics by treatment group
# Here is how to create a table of descriptive statistics of a variable by group (Table 15.1-1) using summarize() from the dplyr package.

circadianStats <- summarize(group_by(circadian, treatment), 
                            Ybar = mean(shift, na.rm = TRUE),
                            s = sd(shift, na.rm = TRUE), 
                            n = n())

circadian %>% group_by(treatment) %>% 
    summarise(Ybar = mean(shift, na.rm = TRUE),
              s = sd(shift, na.rm = TRUE), 
              n = n())

data.frame(circadianStats)

# Basic strip chart

ggplot(circadian, aes(x = treatment, y = shift)) +
    geom_point(color = "firebrick", size = 3, shape = 1) +
    labs(x = "Light treatment", y = "Shift in circadian rhythm (h)") + 
    theme_classic()

# Standard error bars
# Use stat_summary() to overlay mean and standard error bars. We shifted the position of the bars to eliminate overlap.

ggplot(circadian, aes(x = treatment, y = shift)) +
    geom_point(color = "firebrick", size = 3, shape = 1) +
    stat_summary(fun.data = mean_se, geom = "errorbar", 
                 colour = "black", width = 0.1, 
                 position=position_nudge(x = 0.15)) +
    stat_summary(fun.y = mean, geom = "point", 
                 colour = "firebrick", size = 3, 
                 position=position_nudge(x = 0.15)) +
    labs(x = "Light treatment", y = "Shift in circadian rhythm (h)") + 
    theme_classic()

# Fixed effects ANOVA table
# The ANOVA table is made in two steps. The first step involves fitting the ANOVA model to the data using lm() (“lm” stands for “linear model”, of which ANOVA is one type). Then we use the command anova() to assemble the table.

circadianAnova <- lm(shift ~ treatment, data = circadian)
anova(circadianAnova)

## Analysis of Variance Table
## 
## Response: shift
##           Df Sum Sq Mean Sq F value   Pr(>F)   
## treatment  2 7.2245  3.6122  7.2894 0.004472 **
## Residuals 19 9.4153  0.4955                    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# R2
# R2 indicates the fraction of variation in the response variable “explained” by treatment.
circadianAnovaSummary <- summary(circadianAnova)
circadianAnovaSummary$r.squared


#------------------------------------------------------------------------
# Planned and unplanned comparisons
# A planned comparison
# Here we’ll calculate a planned comparison. The only one we are interested in is that between the “control” and “knee” treatment groups, but emmeans() will calculate all pairs.

# First, apply the emmeans() command to the ANOVA object.
install.packages("emmeans", dependencies = TRUE) # only if not yet installed
library(emmeans)
circadianPairs <- emmeans(circadianAnova, specs = "treatment")

# To obtain the planned 95% confidence intervals for a pairwise comparison, use contrast()
circadianPlanned <- contrast(circadianPairs, method = "pairwise", adjust = "none")
circadianPlanned
confint(circadianPlanned)

# # for the control vs knee comparison alone
circadianPlanned[1,]
confint(circadianPlanned)[1,]

# Unplanned comparisons
# Here we show how to carry out Tukey-Kramer tests between all pairs of means. 
circadianPairs <- emmeans(circadianAnova, "treatment")
circadianUnplanned <- contrast(circadianPairs, method = "pairwise", adjust = "tukey")
circadianUnplanned

#------------------------------------------------------------------------
# Kruskal-Wallis test
# The Kruskal-Wallis test is a nonparametric method to compare more than two groups. The method is not needed for the circadian rhythm data, because assumptions of ANOVA are met, but we include it here to demonstrate the method. Write the formula for the command in the same way as with lm().
kruskal.test(shift ~ treatment, data = circadian)

#------------------------------------------------------------------------
# Example 15.6. Walking stick limbs
# Random effects ANOVA to estimate variance components and calculate repeatability of measurements of femur length in walking stick insects.

# Read and inspect data
# Data are in “long” format. Femur length is one column, with another variable indicating specimen identity. Each specimen was measured twice and therefore takes up two rows.

# walkingstick <- read.csv(url("https://whitlockschluter3e.zoology.ubc.ca/Data/chapter15/chap15e6WalkingStickFemurs.csv"), stringsAsFactors = FALSE)
walkingstick <- read.csv("R-Labs/chap15e6WalkingStickFemurs.csv")
head(walkingstick)

# Strip chart
# To make the strip chart in Figure 15.6-1, we first need to order the specimens by their mean measurement.

# First, calculate the mean femur length for each specimen, as well as which of its two measurements is the smallest and which the largest. Save the result in the same data frame.
walkingstick <- mutate(group_by(walkingstick, specimen), meanFemur = mean(femurLength))
head(data.frame(walkingstick))

#sum((walkingstick$meanFemur - walkingstick$femurLength)^2)/25
#sum((walkingstick$femurLength - mean(walkingstick$femurLength))^2)

# plot
walkingstick.ordered <- arrange(walkingstick, meanFemur)
#walkingstick.ordered <- walkingstick %>% 
#    arrange(desc(meanFemur))

walkingstick.ordered$indiv <- rep(1:25, rep(2, 25))

ggplot(walkingstick.ordered, aes(indiv, femurLength)) +
    stat_summary(fun.data = mean_se, geom = "linerange", 
                 colour = "black") +
    geom_point(color = "firebrick", size = 3) +
    labs(x = "Individual walking stick", y = "Femur length (cm)") + 
    theme_classic()


# Random effects ANOVA
# Fit the random effects ANOVA using lme() command from the nlme package. Individual walking stick is the random effect in the walking stick data. The example doesn’t include a fixed-effect variable, so we just provide a symbol for a constant in the formula (~ 1), representing the grand mean. The random effect is included in a second formula as 1|specimen, which indicates that each insect specimen (the variable representing individual insect) has its own true femur length represented in the formula as a constant. This notation takes some getting used to.
library(nlme)

walkingstickAnova <- lme(fixed = femurLength ~ 1, 
                         random = ~ 1|specimen, data = walkingstick)

# Variance components
# Obtain the variances and standard deviations of the random effects using VarCorr. The first row contains the variance (and standard deviation) among the specimens. This is the variability among random groups. The second row contains the variance (and standard deviation) of measurements made on the same individuals. This is the within-group variation and is labeled “Residual”.

walkingstickVarcomp <- VarCorr(walkingstickAnova)
walkingstickVarcomp

# Repeatability
# The repeatability is the proportion of total variance among measurements (within plus among individuals) that is among measurements.

varAmong  <- as.numeric( walkingstickVarcomp[1,1] )
varWithin <- as.numeric( walkingstickVarcomp[2,1] )
repeatability <- varAmong / (varAmong + varWithin)
repeatability
