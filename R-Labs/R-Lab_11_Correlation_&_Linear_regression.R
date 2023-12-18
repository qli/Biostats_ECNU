### R Lab 11 Correlation & Linear regression
### Qin Li 20231126

# https://whitlockschluter3e.zoology.ubc.ca/RExamples/Rcode_Chapter_16.html
# https://whitlockschluter3e.zoology.ubc.ca/RExamples/Rcode_Chapter_17.html


#------------------------------------------------------------
# Pearson's Correlation
#------------------------------------------------------------
# Example 16.1. Flipping booby
# Estimate a linear correlation between the number of non-parent adult visits experienced by boobies as chicks and the number of similar behaviors performed by the same birds when adult.

# Read and inspect data
#booby <- read.csv(url("https://whitlockschluter3e.zoology.ubc.ca/Data/chapter16/chap16e1FlippingBird.csv"), stringsAsFactors = FALSE)
booby <- read.csv("R-Labs/chap16e1FlippingBird.csv")
head(booby)

# Scatter plot
ggplot(booby, aes(nVisitsNestling, futureBehavior)) + 
    geom_point(size = 3, col = "firebrick") + 
    labs(x = "Events experienced while a nestling", y = "Future behavior") + 
    theme_classic()

# Correlation coefficient
# The cor.test function computes a number of useful quantities, which we save in the object boobyCor. The quantities can be extracted one at a time or shown all at once. We show the formula method here.

boobyCor <- cor.test(~ futureBehavior + nVisitsNestling, data = booby)
boobyCor

SE <- sqrt( (1 - r^2)/(nrow(booby) - 3) )
unname(SE)

# Confidence interval
# The 95% confidence interval for the correlation is included in the output of cor.test. It can be extracted from the boobyCor calculated in an earlier step.

boobyCor$conf.int

#------------------------------------------------------------
# Spearman rank correlation
#------------------------------------------------------------
# Example 16.5. Indian rope trick
# Spearman rank correlation between impressiveness score of the Indian rope trick and the number of years elapsed bewteen the witnessing of the trick and the telling of it in writing.

# Read and inspect data
#trick <- read.csv(url("https://whitlockschluter3e.zoology.ubc.ca/Data/chapter16/chap16e5IndianRopeTrick.csv"), stringsAsFactors = FALSE)
trick <- read.csv("R-Labs/chap16e5IndianRopeTrick.csv")
head(trick)

# Scatter plot
ggplot(trick, aes(years, impressivenessScore)) + 
    geom_point(size = 3, col = "firebrick") + 
    labs(x = "Years elapsed", y = "Impressiveness score") + 
    theme_classic()

# Spearman’s rank correlation
# The correlation is part of the output of the cor.test() function. See the next section for the result.

cor.test(~ years + impressivenessScore, data = trick, method = "spearman")

# In this example, the variable impressivenessScore is a number score with lots of tied observations. Because of the ties, R will warn you that the P-value in the output is not exact.

# library(dplyr)
# trick2 = trick %>% arrange(years) %>% 
#     mutate(year_rank = 1:nrow(trick)) %>% 
#     arrange(impressivenessScore) %>% 
#     mutate(score_rank = 1:nrow(trick)) %>% 
#     select(years, year_rank, impressivenessScore, score_rank)


#------------------------------------------------------------
# Spearman rank correlation
#------------------------------------------------------------

# Example 17.1. Lion noses
# Estimate a linear regression and calculate predicted values and their uncertainty for the relationship between lion age and proportion of black on the lion nose.

# Read and inspect data
#lion <- read.csv(url("https://whitlockschluter3e.zoology.ubc.ca/Data/chapter17/chap17e1LionNoses.csv"), stringsAsFactors = FALSE)

lion <- read.csv("R-Labs/chap17e1LionNoses.csv")
head(lion)

# Scatter plot
# This command produces the plot in Figure 17.1-1.
ggplot(lion, aes(proportionBlack, ageInYears)) + 
    geom_point(size = 3, col = "firebrick") + 
    labs(x = "Proportion black", y = "Age (years)") + 
    theme_classic()

# Fit regression line
# Fit the linear regression to the data using least squares. Use lm(), which was also used for ANOVA. Save the results into a model object, and then use other commands to extract the quantities wanted.

lionRegression <- lm(ageInYears ~ proportionBlack, data = lion)

# Slope and intercept of the best fit line, along with their standard errors, are extracted with the summary() function. Extract just the coefficients with coef(lionRegression).

summary(lionRegression)

# 95& CI for intercept and slope
confint(lionRegression)

# Add line to scatter plot
# Overlay the least squares regression line onto the plot (Figure 17.1-4) using geom_smooth().

ggplot(lion, aes(proportionBlack, ageInYears)) + 
    geom_point(size = 3, col = "firebrick") + 
    geom_smooth(method = "lm", se = FALSE, col = "black") +
    labs(x = "Proportion black", y = "Age (years)") + 
    theme_classic()

# Use line to predict
# Use the regression line for prediction. For example, here is how to predict mean lion age corresponding to a value of 0.50 of proportion black in the nose.

yhat <- predict(lionRegression, data.frame(proportionBlack = 0.50), se.fit = TRUE)
data.frame(yhat)

# 95% confidence bands
# The 95% confidence bands measure uncertainty of the predicted mean values on the regression line corresponding to each x-value (Figure 17.2-1, left). The confidence limits are the upper and lower edge of the shaded area in the following plot.
# all possible slopes in the shade area

ggplot(lion, aes(proportionBlack, ageInYears)) + 
    geom_smooth(method = "lm", se = TRUE, col = "black") +
    geom_point(size = 3, col = "firebrick") + 
    labs(x = "Proportion black", y = "Age (years)") + 
    theme_classic()

# ANOVA test
# This method produces the ANOVA table
anova(lionRegression)

# R2
# The R2 associated with the regression is part of the output of the summary command.
summary(lionRegression)$r.squared


# Residual plots
plot(lionRegression, which = 1)
# plot(lionRegression)

lion$residuals <- residuals(lionRegression)
ggplot(lion, aes(ageInYears, residuals)) + 
    geom_point(size = 3, col = "firebrick") + 
    geom_hline(yintercept=0, color = "black") +
    labs(x = "Proportion black", y = "Residuals") + 
    theme_classic()

# see Figure 17.5-4 Residual plots for a better fitted model