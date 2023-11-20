### R Lab 06 Contingency analysis

# ref: https://whitlockschluter3e.zoology.ubc.ca/RLabs/R_tutorial_Contingency_analysis.html

# Learning outcomes
# 1. Display associations between two categorical variables in contingency tables and mosaic plots
# 2. Calculate odds ratios and their confidence intervals
# 3. Test for the independence of two categorical variables.

#--------------------------------------
### Titanic data
#--------------------------------------
titanicData <- read.csv("R-Labs/chap09f01Titanic.csv", stringsAsFactors = TRUE)
# stringsAsFactors=TRUE which tells R to treat all columns that include non-numeric characters to be treated as factors

str(titanicData)
levels(titanicData$survival)

# We want to change the order so that the “Survived” category for survive comes first when we use R to calculate the odds ratio below.
titanicData$survival <- factor(titanicData$survival, levels = c("Survived", "Died"))
titanicData$gender <- factor(titanicData$gender, levels = c("Women", "Men"))

levels(titanicData$survival)

#--------------------------------------
# Frequency table
table(titanicData$gender)
sex_survive_table <- table(titanicData$gender, titanicData$survival)

sex_survive_table <- table(titanicData$survival,titanicData$gender)

sex_survive_table
##         
##         Survived Died
##   Men        338 1329
##   Women      316  109

# if we have a table-type already, we can make the frequency table directly by creating a data frame
sex_survive_table_direct <- data.frame(yes = c(307,142),no = c(156,708),
                                       row.names = c("female","male"))

#--------------------------------------
# Mosaic plots
mosaicplot(sex_survive_table)
# add axie labels
mosaicplot(sex_survive_table, color = c("darkred", "gold"), xlab ="Gender", ylab = "Survival")

#--------------------------------------
# Odds ratios
# One of the ways to measure the strength of the association between two categorical variables is an odds ratio.

# In R, the simplest way to estimate an odds ratio is to use the command fisher.test(). This function will also perform a Fisher’s exact test (more on that later). The input to this function is a contingency table like the one we calculated above. We’ll use the results in a couple of ways, so let’s save the results in an object. (Here we called it sex_survive_fisher.)

sex_survive_fisher <- fisher.test(sex_survive_table)
sex_survive_fisher$estimate

p1 = 316/(316+109)
p2 = 338/(338+1329)

p1/(1-p1)/(p2/(1-p2))

#This shows that the odds ratio is beyond 10—the odds of a female surviving were about ten times the odds of a male surviving. This is (female survival / female death) / (male survival / male death).

# This fisher.test() function also calculates the 95% confidence interval for the odds ratio, and assigns it to an output variable called conf.int. We can see the 95% confidence interval for the odds ratio with a command like:

sex_survive_fisher$conf.int

#--------------------------------------
# χ2 contingency test
# A χ2 contingency analysis allows us to test the null hypothesis that two categorical variables are independent of each other.
# Because this scenario appears frequently, it is one of the most common tests used in biology. However, remember that the χ2 test is an approximation, and requires that all of the expected values are greater than 1 and that at least 80% are greater than 5. When doing such a test of independence on a computer, it is probably better to use Fisher’s exact test, which doesn’t have this restriction.
# The χ2 contingency test can be done with a function we saw last week, chisq.test(). If we give a frequency table as input, this function will calculate the χ2 test for us.

# check expected frequency to see if the data meets the requred assumption
chisq.test(sex_survive_table)$expected

# it is appropriate to do a χ2 contingency test with a frequency table as input
# We’ve added the option “correct = FALSE” to tell R to not do a Yate’s correction, which can be overly conservative.
chisq.test(sex_survive_table, correct=FALSE)

chisq.test(sex_survive_table)

#--------------------------------------
# Fisher’s exact test
# Another, more exact, option for testing for the independence of two categorical variables is Fisher’s exact test. This is a test that is tedious to calculate by hand, but R can do it in a flash. This test makes no approximations and sets no minimum threshold for the sizes of the expected values.
fisher.test(sex_survive_table)

