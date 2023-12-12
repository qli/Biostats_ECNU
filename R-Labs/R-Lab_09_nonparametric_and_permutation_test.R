### R-lab 09 nonparametric and permutation test
### Qin Li 20231204

# ref: https://whitlockschluter3e.zoology.ubc.ca/RExamples/Rcode_Chapter_13.html

library(ggplot2)
library(dplyr)
#--------------------------------------
# Example 13.4. Sexual conflict
# The sign test, comparing the numbers of species in 25 pairs of closely related insect taxa.

conflict <- read.csv("R-Labs/chap13e4SexualConflict.csv")
head(conflict)

hist(conflict$difference[conflict$difference<20000])
shapiro.test(conflict$difference[conflict$difference<20000])
shapiro.test(conflict$difference)

hist(conflict$difference, right = FALSE, col = "firebrick",
     breaks = 50, xlab = "Difference in species number", las = 1)

# Count up the frequency of differences that are below, equal to, and above zero.
conflictZero <- cut(conflict$difference, breaks = c(-Inf, 0, 1, Inf), 
                    labels = c("below","equal","above"), right = FALSE)

conflict$sign = ifelse(conflict$difference>0,"+","-")

table(conflictZero)
table(conflict$sign)

# Sign test
# The sign test is just a binomial test. The output includes a confidence interval for the proportion using the Clopper-Pearson method, which isn’t covered in the book.
binom.test(7, n = 25, p = 0.5)
binom.test(18, n = 25, p = 0.5)


#--------------------------------------
# Example 13.5. Cricket cannibalism
# The Wilcoxon rank-sum test (equivalent to the Mann-Whitney U*-test) comparing times to mating (in hours) of starved and fed female sagebrush crickets.
# We also apply the permutation test to the same data.

cannibalism <- read.csv("R-Labs/chap13e5SagebrushCrickets.csv")
head(cannibalism)

# Multiple histograms
str(cannibalism$feedingStatus)
cannibalism$feedingStatus <- factor(cannibalism$feedingStatus, 
                                    levels = c("starved", "fed"))
str(cannibalism$feedingStatus)

ggplot(cannibalism, aes(x = timeToMating)) + 
    geom_histogram(fill = "firebrick", col = "black", binwidth = 20, 
                   boundary = 0, closed = "left") +
    facet_wrap( ~ feedingStatus, ncol = 1, scales = "free_y") +
    #facet_wrap( ~ feedingStatus, ncol = 1) +
    labs(x = "Time to mating (hours)", y = "Frequency") + 
    theme_classic()

# Wilcoxon rank-sum test
# This test, available in R, is equivalent to the Mann-Whitney U-test.
wilcox.test(timeToMating ~ feedingStatus, data = cannibalism)

#--------------------------------------
# Permutation test
# Permutation test of the difference between mean time to mating of starved and fed crickets.
cannibalism %>% group_by(feedingStatus) %>% 
    summarise(time_mean = mean(timeToMating))

cricketMeans <- summarize(group_by(cannibalism, feedingStatus), 
                          timeToMating = mean(timeToMating, na.rm = TRUE))
cricketMeans <- as.data.frame(cricketMeans)
cricketMeans

diffMeans <- cricketMeans$timeToMating[1] - cricketMeans$timeToMating[2]
diffMeans

# Number of permutations: We use 10K here.
nPerm <- 1000

# Run a loop
# Create a loop to permute the data many times (the number of times determined by nperm). In the following loop, i is a counter that climbs from 1 to nPerm by 1. In each iteration, the permuted difference is saved in the object permResult.

permResult <- vector() # initializes
for(i in 1:nPerm){
    if(i%%100==0) print(i)
    # step 1: permute the times to mating
    cannibalism$permSample <- sample(cannibalism$timeToMating, replace = FALSE)
    # step 2: calculate difference betweeen means
    permSampleMean <- as.data.frame(summarize(group_by(cannibalism, feedingStatus), 
                                              permMean = mean(permSample, na.rm = TRUE)))
    permResult[i] <- permSampleMean$permMean[1] - permSampleMean$permMean[2]
}

write.csv(cannibalism[,c(1,3)], file="R-Labs/cannibalism_permutation.csv")

# Plot null distribution
# Make a histogram of the permuted differences (Figure 13.8-1).
hist(permResult, right = FALSE, breaks = 100)

# or
ggplot(data.frame(permResult), aes(permResult)) + 
    geom_histogram(fill = "white", col = "black", binwidth = 1, 
                   boundary = 0, closed = "left") + 
    labs(x = "Permuted difference in treatment means (hours)", y = "Frequency") + 
    theme_classic()

# Approximate P-value
sum(as.numeric(permResult <= diffMeans))
sum(as.numeric(permResult <= diffMeans)) / nPerm

# Finally, multiply by 2 to get the P-value for a two-sided test. The value won’t be identical to the one in the book, or to the value you obtain, because 10,000 iterations is not large enough for extreme accuracy.
2 * ( sum(as.numeric(permResult <= diffMeans)) / nPerm )

#--------------------------------------
