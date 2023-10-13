### R Lab 03 - Measuring Uncertainty
### Qin Li 20231008
getwd()
setwd("path/to/files") # 
#-----------------------------------#
### UNFINISHED Lab 02 For Monday class ###

Stickleback = read.csv(file = "~/Mirror/Teaching/Biostats_ECNU/R-Labs/chap03e3SticklebackPlates.csv")
Stickleback = read.csv(file = "chap03e3SticklebackPlates.csv")

head(Stickleback)

### Example 3 (skewed distribution)
boxplot(plates ~ genotype, data = Stickleback)

plates_Mm = Stickleback$plates[Stickleback$genotype == "Mm"]

hist(plates_Mm, breaks = 30)
mean(plates_Mm); sd(plates_Mm)
median(plates_Mm); IQR(plates_Mm)

### Example 4 Cumulative frequency distribution (skewed distribution)
range(plates_Mm)
breaks = seq(10, 70, by = 10) 
plates_Mm_cut = cut(plates_Mm, breaks, right=FALSE) 
plates_Mm_freq = table(plates_Mm_cut)
plates_cfd = c(0, cumsum(plates_Mm_freq)) 
length(plates_Mm)
plot(breaks, plates_cfd,            # plot the data 
     main="Plates Mm",  # main title 
     xlab="Numbers of lateral plates",        # x−axis label 
     ylab="Cumulative Frequency")   # y−axis label 
lines(breaks, plates_cfd)           # join the points

# Cumulative relative frequency distribution
plates_crfd = plates_cfd / max(plates_cfd)
plot(breaks, plates_crfd,            # plot the data 
     main="Plates Mm",  # main title 
     xlab="Numbers of lateral plates",        # x−axis label 
     ylab="Cumulative Relative Frequency")   # y−axis label 
lines(breaks, plates_crfd)      

### Example 5 Proportions
tigerData = read.csv("chap02e2aDeathsFromTigers.csv")
tigerTable = table(tigerData$activity)
tigerData$activity_ordered = factor(tigerData$activity, levels = new_order)

sort(tigerTable,decreasing=T)
new_order=names(sort(tigerTable,decreasing=T))

head(tigerData)
str(tigerData)

install.packages("ggplot2") # install package if you didn't do this before
library(ggplot2)
ggplot(data = tigerData, aes(x = activity_ordered)) + 
    geom_bar(stat = "count", fill = "firebrick") +
    labs(x = "Activity", y = "Frequency") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

#-----------------------------------#
# for ggplot reference
# 1) https://www.math.pku.edu.cn/teachers/lidf/docs/Rbook/html/_Rbook/ggplot2.html
# 2) https://danzhuibing.github.io/R_ggplot2.html

# Hadley Wickem的ggplot2包是R的一个作图用的扩展包， 它实现了“图形的语法”， 将一个作图任务分解为若干个子任务， 只要完成各个子任务就可以完成作图。 在作常用的图形时， 只需要两个步骤： 首先将图形所展现的数据输入到ggplot()函数中， 然后调用某个geom_xxx()函数， 指定图形类型，如散点图、曲线图、盒形图等。

# ggplot2的作图一般步骤为：

# (1) 准备数据，一般为数据框， 且一般为长表， 即每个观测时间占一行， 每个观测变量占一列。
# (2) 将数据输入到ggplot()函数中， 并指定参与作图的每个变量分别映射到哪些图形特性， 比如映射为x坐标、y坐标、颜色、形状等。 这些映射称为aesthetic mappings或aesthetics。
# (3) 选择一个合适的图形类型， 函数名以geom_开头， 如geom_point()表示散点图。 图形类型简称为geom。 将ggplot()部分与geom_xxx()部分用加号连接。 到此已经可以作图，下面的步骤是进一步的细化设定。
# (4) 设定适当的坐标系统， 如coord_cartesian(), scale_x_log10()等。 仍用加号连接。
# (5) 设定标题和图例位置等，如labs()。 仍用加号连接。

p = ggplot(data = iris, mapping = aes(x = Petal.Length, y = Petal.Width))
p # only plot area, no data
p + geom_point() # add a layer with scatter plot (point data)
p + geom_point() + geom_smooth(method = "lm") # use linear model to show their relationship
#-----------------------------------#

#-----------------------------------#
### Example: human gene lengths
# Ref: https://whitlockschluter3e.zoology.ubc.ca/RExamples/Rcode_Chapter_4.html

humanGeneLengths = read.csv("chap04e1HumanGeneLengthsLongestTranscript.csv")
head(humanGeneLengths)
# we see the data frame has four columns: gene, size, name and description

# We only want to plot the sizes of genes up to 15,000 nucleotides for this exercise. Use subset() for this purpose and put subset into a second data frame.
geneLengthsUpTo15K = subset(humanGeneLengths, size <= 15000)

# Histogram of the length of genes in the human genome (Figure 4.1-1).
hist(geneLengthsUpTo15K$size, right = FALSE, col = "firebrick", 
     breaks = seq(0, 15000, 500), main = "Gene Lengths", las = 1)

# or use ggplot
library(ggplot2)
ggplot(data = geneLengthsUpTo15K, aes(x = size)) + # deliver the data to ggplot
    geom_histogram(fill = "firebrick", 
                   col = "black", 
                   boundary = 0, closed = "left", binwidth = 500) + # tell ggplot to plot histogram & parameters
    labs(x = "Gene length (nucleotides)", y = "Frequency") + # labels for x- and y-axis
    ggtitle("Gene Lengths") + # figure title
    theme_classic() # background settings (other options: theme_bw())

# Mean and standard deviation
meanGeneLength = mean(humanGeneLengths$size)
meanGeneLength

N = nrow(humanGeneLengths)
N

varGeneLength = sum( (humanGeneLengths$size - meanGeneLength)^2 ) / N
sdGeneLength = sqrt(varGeneLength)
sdGeneLength

# Question: why didn't we use sd(humanGeneLengths$size) ???
# because this is a population, not a sample; sd() uses (n-1) as the nominator.

# Commands to put the mean and standard deviation into a table are shown here.

data.frame(Parameter = c("Mean", "Standard deviation"), 
           Value = c(meanGeneLength, sdGeneLength))

# Take a random sample of 100 genes

# Take a random sample from a known population of measurements:
set.seed(321) # to obtain the same sample everytime
geneSample100 = sample(humanGeneLengths$size, size = 100, replace = FALSE)

# Sample mean and standard deviation
mean(geneSample100)
sd(geneSample100)

# Standard error of the mean, assuming no missing values:
n = length(geneSample100)
sd(geneSample100) 
sd(geneSample100) / sqrt(n)# standard error

# Plot
hist(geneSample100[geneSample100 <= 15000], right = FALSE, col = "firebrick", 
     breaks = seq(0, 15000, 500), main = "", las = 1)

ggplot(data = data.frame(geneSample100), aes(x = geneSample100)) + 
    geom_histogram(fill = "firebrick", col = "black", 
                   boundary = 0, closed = "left", binwidth = 500) + 
    labs(x = "Gene length (nucleotides)", y = "Frequency") + 
    xlim(0,15000) +
    theme_classic()

# Show the sampling distribution
# Create a loop to repeatedly sample from a known population and calculate the mean each time:
results100 = vector() 
for(i in 1:10000){
    temporarySample = sample(humanGeneLengths$size, size = 100, 
                              replace = FALSE)
    results100[i] = mean(temporarySample)
}

# Histogram of sample means
ggplot(data = data.frame(results100), aes(x = results100)) + 
    stat_bin(aes(y = after_stat(count)/sum(after_stat(count))), 
             fill = "firebrick", col = "black",
             boundary = 0, closed = "left", binwidth = 50) +
    labs(x = "Gene length (nucleotides)", y = "Probability") + 
    theme_classic()

# Compare different sample sizes: n=20
results20 = vector()
for(i in 1:10000){
    tmpSample = sample(humanGeneLengths$size, size = 20, replace = FALSE)
    results20[i] = mean(tmpSample)
}
ggplot(data = data.frame(results20), aes(x = results20)) + 
    stat_bin(aes(y = after_stat(count)/sum(after_stat(count))), 
             fill = "firebrick", col = "black",
             boundary = 0, closed = "left", binwidth = 100) +
    labs(x = "Gene length (nucleotides)", y = "Probability") + 
    coord_cartesian(xlim = c(2000, 6000)) +
    theme_classic()

results500 = vector()
for(i in 1:10000){
    tmpSample = sample(humanGeneLengths$size, size = 500, replace = FALSE)
    results500[i] = mean(tmpSample)
}
ggplot(data = data.frame(results500), aes(x = results500)) + 
    stat_bin(aes(y = after_stat(count)/sum(after_stat(count))), 
             fill = "firebrick", col = "black",
             boundary = 0, closed = "left", binwidth = 25) +
    labs(x = "Gene length (nucleotides)", y = "Probability") + 
    coord_cartesian(xlim = c(2000, 6000)) +
    theme_classic()

#-----------------------------------#
# Standard error of the mean
data.frame("Sample size" = c(20,100,500), 
           "Standard error" = c(sd(results20), sd(results100), sd(results500)))

# The standard error of gene length for the unique sample of 100 genes is as follows
n = length(geneSample100)
se.mean = sd(geneSample100) / sqrt(n)
se.mean

#-----------------------------------#
# Approximate confidence intervals
# The commands below plot approximate 95% confidence intervals for the population mean based on 20 random samples of size n=100 (Figure 4.3-1). For the population we used the genes whose sizes were less than or equal to 15,000 nucleotides.

results <- data.frame(mean = rep(0,20), lower = rep(0,20), upper = rep(0,20))
for(i in 1:20){
    tmpSample <- sample(humanGeneLengths$size, size = 100, replace = FALSE)
    t <- t.test(tmpSample)
    results$mean[i] <- t$estimate
    results$lower[i] <- t$conf.int[1]
    results$upper[i] <- t$conf.int[2]
}

plot(c(1:20) ~ mean, data = results, pch = 16, col = "red", yaxt = "n",
     xlim = c(min(results$lower), max(results$upper)), bty = "l", 
     xlab = "Gene length (number of nucleotides)", ylab = "")
lines(c(3511.457, 3511.457), c(0,20), lty = 2)
segments(results$lower, 1:20, results$upper, 1:20)
segments(results$lower, 1:20 - 0.25, results$lower, 1:20 + 0.25)
segments(results$upper, 1:20 - 0.25, results$upper, 1:20 + 0.25)

#-----------------------------------#
# Error bars
head(iris)
ggplot(iris, aes(x=Species, Petal.Length)) +
    geom_point(color = "blue")+
    #geom_jitter(color = "blue", 
     #           size = 1, width = 0.1) +
    stat_summary(fun.data = mean_se, 
                 geom = "errorbar", 
                 width = 0.1, 
                 position=position_nudge(x = 0.2)) +
    stat_summary(fun.y = mean, 
                 geom = "point", 
                 color = "firebrick", 
                 size = 2, position=position_nudge(x = 0.2)) +
    labs(x = "species", y = "Petal.Length") + 
    theme_classic()

