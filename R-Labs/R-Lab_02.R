### R Lab 02 - R introduction
### Qin Li 20230924

#-----------------------------------#
### 1. basic data types

# '=' / '<-': assign values to variables

# character 字符型
a = "a"
a <- "a"
name = "Ross"
class(name)

# numeric variable 数值型
id = 1 # integer
height = 1.70
class(height)

# logic 逻辑型
test = TRUE
test2 = FALSE

test == test2 # 判断

# vector
names = c("Ross Geller", "Rachel Green", "Monica Geller", "Joey Tribbiani", "Chandler Bing", "Phoebe Buffay")

heights = c(1.85, 1.58, 1.62, 1.70, 1.75, 1.68)

genders = c("Male","Female","Female","Male","Male","Female")

# data frame
roles = data.frame(name = names,
                   height = heights,
                   gender = genders)
str(roles)

roles = data.frame(name = names,
                   height = heights,
                   gender = genders,
                   stringsAsFactors = F)
str(roles)

# matrix
matrix1 = matrix(data = c(1:9),3,3)
matrix2 = matrix(data = c(1:9),3,3, byrow=T)

# list
list_1 = list(matrix1, matrix2)

### 2. calculations
5 + 5
5 - 6
5 * 2
5 / 2
(5 + 5)/2
5 %% 2
5^2
log10(10)
log(2.72)

### 3. read in data from files

getwd() # check the path/working directory
# setwd('the path to the data/code')
# or set by click "To Source File Location"

snake_dt = read.csv(file = "chap03e1GlidingSnakes.csv")
head(snake_dt)
plot(snake_dt$undulationRateHz)
hist(snake_dt$undulationRateHz)
hist(snake_dt$undulationRateHz, breaks = 7, xlim = range(0.8,2.2), right = F)

Stickleback = read.csv(file = "chap03e3SticklebackPlates.csv")
head(Stickleback)
hist(Stickleback$plates)
hist(Stickleback$plates[Stickleback$genotype == "MM"])
hist(Stickleback$plates[Stickleback$genotype == "Mm"])
hist(Stickleback$plates[Stickleback$genotype == "mm"])

#-----------------------------------#
### 4. Descriptive statistics

### Example 1
mean(snake_dt$undulationRateHz) # mean
median(snake_dt$undulationRateHz) # median
range(snake_dt$undulationRateHz) # the range: min ~ max

var(snake_dt$undulationRateHz) # variance

snake_mean = mean(snake_dt$undulationRateHz)
sum((snake_dt$undulationRateHz - snake_mean)^2)/(8-1)

snake_sd = sd(snake_dt$undulationRateHz) # standard deviation
sqrt(sum((snake_dt$undulationRateHz - snake_mean)^2)/(8-1))

snake_cv = snake_sd/snake_mean # Coefficient of variation

summary(snake_dt$undulationRateHz)
quantile(snake_dt$undulationRateHz) # quantiles: return min, quartiles, max
quantile(snake_dt$undulationRateHz, probs = c(0.05,0.95))
IQR(snake_dt$undulationRateHz) # Interquartile range


### Example 2
# Practice problems - Page 279
# 2. Here is another sample of systolic blood pressure (in units of mmHg), this time with 101 data points. The mean is 122.73 and the standard deviation is 13.83.
bp_data = c(88, 105, 114, 117, 122, 125, 128, 133, 139, 156, 88, 107, 114, 119, 123, 125, 128, 133, 141, 92, 96, 96, 100, 107, 108, 110, 115, 115, 116, 119,120, 121, 123,123, 123, 126, 126, 126, 129, 129, 130, 133, 134, 135, 142, 142, 142,102, 102, 110, 110, 116, 117, 121, 121,123, 124, 126, 126, 131, 131, 136, 136, 143, 144, 104, 104, 105, 105, 111, 111, 112, 113, 117, 117, 117, 117, 121, 121, 121, 122, 124, 124, 124, 125, 127, 127, 128, 128, 131, 131, 131, 131, 136, 138, 138, 139, 146, 146, 147, 155)

mean(bp_data) # mean
median(bp_data) # median
range(bp_data) # the range: min ~ max
var(bp_data) # variance
sd(bp_data) # standard deviation
sd(bp_data)/mean(bp_data) # coefficient of variation
summary(bp_data)
quantile(bp_data) # quantiles: return min, quartiles, max
IQR(bp_data) # Interquartile range
quantile(bp_data, probs = c(0.05,0.95))

hist(bp_data)
boxplot(bp_data)

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
tigerTable <- table(tigerData$activity)
tigerData$activity_ordered = factor(tigerData$activity, 
                                levels = names(sort(tigerTable,decreasing=T)))

head(tigerData)
str(tigerData)

# install.packages("ggplot2")
library(ggplot2)
ggplot(data = tigerData, aes(x = activity_ordered)) + 
    geom_bar(stat = "count", fill = "firebrick") +
    labs(x = "Activity", y = "Frequency") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

# proportions %
tigerTable/nrow(tigerData)

# How to plot proportions?

