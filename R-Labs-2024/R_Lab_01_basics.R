# R-Lab 20240931

# numeric
x = 2
a = 2
b = a + 2

mode(b)

# character
letter_list = "a"
mode(letter_list)

# vector
letter_list = c("a", "b", "c")
value_list = c(1,2,3,4,5)
value_list = c(1:100)

tiger_group = c("grass", "walking","sleep")
kill_number = c(43, 10, 5)

# data frame
data_frame_example = data.frame(category = tiger_group,
                                number = kill_number)

# matrix?

# list
test_list = list(tiger_group, data_frame_example)

# function
list()
c()
data.frame()

getwd()

# set working directory: an example
#setwd("/Users/qinli/Mirror/Teaching/Biostats_ECNU/R-Labs")
#file1 = read.csv("file1.csv")
#file1 = read.csv("/Users/qinli/Mirror/Teaching/Biostats_ECNU/R-Labs/file1.csv")

# generate values
test_values = rnorm(n = 100, mean = 0, sd = 1)
length(test_values)
test_values

# make plot
plot(test_values)
hist(test_values)

test_values = rnorm(n = 100, mean = 0, sd = 1)

# calculate mean, standard deviation, and variance
mean(test_values)
sd(test_values)
var(test_values)

test_values2 = c(1,2,3.6,7,9,2)
mean(test_values2)
median(test_values2)
