### R Lab_01_Graphics
### Qin Li 20230919

# have a look at the iris dataset
View(iris)
head(iris) # check the first 6 rows of the data
tail(iris)
nrow(iris)
ncol(iris)
summary(iris)
    
#-----------------------------------------------#
### Plot one variable ###
#-----------------------------------------------#

# 1. Display category frequencies - Barplot
# generate a table data of the count number of each species
sp_count = table(iris$Species)
barplot(sp_count)


# 2. Display frequency distribution for numeric variable - Histogram
hist(iris$Sepal.Length)
hist(iris$Sepal.Length, breaks = 30)
hist(iris$Petal.Length)
hist(iris$Petal.Length, breaks = 30)

#-----------------------------------------------#
### Display association between two variables ###
#-----------------------------------------------#

# 1. Display association between two numerical variables - Scatter plot
plot(iris$Sepal.Length ~ iris$Sepal.Width)
plot(iris$Petal.Length ~ iris$Petal.Width)
plot(iris$Petal.Length ~ iris$Sepal.Length)
plot(Sepal.Length ~ Sepal.Width, data = iris, col = iris$Species)
plot(iris$Petal.Length ~ iris$Petal.Width, col = iris$Species)


# 2. Display association between categorical variables - Grouped bar graph
# generate another categorical variable by dividing Petal.Length into small vs. large groups
iris$Sepal.size = ifelse(iris$Sepal.Width > 3, "large","small")
other_table <- table(iris$Sepal.size, iris$Species)

# grouped barplot
barplot(other_table, col = c("darkblue", "red"), 
        xlab = "Species", ylab = "Frequency",
        legend.text = rownames(other_table),
        args.legend = list(x = "topright"),
        beside = TRUE)

# stacked/mosaic
barplot(other_table, col = c("darkblue", "red"),
        xlab = "Species", ylab = "Frequency",
        legend.text = rownames(other_table),
        args.legend = list(x = "topright"))


# 3. Display association between numerical and categorical variable
# 3.1 Strip graph
stripchart(Petal.Length ~ Species, data = iris)

stripchart(Petal.Length ~ Species, data = iris,
           pch = 1, vertical=TRUE)

stripchart(Petal.Length ~ Species, data = iris,
           pch = 1, vertical=TRUE, method = 'jitter')

stripchart(Sepal.Width ~ Species, data = iris,
           pch = 1, vertical=TRUE, method = 'jitter')

# Calculating the means
means <- sapply(levels(factor(iris$Species)), function(i) mean(iris$Sepal.Width[iris$Species == i]))

means <- sapply(levels(factor(iris$Species)), function(i) mean(iris$Sepal.Width[iris$Species == i]))
# 3.2 boxplot

boxplot(Sepal.Width ~ Species, data = iris)

stripchart(Sepal.Width ~ Species, data = iris,
           add = TRUE,
           pch = 1, vertical=TRUE, method = 'jitter')

# add mean values
# calculating the means
means <- sapply(levels(factor(iris$Species)), function(i) mean(iris$Sepal.Width[iris$Species == i]))

stripchart(Sepal.Width ~ Species, data = iris,pch = 1, vertical=TRUE, method = 'jitter')
points(means, col = "red", pch = 2, cex = 1.5, bg = 2, lwd = 2)


# 3.3 violin plot
# install.packages("vioplot") # install the package first
library("vioplot") # load the package
vioplot(Sepal.Width ~ Species, data = iris)

# 3.4 multiple histograms
hist(iris$Sepal.Width[iris$Species=="setosa"], breaks = 15, col='red', xlim=c(1.5, 4.5))
hist(iris$Sepal.Width[iris$Species=="versicolor"], breaks = 15, col='green', add=TRUE)
hist(iris$Sepal.Width[iris$Species=="virginica"], breaks = 15, col='blue', add=TRUE)

# list three figure on the same page
par(mfrow = c(3,1))
hist(iris$Sepal.Width[iris$Species=="setosa"], breaks = 15, col='red', xlim=c(1.5, 4.5))
hist(iris$Sepal.Width[iris$Species=="versicolor"], breaks = 15, col='green', xlim=c(1.5, 4.5))
hist(iris$Sepal.Width[iris$Species=="virginica"], breaks = 15, col='blue', xlim=c(1.5, 4.5))
