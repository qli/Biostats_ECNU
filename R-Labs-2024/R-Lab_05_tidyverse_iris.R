### R Lab 05
### additional basics in R

# "iris" data frame
head(iris)
unique(iris$Species)
table(iris$Species)
str(iris)
View(iris)
boxplot(Sepal.Length ~ Species, iris)
boxplot(Petal.Length ~ Species, iris)
plot(Petal.Length ~ Petal.Width, data = iris)
plot(iris$Petal.Length ~ iris$Petal.Width)

library(ggplot2)
ggplot(iris, aes(x = Species, y = Petal.Length)) +
    #geom_point() +
    geom_jitter(width = 0.1) +
    stat_summary(fun.data = mean_se, 
                 geom = "errorbar",
                 color = "blue",
                 width = 0.1,
                 position = position_nudge(x=0.2)) +
    stat_summary(fun = mean, 
                 geom = "point",
                 color = "red",
                 position = position_nudge(x=0.2)) +
    theme_classic()




ggplot(iris, aes(x=Species, Petal.Length)) +
    #geom_point(color = "blue")+
    geom_jitter(color = "blue", size = 1, width = 0.1) +
    stat_summary(fun.data = mean_se, 
                 geom = "errorbar", 
                 width = 0.1, 
                 position=position_nudge(x = 0.3)) +
    stat_summary(fun = mean, 
                 geom = "point", 
                 color = "firebrick", 
                 size = 2, position=position_nudge(x = 0.3)) +
    labs(x = "species", y = "Petal.Length") + 
    theme_classic()

###########################################

install.packages("tidyverse")
library(tidyverse)

# data manipulation
# tidyverse核心: 管道符号 %>% (press: ctrl + shift + M)
#library(dplyr) # if error when using select()


head(iris)
iris %>% head()
iris %>% glimpse()
iris %>% str()

iris %>% head() %>% str()
str(head(iris))

# choose certain columns by name 选择列: select
# old methods
head(iris[,c(1:2)])
head(iris[,c("Species","Petal.Length")])

# with tidyverse
iris %>% select(Species, Petal.Length) %>% head()
iris_sub = iris %>% select(Species, Petal.Length)

iris %>% select(1,2) %>% head()
iris %>% select(starts_with("S")) %>% str()
iris %>% select(Species) %>% pull()

iris[,"Species"] %>% str()

iris %>% select(starts_with("S")|ends_with("S")) %>% head()

# choose certain rows: filter
iris %>% filter(Petal.Length > 4) %>% glimpse() # large individuals
iris %>% filter(Petal.Length > 4 | Petal.Length < 6) %>% glimpse() # extremes individuals

# calculation by group (for each species)
iris %>% group_by(Species) %>% 
    summarise(species_no = n(),
              petal_mean = mean(Petal.Length),
              petal_sd = sd(Petal.Length),
              petal_se = petal_sd/sqrt(species_no),
              petal_se2 = sd(Petal.Length)/sqrt(n()))



#library(dplyr)
iris_stat = iris %>% 
    group_by(Species) %>% 
    summarise(sp_count = n(),
              Petal.Length_mean = mean(Petal.Length),
              Petal.Length_sd = sd(Petal.Length),
              Petal.Length_cv = sd(Petal.Length)/mean(Petal.Length)*100,
              Petal.Length_median = median(Petal.Length))

# mean and sd
iris %>% group_by(Species) %>% 
    summarise(across(c(Petal.Length, Petal.Width), .fns = list(mean = mean, stdev = sd)))

# alternative:
iris %>% group_by(Species) %>% 
    summarise(Petal.Length_mean = mean(Petal.Length),
              Petal.Length_stdev = sd(Petal.Length),
              Petal.Length_se = Petal.Length_stdev/sqrt(n()),
              Petal.Length_ci_lower = mean(Petal.Length) - qt(0.975, df = n() - 1) * sd(Petal.Length) / sqrt(n()),
              Petal.Length_ci_upper = mean(Petal.Length) + qt(0.975, df = n() - 1) * sd(Petal.Length) / sqrt(n()))

#se = sd(xx) / sqrt(n)

# 95% CI
t.test(iris$Petal.Length[which(iris$Species == "setosa")])$conf.int

# iris_stat$Petal.Length_se = iris_stat$Petal.Length_sd/sqrt(iris_stat$sp_count)

# mutate: add a new column
iris %>% 
    mutate(Petal_level = ifelse(Petal.Length > 4,"large","small")) %>% head()

#------------------------------------------------------------------------------------
### Example 4 Cumulative frequency distribution (skewed distribution)

petal_vi = iris$Petal.Length[iris$Species == "virginica"]

hist(petal_vi, breaks = 10)
mean(petal_vi); sd(petal_vi)
median(petal_vi); IQR(petal_vi)

range(petal_vi)
breaks = seq(1, 7, by = 1) 
petal_vi_cut = cut(petal_vi, breaks, right=FALSE) 
petal_vi_freq = table(petal_vi_cut)
petal_vi_cfd = c(0, cumsum(petal_vi_freq)) 
length(petal_vi_cfd)
plot(breaks, petal_vi_cfd,            # plot the data 
     main="virginica",  # main title 
     xlab="Petal Length",        # x−axis label 
     ylab="Cumulative Frequency")   # y−axis label 
lines(breaks, petal_vi_cfd)           # join the points

# Cumulative relative frequency distribution
petal_vi_crfd = petal_vi_cfd / max(petal_vi_cfd)
plot(breaks, petal_vi_crfd,            # plot the data 
     main="virginica",  # main title 
     xlab="Petal Length",        # x−axis label 
     ylab="Cumulative Relative Frequency")   # y−axis label 
lines(breaks, petal_vi_crfd)    

# repeat the above calculations for three species
species = unique(iris$Species)

breaks = seq(1, 7, by = 1) 
petal_crfd_list = list()
 
for(i in 1:3){
    petal_df = iris$Petal.Length[iris$Species == species[i]]
    
    petal_cut = cut(petal_df, breaks, right=FALSE) 
    petal_freq = table(petal_cut)
    petal_cfd = c(0, cumsum(petal_freq)) 
    petal_crfd = petal_cfd / max(petal_cfd)
    petal_crfd_list[[i]] = petal_crfd
}

plot(breaks, petal_crfd_list[[1]],            # plot the data 
     xlab="Petal Length",                   # x−axis label 
     ylab="Cumulative Relative Frequency")  # y−axis label 
lines(breaks, petal_crfd_list[[1]])    
points(breaks, petal_crfd_list[[2]], col = "red")    
lines(breaks, petal_crfd_list[[2]], col = "red")    
points(breaks, petal_crfd_list[[3]], col = "blue")    
lines(breaks, petal_crfd_list[[3]], col = "blue")    

# How to plot this figure with ggplot() ???