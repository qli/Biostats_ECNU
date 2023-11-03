### R Lab 04
### additional basics in R

getwd()
setwd()

# another method
# read files
data = read.csv(file.choose()) # after run this step, you can click the file in the window
head(data)

# another method
# create project
#new project --> existing directory (choose the directory where you put code & data files)
getwd()

# read the data file for Assignment 1
finch = read.csv("Assignments/chap03q04KenyaFinches.csv")
head(finch)

install.packages("tidyverse")
library(tidyverse)

# data manipulation
# tidyverse核心: 管道符号 %>% (press: ctrl + shift + M)
library(dplyr) # if error when using select()

head(finch)
finch %>% head()
finch %>% glimpse()
finch %>% str()

finch %>% head() %>% str()
str(head(finch))

# choose certain columns by name 选择列: select
# old methods
head(finch[,c(1:2)])
head(finch[,c("species","mass")])

# with tidyverse
finch %>% select(species,mass) %>% head()
finch_sub = finch %>% select(species,mass)

finch %>% select(1,2) %>% head()
finch %>% select(starts_with("s")) %>% str()
finch %>% select(starts_with("s")) %>% pull()

finch[,"species"] %>% str()

finch %>% select(starts_with("s")|ends_with("s")) %>% head()

# choose certain rows: filter
finch %>% filter(mass > 20) %>% glimpse() # large individuals
finch %>% filter(mass > 40 | mass < 8) %>% glimpse() # extremes individuals


# calculation by group (for each species)
sp_no = finch %>% group_by(species) %>% 
    summarise(sp_count = n())

#library(dplyr)
finch_stat = finch %>% 
    group_by(species) %>% 
    summarise(sp_count = n(),
              mass_mean = mean(mass),
              mass_sd = sd(mass),
              mass_cv = sd(mass)/mean(mass)*100,
              mass_median = median(mass))

finch_stat = finch_stat %>% 
    mutate(mass_se = mass_sd/sqrt(sp_count))

finch_stat$mass_se = finch_stat$mass_sd/sqrt(finch_stat$sp_count)
    
finch %>% group_by(species) %>% 
    summarise(across(c(mass,beaklength), .fns = list(mean = mean, stdev = sd)))
        
# create new columns: mutate
finch %>% mutate(beak_mass_ratio = round(beaklength/mass, 2)) %>% glimpse()

finch %>% mutate(mass_level = ifelse(mass > 20,"large","small")) %>% glimpse()


# plot
library(ggplot2)
