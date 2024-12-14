### R Lab 05
### additional basics in R

# read the data
finch = read.csv("R-Labs/chap03q04KenyaFinches.csv")
head(finch)

# how to select elements in a data frame
finch[1,]
finch[,1]
finch[,1:5]
finch[1,1]
finch[1,1:5]
finch[1:5,1:5]

###########################################

#install.packages("tidyverse")
library(tidyverse)

# data manipulation
# tidyverse核心: 管道符号 %>% (press: ctrl + shift + M)
#library(dplyr) # if error when using select()

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

# alternative:
# finch_stat$mass_se = finch_stat$mass_sd/sqrt(finch_stat$sp_count)
 
# mean and sd
finch %>% group_by(species) %>% 
    summarise(across(c(mass,beaklength), .fns = list(mean = mean, stdev = sd)))

# alternative:
finch %>% group_by(species) %>% 
    summarise(sample_size = n(),
              mass_mean = mean(mass),
              mass_stdev = sd(mass),
              beaklength_mean = mean(beaklength),
              beaklength_stdev = sd(beaklength),
              #mass_se = mass_stdev/sqrt(n()),
              #beaklength_se = beaklength_stdev/sqrt(n())
              #mass_ci_lower = mean(mass) - qt(0.975, df = n() - 1) * sd(mass) / sqrt(n()),
              #mass+ci_upper = mean(mass) + qt(0.975, df = n() - 1) * sd(mass) / sqrt(n()))
              )

se = sd(sub_usa$hemoglobin) / sqrt(n)

# 95% CI
t.test(finch$mass[which(finch$species == "CUTTHROA")])$conf.int

# create new columns: mutate
finch %>% mutate(beak_mass_ratio = round(beaklength/mass, 2)) %>% glimpse()

finch %>% mutate(mass_level = ifelse(mass > 20,"large","small")) %>% glimpse()

# ------

Stickleback = read.csv(file = "R-Labs/chap03e3SticklebackPlates.csv")

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

# combine three genotype
plates_MM = Stickleback$plates[Stickleback$genotype == "MM"]
plates_mm = Stickleback$plates[Stickleback$genotype == "mm"]

plates_MM_cut = cut(plates_MM, breaks, right=FALSE) 
plates_MM_freq = table(plates_MM_cut)
plates_cfd_MM = c(0, cumsum(plates_MM_freq)) 
plates_crfd_MM = plates_cfd_MM / max(plates_cfd_MM)

plates_mm_cut = cut(plates_mm, breaks, right=FALSE) 
plates_mm_freq = table(plates_mm_cut)
plates_cfd_mm = c(0, cumsum(plates_mm_freq)) 
plates_crfd_mm = plates_cfd_mm / max(plates_cfd_mm)

plate_dt = data.frame(breaks = rep(breaks,3),
                      genotype = rep(c("MM","Mm","mm"), each = length(plates_crfd_mm)),
                      plates_crfd = c(plates_crfd_MM, plates_crfd, plates_crfd_mm))

ggplot(plate_dt, aes(x = breaks, y = plates_crfd, group = genotype)) +
    geom_point(aes(col = genotype), size = 3) +
    geom_path(aes(col = genotype), linewidth = 1) +
    #scale_color_manual(values = c("firebrick","steelblue","darkgray"),
    #                   labels = c("mm","Mm","MM")) +
    labs(x = "Plate number", y = "Cumulative relative frequency") +
    theme_classic()

