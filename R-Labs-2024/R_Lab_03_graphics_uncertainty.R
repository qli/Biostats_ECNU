# Graphics 20240928

#install.packages("ggplot2")
library(ggplot2)

# 1. bar graph
data_tiger = read.csv("R-Labs/chap02e2aDeathsFromTigers.csv")

#element selection
head(data_tiger)
data_tiger[1,]
data_tiger[,2]
data_tiger[3:5,2]

# Frequency table
sort(table(data_tiger$activity), decreasing = T)
tiger_number = data.frame(sort(table(data_tiger$activity), decreasing = T))

tiger_plot = ggplot(tiger_number, aes(x=Var1, y=Freq)) +
    geom_bar(stat = "identity", fill="firebrick") +
    ylab("Frequency (number of people)") +
    xlab("") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

tiger_plot

tigerTable <- table(data_tiger$activity)
data_tiger$activity_ordered <- factor(data_tiger$activity, 
                                     levels = names(sort(tigerTable, decreasing = TRUE)) )
data.frame(table(data_tiger$activity_ordered), row.names = 1)

ggplot(data = data_tiger, aes(x = activity_ordered)) + 
    geom_bar(stat = "count", fill = "firebrick") +
    labs(x = "Activity", y = "Frequency") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

# (2) Education expenses
educationSpending <- read.csv("R-Labs/chap02f1_3EducationSpending.csv")
head(educationSpending)

ggplot(data = educationSpending, aes(x = as.character(year), 
                                     y = spendingPerStudent)) + 
    geom_bar(stat = "identity", fill = "firebrick") +
    ylab("Education spending ($ per student)") +
    xlab("") +
    theme_classic()

# 2. histgram
bird_data <- read.csv("R-Labs/chap02e2bDesertBirdAbundance.csv")
head(bird_data)

ggplot(data = bird_data, aes(x = abundance)) + 
    geom_histogram(fill = "firebrick", col = "black", 
                   binwidth = 50, 
                   #binwidth = 100, 
                   boundary = 0, closed = "left") + 
    labs(x = "Abundance", y = "Frequency") + 
    theme_classic()

# 3. stripchart/strip graph
locustData <- read.csv("R-Labs/chap02f1_2locustSerotonin.csv")
head(locustData)
locustData[1,]

#meanLevel <- tapply(locustData$serotoninLevel, locustData$treatmentTime,mean)

ggplot(data = locustData, aes(x = as.character(treatmentTime), 
                              y = serotoninLevel)) +
    geom_point(color = "firebrick", size = 3) +
    #geom_jitter(color = "firebrick", size = 3, width = 0.15) +
    xlab("Treatment time (hours)") + 
    ylab("Serotonin (pmoles)") + 
    theme_classic()

# 4. Grouped bar graph & Mosaic plot
birdMalariaData <- read.csv("R-Labs/chap02e3aBirdMalaria.csv")
head(birdMalariaData)

# Contingency table
birdMalariaTable <- table(birdMalariaData$response, birdMalariaData$treatment)
birdMalariaTable

ggplot(birdMalariaData, aes(x = treatment, fill = response)) + 
    geom_bar(stat = "count", 
             position = position_dodge2(preserve="single")) +
    scale_fill_manual(values=c("firebrick", "goldenrod1")) +
    labs(x = "Treatment", y = "Frequency") +
    theme_classic()

mosaicplot(t(birdMalariaTable), col = c("firebrick", "goldenrod1"), 
            sub = "Treatment", ylab = "Relative frequency", main = "")

# 5. scatter plot
guppyFatherSonData <- read.csv("R-Labs/chap02f3_3GuppyFatherSonAttractiveness.csv")
head(guppyFatherSonData)

plot(sonAttractiveness ~ fatherOrnamentation, data = guppyFatherSonData)
plot(sonAttractiveness ~ fatherOrnamentation, data = guppyFatherSonData,
     pch = 16, col = "firebrick")

ggplot(data = guppyFatherSonData, aes(x = fatherOrnamentation, 
                                      y = sonAttractiveness)) + 
    geom_point(size = 3, col = "firebrick") + 
    xlab("Father's ornamentation") + 
    ylab("Son's attractiveness") + 
    theme_classic()

# 6. multiple histograms, violin plot
hemoglobinData <- read.csv("R-Labs/chap02e3bHumanHemoglobinElevation.csv")
head(hemoglobinData)

# sample size
table(hemoglobinData$population)
unique(hemoglobinData$population)

library(ggplot2)

# R pch
ggplot(data = hemoglobinData, aes(x = population, y = hemoglobin)) +
    #geom_point(color = "firebrick", shape = 1, size = 3) +
    geom_jitter(color = "firebrick", shape = 1, size = 3, width = 0.15) +
    xlab("Male population") + 
    ylab("Hemoglobin concentration (g/dL)") + 
    theme_classic()

ggplot(data = hemoglobinData, aes(y = hemoglobin, x = population)) + 
    geom_boxplot(fill = "goldenrod1", col = "black", width = 0.25) + 
    labs(x = "Male population", y = "Hemoglobin concentration (g/dL)") + 
    theme_classic()

ggplot(data = hemoglobinData, aes(y = hemoglobin, x = population)) + 
    geom_violin(fill = "goldenrod1", col = "black") + 
    stat_summary(fun = median,  geom = "point", color = "black") +
    labs(x = "Male population", y = "Hemoglobin concentration (g/dL)") + 
    theme_classic()

ggplot(data = hemoglobinData, aes(x = hemoglobin)) + 
    geom_histogram(fill = "firebrick", binwidth = 1, col = "black", 
                   boundary = 0, closed = "left") +
    labs(x = "Hemoglobin concentration (g/dL)", y = "Frequency") + 
    theme_classic() +
    facet_wrap( ~ population, ncol = 1, scales = "free_y", strip.position = "right")


boxplot(hemoglobin ~ population, data = hemoglobinData,
        boxwex = 0.25, col = "goldenrod1",
        xlab = "xxx", ylab = "yyy",
        main = "main title")


sub_usa = hemoglobinData[which(hemoglobinData$population == "USA"), ]
table(sub_usa$population)
hist(sub_usa$hemoglobin)
mean(sub_usa$hemoglobin)
median(sub_usa$hemoglobin)
sd(sub_usa$hemoglobin)
var(sub_usa$hemoglobin)
summary(sub_usa$hemoglobin)
IQR(sub_usa$hemoglobin)
#se
n = nrow(sub_usa)
length(sub_usa$hemoglobin)
se = sd(sub_usa$hemoglobin) / sqrt(n)
# 95% CI
t.test(sub_usa$hemoglobin)$conf.int


# Add mean and errorbar (se)
ggplot(data = hemoglobinData, aes(x = population, y = hemoglobin)) +
    #geom_point(color = "firebrick", shape = 1, size = 3) +
    geom_jitter(color = "firebrick", shape = 1, alpha = 0.5,
                size = 1, width = 0.1) +
    stat_summary(fun.data = mean_se, geom = "errorbar", 
                 colour = "black", width = 0.1, 
                 position=position_nudge(x = 0.2)) +
    stat_summary(fun = mean, geom = "point", 
                 colour = "firebrick", size = 2, 
                 position=position_nudge(x = 0.2)) + 
    xlab("Male population") + 
    ylab("Hemoglobin concentration (g/dL)") + 
    theme_classic()
