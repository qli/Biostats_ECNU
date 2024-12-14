
data_tiger = read.csv("R-Labs/chap02e2aDeathsFromTigers.csv")

"/Users/qinli/Mirror/Teaching/Biostats_ECNU/R-Labs/chap02e2aDeathsFromTigers.csv"

getwd()

dt2 = read.csv(file.choose())

setwd("/Users/qinli/Mirror/Teaching/Biostats_ECNU")
data_tiger = read.csv("R-Labs/chap02e2aDeathsFromTigers.csv")

setwd("~/Mirror/Teaching/Biostats_ECNU/R-Labs")
dt3 = read.csv("chap02e2aDeathsFromTigers.csv")

# PLOT
data_tiger = read.csv("R-Labs/chap02e2aDeathsFromTigers.csv")

sort(table(data_tiger$activity), decreasing = T)

tiger_number = as.data.frame(t(t(sort(table(data_tiger$activity), decreasing = T))))

install.packages("ggplot2")
library(ggplot2)

tiger_plot = ggplot(tiger_number, aes(x=Var1, y=Freq)) +
    geom_bar(stat = "identity", fill="red") +
    ylab("Frequency (number of people)") +
    xlab("") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

tiger_plot
