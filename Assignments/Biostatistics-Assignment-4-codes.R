# Codes for Assignment 4
# Qin Li 20231212

elev_shift = read.csv("chap11q27birdElevation.csv") # correct the path to the file
mean(elev_shift$changeExtent) # 2.142857
# test
t.test(elev_shift$changeExtent)
t.test(elev_shift$changeExtent)$conf.int

# life_span = data.frame(exercise = c(261, 293, 316, 319, 324, 347),
#                        no_exercise = c(240, 261, 271, 275, 276, 281))
# 
# life_span_df = data.frame(lifeSpan = c(life_span$exercise, life_span$no_exercise),
#                           treatment = rep(c("exercise","no_exercise"),each=6))
# write.csv(life_span_df, file="Assignments/chap12q28lifeSpan.csv",row.names = F, quote = F)


life_span_df = read.csv("chap12q28lifeSpan.csv") # correct the path to the file
# for calculate mean: method 1
library(dplyr)
mean_data = life_span_df %>% group_by(treatment) %>% 
    summarise(mean_lifespan = mean(lifeSpan))
# or method 2
mean(life_span_df$lifeSpan[life_span_df$treatment=="exercise"])
mean(life_span_df$lifeSpan[life_span_df$treatment=="no_exercise"])

# two-sample t-test
t.test(lifeSpan ~ treatment, data = life_span_df, var.equal = TRUE)
t.test(lifeSpan ~ treatment, data = life_span_df, var.equal = TRUE)$conf.int # not necessary
# calculate SE
s1 = sd(life_span_df$lifeSpan[life_span_df$treatment=="exercise"])
s2 = sd(life_span_df$lifeSpan[life_span_df$treatment=="no_exercise"])
n1 = 6; df1 = n1 - 1
n2 = 6; df2 = n2 - 1
SE = sqrt(((df1*s1^2 + df2*s2^2)/(df1+df2))*(1/n1 + 1/n2))
SE # 13.43186

# Q3
math_score = read.csv("chap15q24MathScore.csv")

math_score$areaClassification = factor(math_score$areaClassification,
                                       levels = c("MST","M","HS","O"))
math_score %>% group_by(areaClassification) %>% 
    summarise(mean = mean(ratingAdvantageOfManipulatedAbstractOverTheOther),
              sd = sd(ratingAdvantageOfManipulatedAbstractOverTheOther),
              n=n())

math_effect = math_score %>% 
    select(areaClassification,ratingAdvantageOfManipulatedAbstractOverTheOther) %>% 
    rename(major = 1, scoreDifference = 2)


write.csv(math_effect, file="Assignments/chap15q24MathScore.csv",row.names = F, quote = F)

math_effect = read.csv("chap15q24MathScore.csv")
math_effect$major = factor(math_score$major,
                           levels = c("MST","M","HS","O"))
mathAnova = lm(scoreDifference ~ major, data = math_effect)
anova(mathAnova)

mathAnova2 = lm(scoreDifference ~ major, data = math_effect2)
anova(mathAnova)
