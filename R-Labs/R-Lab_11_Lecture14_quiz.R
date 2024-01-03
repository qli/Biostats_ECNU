
face <- read.csv("R-Labs/chap17q01FacesAndPenalties.csv")

cor.test(~ PenaltyMinutes + FaceWidthHeightRatio, 
         data = face)
cor.test(face$PenaltyMinutes,face$FaceWidthHeightRatio)

cor(face$PenaltyMinutes, face$FaceWidthHeightRatio)
cor(face$FaceWidthHeightRatio,face$PenaltyMinutes)

lm(Y~X)
m1 = lm(face$PenaltyMinutes ~ face$FaceWidthHeightRatio)
m2 = lm(PenaltyMinutes ~ FaceWidthHeightRatio, data=face)
summary(m1)
summary(m2)
anova(m2)

plot(face$PenaltyMinutes ~ face$FaceWidthHeightRatio)
plot(PenaltyMinutes ~ FaceWidthHeightRatio, data=face)

library(ggplot2)
ggplot(face, aes(FaceWidthHeightRatio, PenaltyMinutes)) + 
    geom_smooth(method = "lm", se = T, col = "black") +
    geom_point(size = 3, col = "firebrick") + 
    #scale_x_continuous(limits = c(0,2.5)) +
    labs(x = "Face Width-Height Ratio",
         y = "Penalty Minutes") + 
    theme_classic()

# linear regression
faceRegression <- lm(PenaltyMinutes ~ FaceWidthHeightRatio, 
                     data = face)
summary(faceRegression)
anova(faceRegression)


# Residual Mean Squares
sum(residuals(faceRegression)^2)/19

# Residual standard error
sqrt(sum(residuals(faceRegression)^2)/19)
