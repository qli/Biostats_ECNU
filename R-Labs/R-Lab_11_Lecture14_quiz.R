face <- read.csv("R-Labs/chap17q01FacesAndPenalties.csv")

cor.test(~ PenaltyMinutes + FaceWidthHeightRatio, 
         data = face)

cor(face$PenaltyMinutes, face$FaceWidthHeightRatio)

plot(face$PenaltyMinutes ~ face$FaceWidthHeightRatio)
plot(PenaltyMinutes ~ FaceWidthHeightRatio, data=face)

library(ggplot2)
ggplot(face, aes(FaceWidthHeightRatio, PenaltyMinutes)) + 
    geom_smooth(method = "lm", se = TRUE, col = "black") +
    geom_point(size = 3, col = "firebrick") + 
    #scale_x_continuous(limits = c(0,2.5)) +
    labs(x = "Face Width-Height Ratio", y = "Penalty Minutes") + 
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
