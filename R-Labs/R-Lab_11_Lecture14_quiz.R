face <- read.csv("R-Labs/chap17q01FacesAndPenalties.csv")
head(face)

plot(PenaltyMinutes ~ FaceWidthHeightRatio, data = face)

cor.test(~ PenaltyMinutes + FaceWidthHeightRatio, data = face)
cor(face$PenaltyMinutes, face$FaceWidthHeightRatio)

faceRegression <- lm(PenaltyMinutes ~ FaceWidthHeightRatio, data = face)
summary(faceRegression)
