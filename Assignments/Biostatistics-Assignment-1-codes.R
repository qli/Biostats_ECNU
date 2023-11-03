### Codes for Assignment-1

# 2. finches in Kenya

setwd("./Assignments")

# read file
finch = read.csv("chap03q04KenyaFinches.csv")

head(finch)
#   species mass beaklength
# 1 WB.SPARW   40       10.6
# 2 WB.SPARW   43       10.8
# 3 WB.SPARW   37       10.9
# 4 WB.SPARW   38       11.3
# 5 WB.SPARW   43       10.9
# 6 WB.SPARW   33       10.1

unique(finch$species) # three species
# "WB.SPARW" "CRU.WAXB" "CUTTHROA"

# extract body mass data for each species
# for Crimson-rumped waxbill: CRU.WAXB
bodymass_CRU_WAXB = finch$mass[finch$species == "CRU.WAXB"]

# for Cutthroat finch: CUTTHROA
bodymass_CUTTHROA = finch$mass[finch$species == "CUTTHROA"]

# for White-browed sparrow: WB.SPARW
bodymass_WB_SPARW = finch$mass[finch$species == "WB.SPARW"]

# calculate the mean and keep one decimal
mean(bodymass_CRU_WAXB) # 7.529412 --> 7.5
mean(bodymass_CUTTHROA) # 15.41667 --> 15.4
mean(bodymass_WB_SPARW) # 37.9375 --> 37.9

# calculate standard deviation
sd(bodymass_CRU_WAXB) # 0.6242643 --> 0.6
sd(bodymass_CUTTHROA) # 1.240112 --> 1.2
sd(bodymass_WB_SPARW) # 3.108456 --> 3.1

# calculate CV
sd(bodymass_CRU_WAXB) / mean(bodymass_CRU_WAXB) * 100 # 8.29%
sd(bodymass_CUTTHROA) / mean(bodymass_CUTTHROA) * 100 # 8.04%
sd(bodymass_WB_SPARW) / mean(bodymass_WB_SPARW) * 100 # 8.19%

boxplot(finch$mass ~ finch$species,  boxwex = 0.2,
        xlab="Species", ylab="Body mass (g)")

stripchart(finch$mass ~ finch$species, xlab="Species", ylab="Body mass (g)",
           pch = 1, vertical=TRUE, method = 'jitter')
