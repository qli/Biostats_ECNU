
dt = read.csv("path to file")
dt_mean = mean(dt$numberOfBeetles)
dt_sd = sd(dt$numberOfBeetles)
dt_se = dt_sd/sqrt(nrow(dt))

CI_upper = dt_mean + 2*dt_se
CI_lower = dt_mean - 2*dt_se


