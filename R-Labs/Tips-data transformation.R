dt = c("a","a","c","a",'b')
sort(dt)
dt_2 = factor(dt)
str(dt_2)

dt_3 = factor(dt, levels = c("c", "b","a"))
str(dt_3)
as.numeric(dt_3)
