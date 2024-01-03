### week 17 codes for problems
### Qin Li 20240102

# Q1 - beer drink time and glass type as treatment

straight_glass = c(11.63, 10.37, 17.89, 6.96, 20.40, 20.64, 9.26, 18.11, 10.33, 23.54)
curved_glass = c(7.46, 9.28, 8.90, 6.73, 8.25, 6.16, 13.09, 2.10, 6.37)

# make a long-type data frame (measurements as one column, treatment as one column)
beer_data = data.frame(treatment = c(rep("straight",length(straight_glass)),
                                     rep("curved",length(curved_glass))),
                       drink_time = c(straight_glass, curved_glass))
beer_data$treatment = factor(beer_data$treatment, levels=c("straight","curved"))

# plot
stripchart(drink_time ~ treatment, pch = 1,
           #main = "Drink beer with different glasses",
           ylab = "Drink time (minute)",
           xlab = "Glass type",
           method = "jitter",
           vertical = TRUE, data = beer_data)

boxplot(drink_time ~ treatment, boxwex = 0.5,
        #main = "Drink beer with different glasses",
        ylab = "Drink time (minute)",
        xlab = "Glass type",
        vertical = TRUE, data = beer_data)

library(ggplot2)
ggplot(beer_data, aes(x = drink_time)) + 
    geom_histogram(fill = "gray", col = "black", binwidth = 3, 
                   boundary = 0, closed = "left") +
    facet_wrap( ~ treatment, ncol = 1, scales = "free_y") +
    labs(x = "Glass type", y = "Drink time (minute)") + 
    theme_classic()

# t-test and results
t.test(drink_time ~ treatment, data = beer_data, var.equal = TRUE)
t.test(straight_glass, curved_glass, var.equal = TRUE)

# manually calculate intermediate steps
S2_p = (sd(straight_glass)^2*9 + sd(curved_glass)^2*8)/(9+8) # 21.984
SE_diff = sqrt(S2_p*(1/10 + 1/9)) # 2.154

t_score = (mean(straight_glass) - mean(curved_glass))/SE_diff #3.3976

# Q2 Lizard: can jaw bite force predict the territory size?

lizard_data = data.frame(bite_force = c(28.2, 33.9, 29.5, 39.8, 41.7, 44.7, 46.8, 47.9, 36.3, 35.5, 33.9),
                       territory_size = c(437, 589, 871, 977, 1288, 2138, 2455, 3548, 2692, 2042, 3020))

# linear regression model
lizard_model = lm(territory_size ~ bite_force, data = lizard_data)
summary(lizard_model)
anova(lizard_model)
summary(lizard_model)$r.square

# manually calculate intermediate steps
X_mean = mean(lizard_data$bite_force)
Y_mean = mean(lizard_data$territory_size)
sum((lizard_data$bite_force - mean(lizard_data$bite_force))^2)
sum((lizard_data$territory_size - mean(lizard_data$territory_size))^2)
sum((lizard_data$bite_force - mean(lizard_data$bite_force))*(lizard_data$territory_size - mean(lizard_data$territory_size)))

MS_residual = (11062201 - 91.56 * 41048.93)/9
SE_b = sqrt(MS_residual/448.3164)
