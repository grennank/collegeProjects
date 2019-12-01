

library(pxR)

Ireland_df <- as.data.frame(read.csv("Datasets/WorldBank_IrelandData.csv"))

plot(Ireland_df)

pop_mod <- lm(Pop~Year, data= Ireland_df)

year <- 2017:2050 
predictions <- (year * pop_mod$coef[2]) + pop_mod$coef[1]

summary(pop_mod) 
data.frame(year,predictions)


pop_mod_2 <- lm(Pop~Year+GDP+GNP, data= Ireland_df)

predictions_2 <- (year * 
                    pop_mod_2$coef[2]) + pop_mod_2$coef[1]

summary(pop_mod_2) 
data.frame(year,predictions_2)
