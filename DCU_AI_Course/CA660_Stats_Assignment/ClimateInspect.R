install.packages("Hmisc")
library(pxR)
library(Hmisc)
library(zoo)
library(data.table)
library(dplyr)
library(tidyr)
library(rowr)
rainfall <- as.data.frame(read.px("Datasets/MTM01.px"))
temperature <- as.data.frame(read.px("Datasets/MTM02.px"))
sunshine <- as.data.frame(read.px("Datasets/MTM04.px"))
wind <- as.data.frame(read.px("Datasets/MTM04.px"))
greenhouse <- as.data.frame(read.px("Datasets/EAA01.px"))

with(greenhouse[(greenhouse$Statistic == "All Greenhouse Gas Emissions (000 Tonnes CO2 equivalent)")&(greenhouse$Sector == "All Emissions") ,], plot(greenhouse$Year, greenhouse$value))

df_gh <- greenhouse[(greenhouse$Statistic == "All Greenhouse Gas Emissions (000 Tonnes CO2 equivalent)")&(greenhouse$Sector == "All Emissions") ,]
plot(df_gh)


df_rf <- rainfall[(rainfall$Statistic == "Total Rainfall (Millimetres)")&(rainfall$Meteorological.Weather.Station == "Dublin airport") ,]

plot(df_rf$Month, df_rf$value)

df_ss <- sunshine[(sunshine$Meteorological.Weather.Station == "Dublin airport") ,]

plot(df_ss$Month, df_ss$value)

df_temp <- temperature[(temperature$Statistic == "Mean Temperature (Degrees C)")&(temperature$Meteorological.Weather.Station == "Dublin airport") ,]

df_temp <-  separate(df_temp, col=Month, into = c("Year", "month"), sep="M")
df_tempv2 <- df_temp[(df_temp$Year >1959)&(df_temp$Year <2015) ,]

n=12
Avg_temps <- rollapply(df_tempv2$value, n, mean, by = n)
plot(co2,Avg_temps)
climate_fit <- lm(co2 ~ Avg_temps)
summary(climate_fit)
abline(lm(Avg_temps ~ co2))

par(mfrow = c(2,2))
plot(climate_fit)

plot(df_temp$Month, df_temp$value)

temp_fit <- lm(value~Month, data=df_temp)
summary(temp_fit)
ggplot(df_temp,aes(Month, value)) +
  stat_summary(fun.data= mean_cl_normal) + 
  geom_smooth(method='lm')
ggplot(data = df_temp,aes(Month, value)) + 
  geom_point(color='blue') +
  geom_smooth(method = "lm", se = FALSE)

gh_fit <- lm(value~Year, data=df_gh)

plot(gh_fit)


plot(value~Year, data=df_gh)

ggplot(data = df_gh,aes(Year, value)) + 
  geom_point(color='blue') +
  geom_smooth(method = "lm", se = FALSE)

Climate <- data.frame()
Climate <- data.frame(df_temp[,c(4),])
Climate %>% rename(AbgTemp = df_temp...c.4....)
names(Climate)[1] <- "AvgTemp"

n = 12
rowMeans(df_tempv2[seq(1, nrow(df_tempv2$value), n),])
df_tempv2[, mean(z), by= (seq(nrow(value)) - 1) %% n]
rollapply(df_tempv2$value[,1], 2, mean, na.pad = 1, align="right")
