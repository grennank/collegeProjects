install.packages("ggplot2")
library(pxR)
library(ggplot2)
library(dplyr)

setwd("~/Library/Mobile Documents/com~apple~CloudDocs/GCAI/CA660 Statistical Data Analysis/Assignment")

migration <- as.data.frame(read.px("Datasets/PEA03.px"))
migration
population <- as.data.frame(read.px("Datasets/PEA01.px"))
population
years <- seq(1983,2014,1)
years
rates <- c(14.00 , 15.60 , 16.90 , 17.00 , 16.80 , 16.10 , 14.60 , 13.20 , 14.60 , 15.20 , 15.50 , 14.00 , 12.10, 11.50 , 10.30 ,  7.40 ,  5.50 ,  4.30 ,  3.90 ,  4.40 ,  4.60 ,  4.50 ,  4.40 ,  4.50 ,  4.70 ,  6.40 ,  12.00 ,  13.80 ,  14.60 , 14.70 , 13.10 ,  11.20)
rates
unemployment <- data.frame(years, rates)
unemployment

my.population <- population[(population$Age.Group=="All ages")&(population$Sex=="Both sexes")&(as.numeric(as.character(population$Year)) > 1982)&(as.numeric(as.character(population$Year)) < 2015),]
my.population

my.population65 <- population[(population$Age.Group=="65 years and over")&(population$Sex=="Both sexes")&(as.numeric(as.character(population$Year)) > 1982)&(as.numeric(as.character(population$Year)) < 2015),]
my.population65


my.population15 <- population[(population$Age.Group=="15 years and over")&(population$Sex=="Both sexes")&(as.numeric(as.character(population$Year)) > 1982)&(as.numeric(as.character(population$Year)) < 2015),]
my.population15

my.population$Age65 <- my.population65$value
my.population$Age15 <- my.population15$value
my.population$value.Working <- my.population$Age15- my.population$Age65

unemployment$Population <- my.population$value
unemployment$Population.labour <- my.population$value.Working
unemployment$Total.Labour <- unemployment$Population.labour*unemployment$rates*10

#plot(x=unemployment$years, y= unemployment$migration)
ggplot(data=unemployment, aes(x=unemployment$years, y= unemployment$Population)) +
  geom_line()
ggplot(data=unemployment, aes(x=unemployment$years, y= unemployment$rates)) +
  geom_line()

my.migration <- migration[(migration$Age.Group=="All ages")&(migration$Sex=="Both sexes")&(migration$Inward.or.Outward.Flow=="Immigrants: All origins")&(as.numeric(as.character(migration$Year)) > 1986)&(as.numeric(as.character(migration$Year)) < 2015),]
my.migration$Year.num <- as.numeric(as.character(my.migration$Year))
my.migration$Pop <- my.population[(as.numeric(as.character(my.population$Year)) > 1986),]

unemployment$migration <- c(NA,NA,NA,NA,my.migration$value*1000)

ggplot(data=my.migration, aes(x=my.migration$Year.num, y= my.migration$value)) + geom_line()
ggplot(data=unemployment, aes(x=unemployment$years,y=unemployment$Total.Labour)) + geom_line()
ggplot(data=unemployment, aes(x=unemployment$years,y=unemployment$migration)) + geom_line()

ggplot(data=unemployment, aes(x=unemployment$years)) +
  geom_line(aes(y=unemployment$migration)) + geom_line(aes(y=unemployment$Total.Labour)) 


corr_coeff <- cor(unemployment$migration, unemployment$Total.Labour, use="complete.obs")  

coefficients_1 <- lm(unemployment$migration ~ unemployment$Total.Labour,data=unemployment)

unemployment_1987 <-   unemployment[(as.numeric(as.character(unemployment$years)) > 1986),]
coefficients_1 <- lm(unemployment_1987$migration ~ unemployment_1987$Total.Labour,data=unemployment_1987)

ggplot(data=unemployment_1987, aes(x=unemployment_1987$migration, y = unemployment_1987$Total.Labour)) + geom_point() + abline(coefficients_1)
#method="lm", formula = unemployment_1987$Total.Labour ~ unemployment_1987$migration        
ggplot(data=unemployment_1987, aes(x=unemployment_1987$migration, y = unemployment_1987$Total.Labour)) + geom_point() + geom_smooth(method="lm", formula = y~x)

#Males
mpopulation <- population[(population$Age.Group=="All ages")&(population$Sex=="Male")&(as.numeric(as.character(population$Year)) > 2001)&(as.numeric(as.character(population$Year)) < 2015),]
mpopulation

mpopulation65 <- population[(population$Age.Group=="65 years and over")&(population$Sex=="Male")&(as.numeric(as.character(population$Year)) > 2001)&(as.numeric(as.character(population$Year)) < 2015),]
mpopulation65

mpopulation15 <- population[(population$Age.Group=="15 years and over")&(population$Sex=="Male")&(as.numeric(as.character(population$Year)) > 2001)&(as.numeric(as.character(population$Year)) < 2015),]
mpopulation15

mpopulation$Age65 <- mpopulation65$value
mpopulation$Age15 <- mpopulation15$value
mpopulation$value.Working <- mpopulation$Age15- mpopulation$Age65

munemployment <- unemployment[(as.numeric(as.character(unemployment$years)) > 2001),]
munemployment$Population.labour <- mpopulation$value.Working
munemployment$Total.Labour <- munemployment$Population.labour*munemployment$rates*10
munemployment

mmigration <- migration[(migration$Age.Group=="All ages")&(migration$Sex=="Male")&(migration$Inward.or.Outward.Flow=="Immigrants: All origins")&(as.numeric(as.character(migration$Year)) > 2001)&(as.numeric(as.character(migration$Year)) < 2015),]
munemployment$migration <- mmigration$value*1000


mcorr_coeff <- cor(munemployment$migration, munemployment$Total.Labour, use="complete.obs")  
mcoefficients_1 <- lm(munemployment$migration ~ munemployment$Total.Labour,data=munemployment)
ggplot(data=munemployment, aes(x=munemployment$migration, y = munemployment$Total.Labour)) + geom_point() + geom_smooth(method="lm", formula = y~x)


#Females
fpopulation <- population[(population$Age.Group=="All ages")&(population$Sex=="Female")&(as.numeric(as.character(population$Year)) > 2001)&(as.numeric(as.character(population$Year)) < 2015),]
fpopulation

fpopulation65 <- population[(population$Age.Group=="65 years and over")&(population$Sex=="Female")&(as.numeric(as.character(population$Year)) > 2001)&(as.numeric(as.character(population$Year)) < 2015),]
fpopulation65

fpopulation15 <- population[(population$Age.Group=="15 years and over")&(population$Sex=="Female")&(as.numeric(as.character(population$Year)) > 2001)&(as.numeric(as.character(population$Year)) < 2015),]
fpopulation15

fpopulation$Age65 <- fpopulation65$value
fpopulation$Age15 <- fpopulation15$value
fpopulation$value.Working <- fpopulation$Age15- fpopulation$Age65

funemployment <- unemployment[(as.numeric(as.character(unemployment$years)) > 2001),]
funemployment$Population.labour <- fpopulation$value.Working
funemployment$Total.Labour <- funemployment$Population.labour*funemployment$rates*10
funemployment

fmigration <- migration[(migration$Age.Group=="All ages")&(migration$Sex=="Female")&(migration$Inward.or.Outward.Flow=="Immigrants: All origins")&(as.numeric(as.character(migration$Year)) > 2001)&(as.numeric(as.character(migration$Year)) < 2015),]
funemployment$migration <- fmigration$value*1000


fcorr_coeff <- cor(funemployment$migration, funemployment$Total.Labour, use="complete.obs")  
fcoefficients_1 <- lm(funemployment$migration ~ funemployment$Total.Labour,data=funemployment)
ggplot(data=funemployment, aes(x=funemployment$migration, y = funemployment$Total.Labour)) + geom_point() + geom_smooth(method="lm", formula = y~x)

  