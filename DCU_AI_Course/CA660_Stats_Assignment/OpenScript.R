install.packages("ggplot2")
library(pxR)
library(ggplot2)
library(dplyr)

migration <- as.data.frame(read.px("Datasets/PEA03.px"))
population <- as.data.frame(read.px("Datasets/PEA01.px"))
years <- seq(1983,2014,1)
rates <- c(14.00 , 15.60 , 16.90 , 17.00 , 16.80 , 16.10 , 14.60 , 13.20 , 14.60 , 15.20 , 15.50 , 14.00 , 12.10, 11.50 , 10.30 ,  7.40 ,  5.50 ,  4.30 ,  3.90 ,  4.40 ,  4.60 ,  4.50 ,  4.40 ,  4.50 ,  4.70 ,  6.40 ,  12.00 ,  13.80 ,  14.60 , 14.70 , 13.10 ,  11.20)
unemployment <- data.frame(years, rates)
population
my.population <- population[(population$Age.Group=="All ages")&(population$Sex=="Both sexes")&(as.numeric(as.character(population$Year)) > 1982)&(as.numeric(as.character(population$Year)) < 2015),]

my.population65 <- population[(population$Age.Group=="65 years and over")&(population$Sex=="Both sexes")&(as.numeric(as.character(population$Year)) > 1982)&(as.numeric(as.character(population$Year)) < 2015),]
my.population15 <- population[(population$Age.Group=="15 years and over")&(population$Sex=="Both sexes")&(as.numeric(as.character(population$Year)) > 1982)&(as.numeric(as.character(population$Year)) < 2015),]

my.population$Age65 <- my.population65$value
my.population$Age15 <- my.population15$value
my.population$value.Working <- my.population$Age15 - my.population$Age65

unemployment$Population <- my.population$value

unemployment$Population.labour <- my.population$value.Working

unemployment$Total <- unemployment$Population*unemployment$rates*10
unemployment$Total <- unemployment$Population.labour*unemployment$rates*10

levels(population$Age.Group)

plot(x=unemployment$years, y= unemployment$migration)
ggplot(data=unemployment, aes(x=unemployment$years, y= unemployment$Population)) +
  geom_line()
ggplot(data=unemployment, aes(x=unemployment$years, y= unemployment$rates)) +
  geom_line()

my.migration <- migration[(migration$Age.Group=="All ages")&(migration$Sex=="Both sexes")&(migration$Inward.or.Outward.Flow=="Immigrants: All origins")&(as.numeric(as.character(migration$Year)) > 1986)&(as.numeric(as.character(migration$Year)) < 2015),]
my.migration$Year.num <- as.numeric(as.character(my.migration$Year))

my.migration$Pop <- my.population[(as.numeric(as.character(my.population$Year)) > 1986),]

unemployment$migration <- c(NA,NA,NA,NA,my.migration$value*1000)
ggplot(data=my.migration, aes(x=my.migration$Year.num, y= my.migration$value)) + geom_line()
                           
ggplot(data=unemployment, aes(x=unemployment$years,y=unemployment$migration)) + geom_line()

#ggplot(data=unemployment, aes(x=unemployment$years)) +
  #geom_line(aes(y=unemployment$migration)) + geom_line(aes(y=unemployment$Total.Labour)) 


corr_coeff <- cor(unemployment$migration, unemployment$Total, use="complete.obs")  

coefficients_1 <- lm(unemployment$migration ~ unemployment$Total, data=unemployment)

unemployment_1987 <-   unemployment[(as.numeric(as.character(unemployment$years)) > 1986),]


coefficients_1 <- lm(unemployment_1987$migration ~ unemployment_1987$Total,data=unemployment_1987)


                  
ggplot(data=unemployment_1987, aes(x=unemployment_1987$migration, y = unemployment_1987$Total)) + geom_point() + abline(coefficients_1)
#method="lm", formula = unemployment_1987$Total.Labour ~ unemployment_1987$migration        
ggplot(data=unemployment_1987, aes(x=unemployment_1987$migration, y = unemployment_1987$Total)) + geom_point() + geom_smooth(method="lm", formula = y~x)

  