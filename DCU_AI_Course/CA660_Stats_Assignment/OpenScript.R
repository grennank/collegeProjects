#setwd("C:/collegeProjects/DCU_AI_Course/CA660_Stats_Assignment")
# setwd("~/Documents/GitProjects/collegeProjects/DCU_AI_Course/CA660_Stats_Assignment")

install.packages("gmodels")
install.packages("ggplot2")
install.packages("GoodmanKruskal")
library(pxR)
library(ggplot2)
library(dplyr)
library(gmodels)
library(GoodmanKruskal)
update.packages("rlang")
install.packages("rlang")

migration <- as.data.frame(read.px("Datasets/PEA03.px"))
population <- as.data.frame(read.px("Datasets/PEA01.px"))

  
years <- seq(1983,2014,1)
rates <- c(14.00 , 15.60 , 16.90 , 17.00 , 16.80 , 16.10 , 14.60 , 13.20 , 14.60 , 15.20 , 15.50 , 14.00 , 12.10, 11.50 , 10.30 ,  7.40 ,  5.50 ,  4.30 ,  3.90 ,  4.40 ,  4.60 ,  4.50 ,  4.40 ,  4.50 ,  4.70 ,  6.40 ,  12.00 ,  13.80 ,  14.60 , 14.70 , 13.10 ,  11.20)
unemployment <- data.frame(years, rates)
#population
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

plot(x=unemployment$years, y= unemployment$Total)

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


monthly_umeploy <- as.data.frame(read.px("Datasets/MUM01.px"))
mon_unemploy_tots <- monthly_umeploy[(monthly_umeploy$Statistic == "Seasonally Adjusted Monthly Unemployment (Thousand)"),]

plot_munemploy <- ggplot(monthly_umeploy, aes(monthly_umeploy$Month, monthly_umeploy$value))

plot_munemploy + geom_point(aes(colour = factor(monthly_umeploy$Age.Group))) 
plot_munemploy + geom_point(aes(colour = factor(monthly_umeploy$Sex))) 

 #geom_text(aes(label = monthly_umeploy$Age.Group))

#Boxplot of Nonzero Unemployment by Age Group
ggplot(mapping = aes(x = monthly_umeploy$Age.Group, y = monthly_umeploy$value),
       data = subset(monthly_umeploy, monthly_umeploy$value > 0)) + 
  geom_boxplot() + stat_summary(fun.y = mean,
                                geom = 'point', 
                                shape = 19,
                                color = "red",
                                cex = 2) +
  labs(x = "Age Groups", 
       y = "Unemployment (thousands)") +
  ggtitle("Nonzero Unemployment by Age Group") 

ggplot(data = monthly_umeploy, aes(monthly_umeploy$value, fill = monthly_umeploy$Sex)) + 
  geom_density(alpha = 0.2) 

as.numeric(monthly_umeploy$Sex)
ggplot(data = monthly_umeploy, aes(monthly_umeploy$value, fill = monthly_umeploy$Age.Group)) + 
  geom_density(alpha = 0.2) 

#CrossTable(monthly_umeploy$Sex, monthly_umeploy$value, 
#           prop.chisq = TRUE,
#           chisq = TRUE)

#CrossTable(population$Year, population$value, 
#           prop.chisq = TRUE,
#           chisq = TRUE)
chisq.test(population$Year, population$value)

chisq.test(population$Age.Group, population$value)
#Men 
  
unemploy_combined

#Women

# Persons on the Live Register by Sex, Duration, Month and Age Group (
live_reg_mon_dur <- as.data.frame(read.px("Datasets/LRM11.px"))
plot_munemp_dur <- ggplot(live_reg_mon_dur, aes(live_reg_mon_dur$Month, live_reg_mon_dur$value))
plot_munemp_dur + geom_point(aes(colour = factor(live_reg_mon_dur$Duration))) 

# Persons on the Live Register by Month, Sex, Age Group and Statistic (1967M01-2019M10) 
live_reg_mon_stat <- as.data.frame(read.px("Datasets/LRM02.px"))


names(population)

population$Year <- as.factor(population$Year)
population$Age.Group <- as.factor(population$Age.Group)
population$Sex <- as.factor(population$Sex)

covariates <- paste("Year", "Sex", "Age.Group", sep = "+")
form <- as.formula(paste("value ~", covariates))
  
glm.model <- glm(formula = form,
                 data = population, 
                 family = binomial(link = "logit"),
                 x = TRUE)
classification_model <- glm(as.factor(population$value) 
                            ~as.factor(population$Sex) 
                              , data = 
                              population, family = binomial(link = "logit"))
# The option "x=TRUE" returns the design matrix

summary(classification_model)$coefficients[, 1:2]

GKmatrix <- GKtauDataframe(population[, c("Year", "Sex", "Age.Group")])
plot(GKmatrix)

## Splitting training and test
## 75% of the sample size
smp_size <- floor(0.75 * nrow(population))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(population)), size = smp_size)

train <- population[train_ind, ]
test <- population[-train_ind, ]

#SVM dataset setup
rm(SVM_df)
SVM_df$Sex <- NA
SVM_df$Duration <- NULL
SVM_df = data.frame()

# if Number <= 50 Size is small, 
# if Number is between 50 and 70, Size is Medium
# if Number is Bigger than 70, Size is Big
liv_reg_mod_dur_v2 <- live_reg_mon_dur[(live_reg_mon_dur$Duration != "All durations") 
                                       & (live_reg_mon_dur$Sex != "Both sexes")
                                       & (live_reg_mon_dur$Age.Group != "All ages")
                                       & (live_reg_mon_dur$Age.Group != "Under 20 years")
                                       & (live_reg_mon_dur$Age.Group != "20 - 24 years")
                                       & (live_reg_mon_dur$Age.Group != "25 years and over"),]
levels(liv_reg_mod_dur_v2$Age.Group)
levels(liv_reg_mod_dur_v2$Duration)

liv_reg_mod_dur_v2$Dur <- ifelse(liv_reg_mod_dur_v2$Duration == "Less than one year", 0, ifelse(liv_reg_mod_dur_v2$Duration == "One year or more", 1, NA))

liv_reg_mod_dur_v2$Age <- ifelse(liv_reg_mod_dur_v2$Age.Group == "Under 25 years", 0,
     ifelse(liv_reg_mod_dur_v2$Age.Group == "25 - 34 years", 1, 
            ifelse(liv_reg_mod_dur_v2$Age.Group == "35 - 44 years", 2,
                   ifelse(liv_reg_mod_dur_v2$Age.Group == "45 - 54 years", 3,
                          ifelse(liv_reg_mod_dur_v2$Age.Group == "55 - 59 years", 4,
                                 ifelse(liv_reg_mod_dur_v2$Age.Group == "60 - 64 years", 5,NA))))))


install.packages('caTools') 
library(caTools) 

set.seed(123) 
liv_reg_mod_dur_SVM = liv_reg_mod_dur_v2[5:7] 
liv_reg_mod_dur_SVM$value = as.numeric(as.factor(liv_reg_mod_dur_SVM$value))
liv_reg_mod_dur_SVM$Age = as.numeric(as.factor(liv_reg_mod_dur_SVM$Age))
liv_reg_mod_dur_SVM$Dur = as.numeric(as.factor(liv_reg_mod_dur_SVM$Dur))

split = sample.split(liv_reg_mod_dur_SVM$value, SplitRatio = 0.75) 

training_set = subset(liv_reg_mod_dur_SVM, split == TRUE) 
test_set = subset(liv_reg_mod_dur_SVM, split == FALSE) 

# Feature Scaling 
training_set[-3] = scale(training_set[-3]) 
test_set[-3] = scale(test_set[-3]) 

install.packages('e1071') 
library(e1071) 

Sys.setenv('R_MAX_VSIZE'=32000000000)
Sys.getenv('R_MAX_VSIZE')
usethis::edit_r_environ()
classifier = svm(formula = value ~ ., 
                 data = liv_reg_mod_dur_SVM, 
                 type = 'C-classification', 
                 kernel = 'linear') 

y_pred = predict(classifier, newdata = test_set[-3]) 








