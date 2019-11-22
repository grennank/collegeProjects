#Read in table
library(pxR)
library(reshape2)
library(dplyr)
library(data.table)
filename <- "EB005.px"

# load px file as data frame
data <- as.data.frame(read.px("EP007_Housing.px"))
attach(data)
names(data)
# make it wide format (requires reshape2 package)
data_wide <- dcast(data, Principal.Economic.Status~Sex+Nationality+CensusYear, value.var = 'value')
# then remove 
# requires (dplyr package)
# ep007_wide_admin <- as.data.frame( filter(ep007_wide, !(Province.County.or.City %in%
                                                          #c("Cork", "Dublin", "Galway", "Limerick", "Waterford", "State",
                                                            #"Connacht", "Munster", "Leinster", "Ulster (part of)"))) )

Malesdf <- data[data$Sex == "Male",]
mean(Malesdf$value)

Femalesdf <- data[data$Sex == "Female",]
mean(Femalesdf$value)

t.test(Malesdf$value, Femalesdf$value)

ggplot(data, aes(x=Principal.Economic.Status, y=value)) + 
  geom_boxplot() +ylim(0, 1000)

ggplot(data, aes(x=Sex, y=value, fill=CensusYear)) + 
  geom_boxplot() +ylim(0, 1000)

t.test(value,as.numeric(CensusYear), var.equal=TRUE, paired=FALSE)
t.test(value,as.numeric(Sex), var.equal=TRUE, paired=FALSE)
