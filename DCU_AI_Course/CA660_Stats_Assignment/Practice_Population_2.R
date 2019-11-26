# load required libraries
library(pxR)
library(reshape2)
library(dplyr)
library(rgdal)
library(ggplot2)
#http://maps.unomaha.edu/Peterson/geog1000/PopulationProjections/Population_Projections_GEOG1000-Answers.pdf 
#https://www.irishexaminer.com/breakingnews/ireland/irish-population-rises-by-64500-bringing-it-to-almost-5m-946672.html 

#Population growth equation
P <- 4880000
e <- 2.71828
r <- 0.0079
t <- 30
Nt <- P*e^(r*t)



filename <- "Datasets/WorldBank_Population.csv"
data <- read.csv(filename)


plot(data$Country.Code, data$X1960)# load px file as data frame
plot(data$Country.Code, data$X1960)



# read the shape file
spdf <- readOGR(dsn  = 
                  "Datasets/Census2011_Admin_Counties_generalised20m.shp")


# make it ggplot-friendly
spdf@data$id <- rownames(spdf@data)
spdf.points <- fortify(spdf, region="id")
counties <- inner_join(spdf.points, spdf@data, by="id")

# plot it
ggplot(counties) + geom_polygon(colour="black", fill=NA, aes(x=long, y=lat, group=group)) + coord_fixed()


filename <- "Datasets/E2001.px"
filename <- "Datasets/IIA13.px"
# load px file as data frame
data <- as.data.frame(read.px(filename))
attach(data)
names(data)

Population <- data[data$Sex == 'Both sexes',]
Population <- data[data$Sex == 'Both sexes' & data$County == 'State',]
linmodel <- lm(Population$value~Population$CensusYear, Population)
plot(Population$CensusYear,Population$value)

data_2016 <- data[data$CensusYear == '2016',]
data_2 <- subset(data_2016, select=-c(Sex))
data_wide <- dcast(data_2, County.and.City~Age.Group, value.var = "value")
