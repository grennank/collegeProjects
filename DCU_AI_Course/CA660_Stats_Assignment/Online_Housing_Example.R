# load required libraries
library(pxR)
library(reshape2)
library(dplyr)
library(rgeos)
library(maptools)
library(rgdal)
library(ggplot2)
library(proj4)
library(ggmap)

register_google(key = "AIzaSyDcloJoYiwGVrTl14Rmed3O2oYxZmo4TBs")


# read the shape file
spdf <- readOGR(dsn  = 
  "Datasets/Census2011_Admin_Counties_generalised20m.shp")

# make it ggplot-friendly
spdf@data$id <- rownames(spdf@data)
spdf.points <- fortify(spdf, region="id")
counties <- inner_join(spdf.points, spdf@data, by="id")

# plot it
ggplot(counties) + geom_polygon(colour="black", fill=NA, aes(x=long, y=lat, group=group)) + coord_fixed()

# store the 'proj4' string for TM65
# (can be found on spatialreference.org)
tm65 <- "+proj=tmerc +lat_0=53.5 +lon_0=-8 +k=1.000035 +x_0=200000 +y_0=250000 +a=6377340.189 +b=6356034.447938534 +units=m +no_defs "

# reverse the projection on long/lat
newlonglat <- project(
  cbind(counties$long, counties$lat),
  proj=tm65, inverse=TRUE)

# replace long/lat with the new ones
counties$long <- newlonglat[,1]
counties$lat <- newlonglat[,2]

# plot it
ggplot(counties) + geom_polygon(colour="black", fill=NA, aes(x=long, y=lat, group=group)) + coord_fixed()


gmireland + geom_polygon(data=counties, colour="black", fill=NA, aes(x=long, y=lat, group=group))


names(counties)

# fill each region according to 
# the TOTAL2011 column value
ggplot(counties) + geom_polygon(colour="black", aes(x=long, y=lat, group=group, fill=TOTAL2011))

# load px file as data frame
ep007 <- as.data.frame(read.px("Datasets/EP007_Housing.px"))
# make it wide format (requires reshape2 package)
ep007_wide <- dcast(ep007, Province.County.or.City~Statistic, value.var = 'value')
# then remove non-admin counties aggregates: counties, state, provinces#
# requires (dplyr package)
ep007_wide_admin <- as.data.frame( filter(ep007_wide, !(Province.County.or.City %in%
                                                          c("Cork", "Dublin", "Galway", "Limerick", "Waterford", "State",
                                                            "Connacht", "Munster", "Leinster", "Ulster (part of)"))) )
# rename counties to match map nomenclature and remove unused factor levels
# (my function doing this is provided after the 'last' slide)
ep007_wide_admin$Province.County.or.City <- factor(factor_rename_counties_long(ep007_wide_admin$Province.County.or.City))
# strange errors on merge with factors, so forcing to string
counties$COUNTYNAME <- as.vector(counties$COUNTYNAME)
ep007_wide_admin$Province.County.or.City <- as.vector(ep007_wide_admin$Province.County.or.City)
# encoding problem with the 'ú' so overwriting for consistency
counties$COUNTYNAME[counties$COUNTYNAME == "D\xfan Laoghaire-Rathdown"] <- "Dún Laoghaire-Rathdown"
ep007_wide_admin$Province.County.or.City[ep007_wide_admin$Province.County.or.City == "Dún Laoghaire-Rathdown"] <- "Dún Laoghaire-Rathdown"

# check available data column names
colnames(ep007_wide_admin)

# merge map and data
ep007_map <- left_join(counties, ep007_wide_admin, by=c("COUNTYNAME" = "Province.County.or.City"))
ep007_map$COUNTYNAME <- factor(ep007_map$COUNTYNAME)

# define a base map layer
housingplot_baselayer <- ggplot(ep007_map) + 
  aes(long, lat, group=group) +
  geom_polygon(colour="grey")

# plot a data column (vacancy rates 2016 %)
housingplot_baselayer + aes(fill=`Vacancy Rate - 2016 (%)`)

# substracting both column to show evolution of housing stock between 2011 and 2016
housingplot_baselayer + aes(fill=`Housing Stock -2016 (Number)` - `Housing Stock - 2011 (Number)`) +
  scale_fill_gradient2(limits=c(-350,2900), low="red", mid="white", high="blue") +
  labs(fill="Housing Stock evolution 2011-2016")

#show counties by positive/negative housing stock evolution
housingplot_baselayer +
  aes(fill=(`Housing Stock -2016 (Number)` - `Housing Stock - 2011 (Number)`)>0) +
  labs(fill="Housing Stock increase 2011-2016")

p1 <- housingplot_baselayer + aes(fill=`Total vacant dwellings 2011 (Number)`) + scale_fill_gradient(limits=c(0,30000), low="white", high="blue") + labs(fill='Total, 2011')
p2 <- housingplot_baselayer + aes(fill=`Total vacant dwellings 2016 (Number)`) + scale_fill_gradient(limits=c(0,30000), low="white", high="blue") + labs(fill='Total, 2016')
multiplot(p1, p2, cols=2) # see additional code at the end for that function's definition




factor_rename_counties_long <- function(myfactor){
  recode(myfactor,
         Carlow = 'Carlow County',
         Dublin = 'Dublin County',
         Kildare = 'Kildare County',
         Kilkenny = 'Kilkenny County',
         Laois = 'Laois County',
         Longford = 'Longford County',
         Louth = 'Louth County',
         Meath = 'Meath County',
         Offaly = 'Offaly County',
         Westmeath = 'Westmeath County',
         Wexford = 'Wexford County',
         Wicklow = 'Wicklow County',
         Clare = 'Clare County',
         Cork = 'Cork County',
         Kerry = 'Kerry County',
         Limerick = 'Limerick County',
         Waterford = 'Waterford County',
         Galway = 'Galway County',
         Leitrim = 'Leitrim County',
         Mayo = 'Mayo County',
         Roscommon = 'Roscommon County',
         Sligo = 'Sligo County',
         Cavan = 'Cavan County',
         Donegal = 'Donegal County',
         Monaghan = 'Monaghan County')
}

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

## shp2df: convenience function transforming an Esri shape file
## into a "simple" data frame that can be plotted with ggplot2.
## takes 2 parameters:
##   - shapefilepath: path to .shp file (and corresponding .dbf and .shx)
##   - projectionstring: proj4 format string defining the projection to reverse
##     on the long/lat columns. If undefined, defaults to TM65 (Irish Grid).
##     If set to FALSE, then the coordinates are left to the original values
##     from the shape file.
shp2df <- function(shapefilepath, projectionstring){
  # read shape file into SpatialPolygonsDataFrame object
  spdf <- readShapePoly( shapefilepath )
  # make a redundant but ggplot-friendly data frame out of it
  spdf@data$id <- rownames(spdf@data)
  spdf.points <- fortify(spdf, region="id")
  spdf.df <- inner_join(spdf.points, spdf@data, by="id")
  # default to TM65 (irish grid) if no transformation was specified
  if (missing(projectionstring)){
    projectionstring <- "+proj=tmerc +lat_0=53.5 +lon_0=-8 +k=1.000035 +x_0=200000 +y_0=250000 +a=6377340.189 +b=6356034.447938534 +units=m +no_defs "
  }
  # reverse transformation to get long/lat
  if (projectionstring != FALSE){
    longlat_transformed <- project(cbind(spdf.df$long, spdf.df$lat),
                                   proj=projectionstring, inverse=TRUE)
    spdf.df$long <- longlat_transformed[,1]
    spdf.df$lat <- longlat_transformed[,2]
  }
  return(spdf.df)
}