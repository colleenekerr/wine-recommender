setwd("~/Documents/Booth/MachineLearning/wine-recommender/data")


PackageList =c('recommenderlab','reshape2','data.table','lattice','curl','RColorBrewer','ggplot2')
NewPackages=PackageList[!(PackageList %in% 
                            installed.packages()[,"Package"])]
if(length(NewPackages)) install.packages(NewPackages,repos = "http://cran.us.r-project.org")
lapply(PackageList,require,character.only=TRUE)#array function

set.seed(1)

# Read in data
winedata <- read.csv("wine-data.csv",stringsAsFactors=FALSE)
varietal_lookup <- read.csv("Varietal_Lookup.csv",stringsAsFactors=FALSE )

# Remove duplicates
winedata <- winedata[!duplicated(winedata$description),]

# Remove ratings from tasters with < 1000 reviews and wines with no reviewer
winedata <- winedata[winedata$taster_name %in% names(which(table(winedata$taster_name) > 1000)), ]
winedata$taster_name[winedata$taster_name==""] <- "NA"
winedata <- winedata[winedata$taster_name != "NA",]

# Add additional fields
winedata$year = stringr::str_extract(winedata$title, "[0-9]{4}")
for (i in 1:nrow(winedata)) {
  winedata$style[i] = ifelse((winedata$variety[i] %in% varietal_lookup[,1]), varietal_lookup[which(winedata$variety[i] == varietal_lookup[,1]),2], "Other")
}
winedata$region_2 <- ifelse(winedata$region_2=="", winedata$region_1, winedata$region_2)

#Convert all columns to factors
winedata <- as.data.frame(unclass(winedata))

attach(winedata)

winetable <- data.table(winedata)
data.table(winedata)[,sum(points/points),by=taster_name]
winetable[,mean(points),by=country]

qplot(y=points, x=country, data=winedata, geom="boxplot")
table(year)

qplot(y=points, x=taster_name, data=winedata, geom="boxplot")
qplot()
hist(points)
hist(log(price))
plot(log(price), points)
boxplot(log(stringr::str_length(description)) ~ points, main="log(description length) vs. wine rating")

#recommenderlab setup

wineratings = as(winedata, 'realRatingMatrix')

evaluation_scheme=evaluationScheme(
  wineratings,       # data, must be a realRatingMatrix
  method='split',# split randomly into training and test set, can be "cross-validation","bootstrap"
  train=0.6,     # fraction of data used for training
  k=1,           # number of folds/times to run evaluation, default 1 for split, and 10 for bootstrap/cross-validation
  given=10       # number of items (movies) given for evaluation per test user
)

?realRatingMatrix
