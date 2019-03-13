setwd("~/Documents/Booth/MachineLearning/wine-recommender/data")
install.packages("qdap")
library(qdap)
PackageList =c('recommenderlab','reshape2','data.table','lattice','curl','RColorBrewer','ggplot2','tm','qdap')
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

############## Sample smaller dataset for testing #########################
winedata <- winedata[sample(nrow(winedata),10000),]
###########################################################################

# Remove ratings from tasters with < 1000 reviews and wines with no reviewer
# winedata <- winedata[winedata$taster_name %in% names(which(table(winedata$taster_name) > 1000)), ]
winedata$taster_name[winedata$taster_name==""] <- "Unknown"
# winedata <- winedata[winedata$taster_name != "NA",]

# Add additional data fields
winedata$year = stringr::str_extract(winedata$title, "[0-9]{4}")
for (i in 1:nrow(winedata)) {
  winedata$style[i] = ifelse((winedata$variety[i] %in% varietal_lookup[,1]), varietal_lookup[which(winedata$variety[i] == varietal_lookup[,1]),2], "Other")
}
winedata$region_2 <- ifelse(winedata$region_2=="", winedata$region_1, winedata$region_2)
winedata$description_length <- stringr::str_length(description)

#Convert all columns to factors
winedata <- as.data.frame(unclass(winedata))

# clean descriptions before creatign DocumentTermMatrix
winedata$description <- tolower(winedata$description)
winedata$description <- removeWords(winedata$description, stopwords("en")) 
winedata$description <- removePunctuation(winedata$description)

corp <- Corpus(VectorSource(winedata$description))
dtm <- DocumentTermMatrix(corp)
dtm <- removeSparseTerms(dtm, .99)    # remove words that appear in < 1% of reviews

description_word_matrix <- as.matrix(dtm)

winedata <- cbind(winedata, description_word_matrix)

# clusters

wine_clusters <- 




  
  
  
  
  
  

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




