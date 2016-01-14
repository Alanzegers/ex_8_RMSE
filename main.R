### Alan Zegers & Stijn Wijdeven
### Teamname*
### January 2016
#setwd("/home/user/R_projects/Ex_8")  #for ourselves...
#getwd()
rm(list=ls())
ifolder<- ('./data')
ofolder<- ('./output')
dir.create(ifolder, showWarnings = FALSE)
dir.create(ofolder, showWarnings = FALSE)
dataURL <- "https://github.com/GeoScripting-WUR/AdvancedRasterAnalysis/archive/gh-pages.zip"
inputZip <- list.files(path=ifolder, pattern= '^.*\\.zip$')

if (length(inputZip) == 0){ ##only download when not alrady downloaded
	download.file(url = dataURL, destfile = 'data/landsatData.zip', method = 'wget')
} 

unzip('data/landsatData.zip', exdir=ifolder)
### Exercise 8 Advanced Raster Analysis
rm(list=ls())
### Download files

### Load libraries
library(raster)
library(rasterVis)

### Load data
load("data/AdvancedRasterAnalysis-gh-pages/data/GewataB2.rda")
load("data/AdvancedRasterAnalysis-gh-pages/data/GewataB3.rda")
load("data/AdvancedRasterAnalysis-gh-pages/data/GewataB4.rda")
load("data/AdvancedRasterAnalysis-gh-pages/data/GewataB1.rda")
load("data/AdvancedRasterAnalysis-gh-pages/data/GewataB5.rda")
load("data/AdvancedRasterAnalysis-gh-pages/data/GewataB7.rda")
load("data/AdvancedRasterAnalysis-gh-pages/data/vcfGewata.rda")
load("data/AdvancedRasterAnalysis-gh-pages/data/trainingPoly.rda")

### Brick layers
gewata <- brick(GewataB1, GewataB2, GewataB3, GewataB4, GewataB5, GewataB7)

### Exclude clouds and water
vcfGewata[vcfGewata > 100] <- NA

### Make reflectance values between 0 and 1
gewata <- calc(gewata, fun=function(x) x / 10000)

### Add VCF to brick
covs <- addLayer(gewata, vcfGewata)
names(covs) <- c("band1", "band2", "band3", "band4", "band5", "band7", "VCF")

valuecovs <- getValues(covs)
valuecovs <- as.data.frame(valuecovs)
valuecovs <- na.omit(valuecovs)

### Perform regression
regression <- lm(VCF ~ band1 + band2 + band3 + band4 + band5 + band7, data=valuecovs)
summary(regression)

### Make prediction
prediction <- predict(covs, model=regression, na.rm=TRUE)
prediction[prediction < 0] <- NA

### Make comparison
comparison <- vcfGewata - prediction
names(comparison) <- ("VCF - Prediction")

levelplot(comparison, col.regions=terrain.colors(20), colorkey = list(space = "bottom"))

comparisonDF <- as.data.frame(comparison)
comparisonDF <- na.omit(comparisonDF)

### Perform RMSE
RMSE <- sqrt(mean((comparisonDF)^2))

### Define polygon classes
trainingPoly@data$Code <- as.numeric(trainingPoly@data$Class)

classes <- rasterize(trainingPoly, vcfGewata, field='Code')

### RMSE per class Table
RS_comparison <- (comparison**2)
RMSEclass <- zonal(RS_comparison, classes, fun='mean', digits=0, na.rm=TRUE)
RMSEclass[,2] <- RMSEclass[,2]**0.5
RMSE_Table <- as.data.frame(RMSEclass)
labels <- c("Cropland", "Forest", "Wetland")
RMSE_Table$zone <- labels
print(RMSE_Table)

### Plot RMSE per class & create output file
cols <- c("light green", "dark green", "light blue")
barplot(RMSE_Table$mean, col=cols, ylim = c(0, 12), names=labels, main = "RMSE per class", ylab="Value RMSE" )