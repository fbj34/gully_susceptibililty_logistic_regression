
getwd()

library(sf)
library(sp)
library(tidyverse)
library(forcats)
library(rpart)
library(caret)
library(raster)
library(rpart.plot)
library(mapview)
library(rasterVis)

#reading in raster files
ndvi <- raster("F:/DA/R/Gully_dataset/reclass_2018_ndv1.tif")
aspect <- raster("F:/DA/R/Gully_dataset/aspect.tif")
curvature <- raster("F:/DA/R/Gully_dataset/reclass_curvature.tif")
elevation <- raster("F:/DA/R/Gully_dataset/reclass_elevationn.tif")
erosivity <- raster("F:/DA/R/Gully_dataset/reclass_erosivity.tif")
geology <- raster("F:/DA/R/Gully_dataset/reclass_geo.tif")
landuse <- raster("F:/DA/R/Gully_dataset/reclass_landuse.tif")
slope <- raster("F:/DA/R/Gully_dataset/reclass_slope.tif")
soil <- raster("F:/DA/R/Gully_dataset/reclass_soil.tif")
twi <- raster("F:/DA/R/Gully_dataset/Reclass_TWI_filter_2nd1.tif")

#converting raster to a list file type
raster_list <-as.list(ndvi, aspect, curvature, elevation, erosivity, geology, landuse, slope, soil, twi)
str(raster_list)

#resampling so that all raster will have same dimension and origin
raster_list_resample <- lapply(raster_list, FUN = resample, raster_list[[10]])

#stacking raster files
gully_factors_stack <- stack(raster_list_resample, quick=TRUE)

str(gully_factors_stack)

#checking number of layers
nlayers(gully_factors_stack)
names(gully_factors_stack)

#renaming bands of the raster file
names(gully_factors_stack) <- c("ndvi", "aspect", "curvature","elevation", "erosivity", "geology", "landuse", "slope", "soil", "twi")


#reading in shapefile of gully points and non-gully points
gully_pts <- st_read("F:/DA/R/Gully_dataset/gully_tr_pts.shp", quiet = T)
non_gully_pts <- st_read("F:/DA/R/Gully_dataset/non_gully_tr_pts.shp", quiet = T)

summary(gully_pts)
summary(non_gully_pts)

str(gully_pts)
str(non_gully_pts)

#extracting the attribute presence/absence from the imported shapefiles
gully <-gully_pts$gng
non_gully <- non_gully_pts$gng

#converting imported shapefile to a spatial feature
g_pts_sp <-as(gully_pts, 'Spatial')
ng_pts_sp <-as(non_gully_pts, 'Spatial')

#extracting values from the stacked raster file to the shape file of gully occurrences
#the round function round to three decimal places
gully_ext <- raster::extract(gully_factors_stack, g_pts_sp) %>% round(., 3)

head(gully_ext)
tail(gully_ext)

summary(gully_ext)
str(gully_ext)

summary(gully_ext[,1])
View(gully_ext)


#extracting values from the stacked raster file to the shape file of non-gully points
ng_ext <- raster::extract(gully_factors_stack, ng_pts_sp) %>% round(., 3)
head(ng_ext)


#appending presence/absence (1/0) information to the data extracted from the raster
g_ext_df <- data.frame(gully_ext, gully)
ng_ext_df <- data.frame(ng_ext, non_gully)


#naming the column
colnames(g_ext_df)[11] <- "presence_absence"
colnames(ng_ext_df)[11] <- "presence_absence"

#combining extracted data for gully points and non-gully points
training_pts <- rbind(g_ext_df, ng_ext_df)
head(training_pts)
summary(training_pts)

names(training_pts)

par(mfrow=c(2,5)) 

#checking the distribution of factors in both gully and non-gully points
boxplot(aspect ~ presence_absence, data=training_pts, main= "Aspect", xlab=" ", ylab=" ", boxwex=0.3, col="lightblue", border="darkblue")
boxplot(ndvi ~ presence_absence, data=training_pts,main= "NDVI", xlab=" ", ylab=" ", boxwex=0.3, col="lightblue", border="darkblue")
boxplot(curvature ~ presence_absence, data=training_pts, main="Curvature", xlab=" ", ylab=" ", boxwex=0.3, col="lightblue", border="darkblue")
boxplot(elevation ~ presence_absence, data=training_pts, main="Elevation", xlab=" ", ylab=" ", boxwex=0.3, col="lightblue", border="darkblue")
boxplot(erosivity ~ presence_absence, data=training_pts, main="Erosivity", xlab=" ", ylab="", boxwex=0.3, col="lightblue", border="darkblue")
boxplot(geology ~ presence_absence, data=training_pts, main="Geology", xlab=" ", ylab=" ", boxwex=0.3, col="lightblue", border="darkblue")
boxplot(landuse ~ presence_absence, data=training_pts, main="Landuse", xlab=" ", ylab=" ", boxwex=0.3, col="lightblue", border="darkblue")
boxplot(slope ~ presence_absence, data=training_pts, main="Slope", xlab=" ", ylab=" ", boxwex=0.3, col="lightblue", border="darkblue")
boxplot(soil ~ presence_absence, data=training_pts,main="Soil", xlab=" ", ylab=" ", boxwex=0.3, col="lightblue", border="darkblue")
boxplot(twi ~ presence_absence, data=training_pts, main="TWI", xlab=" ", ylab=" ", boxwex=0.3, col="lightblue", border="darkblue")

par(mfrow=c(1,1))

#checking the number of samples in class i.e gully and non-gully points
aggregate(training_pts$soil ~training_pts$presence_absence, FUN=length)


str(training_pts)
summary(training_pts)


#duplicated training data
training_pts1 <- training_pts




#converting categorical data to factor data type
training_pts1$geology <-as.factor(training_pts1$geology)
training_pts1$soil <-as.factor(training_pts1$soil)

str(training_pts1)
levels(training_pts1$geology)


#implement one hot coding for categorical data
dummy <- dummyVars(" ~ .", data=training_pts1)
training_df <- data.frame(predict(dummy, newdata=training_pts1))


#SPLIT DATA INTO TWO SETS
training_df$random<-runif(length(training_df$presence_absence))
#runif function provides information about the uniform distribution on the interval
#from min to max, to generate random deviates.
train<-training_df[training_df$random<0.70, 1:11] ## the training database
evalu<-training_df[training_df$random>=0.70, 1:11] ## the evaluation database

nrow(train)
nrow(evalu)


library(MASS)
log_model <- glm(presence_absence~., data=train, family="binomial")%>% stepAIC(trace=FALSE) 

summary(log_model)

#Predicting the probabilities for test data
evalu$lr_pred <-log_model%>% predict(evalu, type="response")
summary(evalu)

#Accuracy assessment through AUC/ROC Plot
library(PresenceAbsence)
pa_validate<-data.frame(ID=1:length(evalu$presence_absence),
                        PA=evalu$presence_absence,
                        logistic=evalu$lr_pred)

presence.absence.accuracy(pa_validate, threshold=0.5)
error.threshold.plot(pa_validate,which.model=1)
auc.roc.plot(pa_validate)

#Accuracy assessment through comparison of mean and confusion matrix
#Predictions values <0.5 are converted to 0, >0.5 are converted 1
evalu$lr_pred1<-ifelse(evalu$lr_pred>0.5, "1", "0")
mean(evalu$presence_absence==evalu$lr_pred1)
confusionMatrix((as.factor(evalu$presence_absence)), (as.factor(evalu$lr_pred1)))


#converting the raster stack to a dataframe
raster_df <-terra::as.data.frame(gully_factors_stack, xy=TRUE)

raster_df$geology <-as.factor(raster_df$geology)
raster_df$soil <-as.factor(raster_df$soil)

#applying one hot coding to categorical data
dummy <- dummyVars(" ~ .", data=raster_df)
raster_df <- data.frame(predict(dummy, newdata=raster_df))


#making prediction for the converted raster dataframe
raster_df$logreg <- logreg_model %>% predict(raster_df[,3:19], type="response")
head(raster_df)

#converting the data frame back to raster
gully_ras_logreg <- rasterFromXYZ(raster_df)
projection(gully_ras_logreg) <-projection(gully_factors_stack)



#Minimum and maximum prediction values
min(gully_ras_logreg$lr_pred@data@values, na.rm = T)
max(gully_ras_logreg$lr_pred@data@values, na.rm = T)



library(RColorBrewer)
#There 11 graduations in the RdYlGn colour Palette. You can check for other Palettes
MyColour <- brewer.pal(11, "RdYlGn")
MyColour
MyPalette<-colorRampPalette(MyColour)
#The rev used in the col is to reverse the order of the colour so that low values appear as green while high values appear as red
plot(gully_ras_logreg$logreg, main="Gully Susceptibility Map", col=rev(MyPalette(20)))




