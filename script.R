
getwd()

library(sf)
library(sp)
library(tidyverse)
library(raster)
library(mapview)
library(rasterVis)

#reading in raster files
ndvi <- raster("F:/DA/R/Gully_dataset/reclass_2018_ndv1.tif")
aspect <- raster("F:/DA/R/Gully_dataset/aspect.tif")
curvature <- raster("F:/DA/R/Gully_dataset/reclass_curvature.tif")
elevation <- raster("F:/DA/R/Gully_dataset/reclass_elevationn.tif")
erosivity <- raster("F:/DA/R/Gully_dataset/reclass_erosivity.tif")
slope <- raster("F:/DA/R/Gully_dataset/reclass_slope.tif")
twi <- raster("F:/DA/R/Gully_dataset/Reclass_TWI_filter_2nd1.tif")
landuse <- raster("F:/DA/R/Gully_dataset/lulc_recl.tif")
geology <- raster("F:/DA/R/Gully_dataset/reclass_geo.tif")
soil <- raster("F:/DA/R/Gully_dataset/reclass_soil.tif")



#converting raster to a list file type
raster_list <-as.list(ndvi, aspect, curvature, elevation, erosivity, slope, twi, landuse, geology, soil)
str(raster_list)

#resampling so that all raster will have same dimension and origin
raster_list_resample <- lapply(raster_list, function(x) resample(x, raster_list[[10]], method='ngb'))


#stacking raster files in the list
gully_factors_stack <- stack(raster_list_resample, quick=TRUE)

str(gully_factors_stack)

#checking number of layers
nlayers(gully_factors_stack)
names(gully_factors_stack)

#renaming bands of the raster file
names(gully_factors_stack) <- c("ndvi", "aspect", "curvature","elevation", "erosivity", "slope", "twi", "landuse", "geology","soil")

#plotting the distribution of training points on ndvi
library(RColorBrewer)
MyColour <- brewer.pal(11, "RdYlGn")
MyPalette<-colorRampPalette(MyColour)

plot(gully_factors_stack, col=rev(MyColour))

#reading in shapefile of gully points and non-gully points
gully_pts <- st_read("F:/DA/R/Gully_dataset/gully_tr_pts.shp", quiet = T)
non_gully_pts <- st_read("F:/DA/R/Gully_dataset/non_gully_tr_pts.shp", quiet = T)

summary(gully_pts)
summary(non_gully_pts)

str(gully_pts)
str(non_gully_pts)



library(ggplot2)
library(terra)
library(tidyterra)

#plotting the distribution of training points over one raster
ggplot()+
  theme_bw()+
  theme(panel.grid.major = element_blank())+
  geom_sf (data=gully_pts, color="black")+
  geom_sf(data=non_gully_pts, color="grey")+
  ggtitle ("Distribution of training points")+
  coord_sf()

#converting ndvi to terra raster  
ndvi_t <-rast(ndvi)



ggplot()+
  theme_bw()+
  theme(panel.grid.major = element_blank())+
  geom_spatraster(data=ndvi_t)+
  geom_sf (data=gully_pts, aes(col="red"))+
  geom_sf(data=non_gully_pts, aes(col="lightblue"))+
  scale_fill_gradientn(colours=MyPalette(20), na.value=NA)+
  scale_color_identity(labels=c(red="Gully", lightblue="Non-gully"), guide="legend")+
  ggtitle ("distribution of training points")+
  coord_sf()


#extracting the attribute presence/absence from the imported shapefiles
gully <-gully_pts$gng
non_gully <- non_gully_pts$gng

#converting imported shapefile to a spatial feature
g_pts_sp <-as(gully_pts, 'Spatial')
ng_pts_sp <-as(non_gully_pts, 'Spatial')

#extracting values from the stacked raster file to the shape file of gully occurrences
gully_ext <- raster::extract(gully_factors_stack, g_pts_sp, method="simple")

head(gully_ext)
tail(gully_ext)

summary(gully_ext)
str(gully_ext)

summary(gully_ext[,1])
View(gully_ext)


#extracting values from the stacked raster file to the shape file of non-gully points
ng_ext <- raster::extract(gully_factors_stack, ng_pts_sp, method="simple")
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


#checking the distribution of continuous factors in both gully and non-gully points
boxplot(aspect ~ presence_absence, data=training_pts, main="Aspect", xlab=" ", ylab="Aspect ", boxwex=0.3, col="lightblue", border="black", names=c("Non-gully", "Gully"))
boxplot(ndvi ~ presence_absence, data=training_pts, main="NDVI", xlab=" ", ylab="NDVI", boxwex=0.3, col="lightblue", border="darkblue", names=c("Non-gully", "Gully"))
boxplot(curvature ~ presence_absence, data=training_pts, main="Curvature", xlab="Curvature", ylab="Curvature", boxwex=0.3, col="lightblue", border="darkblue", names=c("Non-gully", "Gully"))
boxplot(elevation ~ presence_absence, data=training_pts, main="Elevation", xlab="Elevation", ylab="Elevation", boxwex=0.3, col="lightblue", border="darkblue", names=c("Non-gully", "Gully"))
boxplot(erosivity ~ presence_absence, data=training_pts, main="Erosivity", xlab="Erosivity", ylab="Erosivity", boxwex=0.3, col="lightblue", border="darkblue", names=c("Non-gully", "Gully"))
boxplot(slope ~ presence_absence, data=training_pts, main="Slope", xlab="Slope", ylab="Slope ", boxwex=0.3, col="lightblue", border="darkblue", names=c("Non-gully", "Gully"))
boxplot(twi ~ presence_absence, data=training_pts, main="TWI", xlab="TWI", ylab="TWI", boxwex=0.3, col="lightblue", border="darkblue", names=c("Non-gully", "Gully"))

pairs(training_pts)

par(mfrow=c(1,1))

#checking the number of samples in class i.e gully and non-gully points
aggregate(training_pts$soil ~training_pts$presence_absence, FUN=length)


str(training_pts)
summary(training_pts)



#duplicated training data
training_pts1 <- na.omit(training_pts)
summary(training_pts1)
aggregate(training_pts1$soil ~training_pts1$presence_absence, FUN=length)

#converting categorical data to factor data type
training_pts1$landuse <-as.factor(training_pts1$landuse)
training_pts1$geology <-as.factor(training_pts1$geology)
training_pts1$soil <-as.factor(training_pts1$soil)


str(training_pts1)
levels(training_pts1$geology)


library(caret)

#implement one hot coding for categorical data
dummy <- dummyVars(" ~ .", data=training_pts1)
training_df <- data.frame(predict(dummy, newdata=training_pts1))

head(training_df)
names(training_df)

#SPlitting data into training and test data
training_df$random<-runif(length(training_df$presence_absence))
#runif function provides information about the uniform distribution on the interval
#from min to max, to generate random deviates.
train<-training_df[training_df$random<0.70, 1:23] ## the training data
evalu<-training_df[training_df$random>=0.70, 1:23] ## the evaluation data

nrow(train)
nrow(evalu)
names(train)
names(evalu)

library(MASS)
log_model <- suppressWarnings(glm(presence_absence~., data=train, family="binomial")%>% stepAIC(trace=FALSE))

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

raster_df$landuse <-as.factor(raster_df$landuse)
raster_df$geology <-as.factor(raster_df$geology)
raster_df$soil <-as.factor(raster_df$soil)

#applying one hot coding to categorical data
dummy <- dummyVars(" ~ .", data=raster_df)
raster_df <- data.frame(predict(dummy, newdata=raster_df))

raster_df %>%
  colnames

#making prediction for the converted raster dataframe
raster_df$logreg <- log_model %>% predict(raster_df[,3:24], type="response")


#converting the data frame back to raster
gully_ras_logreg <- rasterFromXYZ(raster_df)
projection(gully_ras_logreg) <-projection(gully_factors_stack)



#Minimum and maximum prediction values
min(gully_ras_logreg$logreg@data@values, na.rm = T)
max(gully_ras_logreg$logreg@data@values, na.rm = T)


#Plot the gully susceptibility map
plot(gully_ras_logreg$logreg, main="Gully Susceptibility Map", col=rev(MyPalette(20)))




