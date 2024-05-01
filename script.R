
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

