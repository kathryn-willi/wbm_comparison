#####################################################################
###   Pulling Site Parameters for D. Thoma Water Balance Model ######
#####################################################################
library(raster)
library(sf)
library(dplyr)
library(WaterBalance)
# Set projection to be used for all spatial data:

rm(list=ls())

# select park

SiteID <- "ROMO"
if(!dir.exists("./Outputs/"))dir.create("./Outputs/")

epsg <- 5070 # North American Albers Equal Area Conic

###  Spatial Data  #####
# This data is available from NPS

# rasters - use proj4 to define projection

maca <- raster('./data/tdn_90d.nc') 
maca <- projectRaster(maca, crs = epsg)
dem <- raster('./data/elevation_cropped.tif') #will get warning because unprojected
dem <- projectRaster(dem, crs = epsg)
soil <- raster('./data/water_storage.tif') #will get warning because unprojected
soil <- projectRaster(soil, crs= epsg)

nps_centroids <- st_read('./data/nps_boundary_centroids/nps_boundary_centroids.shp')
nps_centroids <- st_transform(nps_centroids, st_crs(maca))

centroid <- filter(nps_centroids, UNIT_CODE == SiteID) # use this line if using park centroid

# Obtain MACA grid outline (not information within)

centroid<- as_Spatial(centroid) # objects must be Spatial (sp) to work with raster package (cannot be sf)
cell <- cellFromXY(maca, centroid) # find grid cell park centroid lies within
maca_cell <- rasterFromCells(maca, cell) # create stand-alone raster for single MACA cell
maca.poly <- rasterToPolygons(maca_cell) # Create MACA polygon - original file in lat/long (note: datum differs from park shapefiles)

# Plot to see that MACA cell is visible and appropriately located within park

#####   SLOPE, ASPECT AND RANDOM POINTS   ##########################################################################################

# Create slope and aspect rasters

slope <- terrain(dem, opt = "slope", unit = "degrees", neighbors = 4) # 4 is better for "smooth" surfaces; 8 is better for rough. See https://www.rdocumentation.org/packages/raster/versions/3.1-5/topics/terrain
aspect <- terrain(dem, opt = "aspect", unit = "degrees")

# get 10 random points from soil raster and create SpatialPoints object
points <- spsample(maca.poly, n = 10, type = "random") 

####    EXTRACT DATA FROM POINTS  ######################################################################################################

# reproject points to lat/long so can eventually add to .csv

latlong <- st_as_sf(points) # convert to sf object 
latlong <- st_transform(latlong, crs = 4326) # project to lat/long

wb_sites <- as.data.frame(st_coordinates(latlong)) # begin new dataframe for wb_sites

wb_sites[,3] <- raster::extract(dem, points)
wb_sites[,4] <- raster::extract(aspect, points)
wb_sites[,5] <- raster::extract(slope, points)
wb_sites[,6] <- raster::extract(soil, points)
wb_sites[,7] <- seq.int(nrow(wb_sites))
wb_sites[,8] <- 5 # default value for wind
wb_sites[,9] <- 0 # default value for snowpack
wb_sites[,10] <- 0 # default value for Soil.Init
wb_sites[,11] <- 1 # default value for shade coefficient

wb_sites <- select(wb_sites, 7,2,1,3:6, 8:11) # reorder columns
colnames(wb_sites) <- c("WB_site", "Lat", "Lon", "Elev", "Aspect", "Slope", "SWC.Max", "Wind", "Snowpack", "Soil.Init", "Shade.Coeff")

wb_sites$SWC.Max = wb_sites$SWC.Max*10 # convert units for Soil Water-holding capacity
wb_sites # check to be sure values are populated correctly. There should not be NA values. 

write.csv(wb_sites, file = paste('./Outputs/', SiteID, " WB site parameters ", Sys.Date(), ".csv", sep = ""), row.names = FALSE)


