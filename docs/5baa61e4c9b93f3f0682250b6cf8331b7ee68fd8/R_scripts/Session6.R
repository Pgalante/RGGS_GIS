## The purpose of this lab is to learn how to manipulate shapefiles through many means
# and to learn how to select small patches of remnant rainforest on freehold or leasehold
# land in the Daintree River catchment in the Wet Tropics of Far North Queensland, Australia.

# Load the libraries to use
library(rgdal)
library(raster)
library(maps)
library(sp)
library(rgeos)
# Set the working directory to where your shapefiles are
setwd('C:/Users/pgalante/Documents/Projects/QGIS_tutorial/RGGS_GIS/Fragmentation_Lab/vector')
# Get the directory of your vector files
vectorwd<-'C:/Users/pgalante/Documents/Projects/QGIS_tutorial/RGGS_GIS/Fragmentation_Lab/vector'
# Get the directory of your raster files
rasterwd<-"C:/Users/pgalante/Documents/Projects/QGIS_tutorial/RGGS_GIS/Fragmentation_Lab/raster"
# load in the shapefiles we will be using. 
coast<- readOGR(dsn = paste(vectorwd, sep='') , layer = 'coast')
daintree_catch<- readOGR(dsn = paste(vectorwd, sep=''), layer = 'daintree_catch')
dcdb_clip<- readOGR(dsn = paste(vectorwd, sep=''), layer = 'dcdb_clip')
rainforest_clip<- readOGR(dsn = paste(vectorwd, sep=''), layer = 'rainforest_clip')
rivers<- readOGR(dsn = paste(vectorwd, sep=''), layer = 'rivers_clip')
roads<- readOGR(dsn = paste(vectorwd, sep=''), layer = 'roads')
town<- readOGR(dsn = paste(vectorwd, sep=''), layer = 'town')
# Load in the raster to use
daintree_catchment_landsat<-raster(paste(rasterwd, "daintree_catchment_landsat.tif", sep='/'))

## First, we want to select all of the rainforest patches between 50 and 100 hectares in area. 
rainforest_clip_selection <- rainforest_clip[rainforest_clip@data$Hectares>50 & rainforest_clip@data$Hectares < 100,] 
plot(rainforest_clip_selection)
# Next, we want to identify all freehold and leasehold land in the Daintree Catchment.
# To see the available Tenure options check the levels
levels(dcdb_clip@data$TENURE)
# Now, subset by freehold then leasehold, then rbind the two into one shape
FH<-subset(dcdb_clip, TENURE == "FH")
LL<-subset(dcdb_clip, TENURE == "LL")
freehold_leasehold<-rbind(FH, LL)
# Let's plot this and take a look
plot(freehold_leasehold)

## Find which forest patches thas have centroids in freeholds or leaseholds.
# Get the centroids of the rainforest patches
rainforest.coords<-SpatialPoints(coordinates(rainforest_clip_selection))
# Set the CRS as the same as the rest of the data
crs(rainforest.coords)<-'+proj=utm +zone=55 +south +ellps=GRS80 +units=m +no_defs'
# Find out which patches have their centroid in a freehold or leasehold
freehold.patches<-over(rainforest.coords, freehold_leasehold)
# Get only those polygons from the rainforest clip, while removing NAs
rainforest_freelease <- rainforest_clip_selection[!is.na(over(rainforest.coords,as(freehold_leasehold,"SpatialPolygons"))),]
plot(rainforest_freelease)

## Now, we want to calculate the shape index of this new shapefile, then buffer the inside of the polygons by 50 meters to find the core areas. 
# Add SI as a column, and divide "PERIMETER" by 2*square root("AREA")
rainforest_freelease@data$SI<-(rainforest_freelease@data$PERIMETER/(2*sqrt(rainforest_freelease@data$AREA)))
# Buffer the inside of the polygons
rainforest_cores<-SpatialPolygonsDataFrame(Sr = gBuffer(rainforest_freelease, width = -50, byid = T ), data = rainforest_freelease@data)
plot(rainforest_cores)
# Notice that the polygons get smaller.If you look closely, you'll notice that some shapes fragmented into smaller shapes.
# Now, we want to calculate the area of each core. 
rainforest_cores@data$Core_area<-sapply(slot(rainforest_cores, "polygons"), slot, "area")
# Finally, lets calculate the ratio of core area to patch area. 
rainforest_cores@data$CA_ratio<-rainforest_cores@data$AREA/rainforest_cores@data$Core_area




