#### Reading fire data ##################################
## Sript to: 
## clip fire shapefile to Legal Amazon
## convert date format in files
## export csv files with shp info 

# loading packages
library(rgdal)
library(lubridate) # for date changes
library(raster) # to use bind()
library(spatialEco) # points.in.poly
library(rgeos)

# 1. reading Legal Amazon shapefile, brazilian municipalties and conservation units ####
amz <- readOGR("data/shapefile/brazilian_legal_amazon/brazilian_legal_amazon.shp") # terrabrasilis
muni <- readOGR("data/shapefile/municipios_br/MUNICIPIOS_polígonos.shp",
                use_iconv = TRUE, 
                encoding = "UTF-8") 
uc <- readOGR("data/shapefile/uc_fed_junho_2018_shp/UCs_fed_junho_2018.shp",
              use_iconv = TRUE, 
              encoding = "UTF-8")

## setting the same projection
amz <- spTransform(amz, CRS=CRS("+proj=longlat +datum=WGS84"))
muni <- spTransform(muni, CRS=CRS("+proj=longlat +datum=WGS84"))
uc <- spTransform(uc, CRS=CRS("+proj=longlat +datum=WGS84"))

## clipping shp
muni.amz <- muni[amz,]
uc.amz <- uc[amz,]

## exporting shapefiles
#new.dir <- dir.create("data/shapefile/outputs")

# exporting file
writeOGR(muni.amz, 
         "data/shapefile/outputs", 
         "muni_amz", 
         driver="ESRI Shapefile", overwrite_layer = TRUE)

writeOGR(uc.amz, 
         "data/shapefile/outputs", 
         "uc_amz", 
         driver="ESRI Shapefile", overwrite_layer = TRUE)

# 2. reading fire data ####
fire19 <- readOGR("data/shapefile/fire_modis2019/fire_nrt_M6_71094.shp")
#fire <- readOGR("data/shapefile/fire_modis2016_2019/fire_archive_M6_67612.shp")

## setting the same projection
#fire <- spTransform(fire, CRS=CRS("+proj=longlat +datum=WGS84"))
fire19 <- spTransform(fire19, CRS=CRS("+proj=longlat +datum=WGS84"))

## clipping to legal amazon
#fire1.amz <- fire[amz,]
fire2.amz <- fire19[amz,]

fire2.amz$DATE <- ymd(fire2.amz$ACQ_DATE)
fire2.amz$YEAR <- year(fire2.amz$DATE)

range(fire2.amz$DATE)

## selecting only fire data w/ >80 confidence
fire.amz80 <- fire2.amz[fire2.amz$CONFIDENCE>80,]

head(fire.amz80)

range(fire.amz80$DATE)

table(fire.amz80$DATE>"2019-07-31")

# selecting only data from august 2019 
fire.amz80.aug <- fire.amz80[fire.amz80$DATE<"2019-09-01",] 

range(fire.amz80.aug$DATE)

# Fire per municipality and UC
fire.muni <- point.in.poly(fire.amz80.aug, muni.amz)

fire.uc <- point.in.poly(fire.amz80.aug, uc.amz)

# exporting shapefile 80 and legal amazon and august
writeOGR(fire.amz80.aug, 
         "data/shapefile/outputs", 
         "fire_amz80", 
         driver="ESRI Shapefile", 
         overwrite_layer = TRUE)

# exporting shapefile 80 and legal amazon and county
writeOGR(fire.muni, 
         "data/shapefile/outputs", 
         "fire_amz80_muni", 
         driver="ESRI Shapefile",
         overwrite_layer = TRUE)

# exporting shapefile 80 and legal amazon and uc
writeOGR(fire.uc, 
         "data/shapefile/outputs", 
         "fire_amz80_uc", 
         driver="ESRI Shapefile",
         overwrite_layer = TRUE)

