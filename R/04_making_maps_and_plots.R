# loading packages
library(ggplot2)
library(viridis)
###### Making a cool map #####
library(tidyverse)
library(sf)
library(dplyr)
library(maps)
library(ggrepel)
library(broom) # to use tidy
# to load data
library(rgdal)
library(raster)
library(rgeos)

library(caret)

##### 1. reading shapefiles ####

# shapefiles
amz <- readOGR("data/shapefile/brazilian_legal_amazon/brazilian_legal_amazon.shp", 
               use_iconv = TRUE, 
               encoding = "UTF-8")
muni.amz <- readOGR("data/shapefile/outputs/muni_amz.shp", 
                use_iconv = TRUE, 
                encoding = "UTF-8")
uc.amz <- readOGR("data/shapefile/outputs/uc_amz.shp", 
                    use_iconv = TRUE, 
                    encoding = "UTF-8")
fire.muni <- readOGR("data/shapefile/outputs/fire_amz80_muni.shp",
                     use_iconv = TRUE, 
                     encoding = "UTF-8")


fire <- raster("data/raster/fire_raster.tif")

# plot(amz)
# plot(uc.amz, add=TRUE, col="grey60")
# plot(fire, col=heat.colors(10)[5:1], add=TRUE)

sp <- readOGR("data/shapefile/pontos_ameacadas_atualizado_portaria_443_2014/pontos_ameacadas_atualizado_portaria_443_2014.shp")

proj4string(sp)
proj4string(fire.muni)
proj4string(amz)

amz <- spTransform(amz, CRS=CRS("+proj=longlat +datum=WGS84"))
sp <- spTransform(sp, CRS=CRS("+proj=longlat +datum=WGS84"))
fire.muni <- spTransform(fire.muni, CRS=CRS("+proj=longlat +datum=WGS84"))

sp.amz <- sp[amz,]

head(sp.amz)

plot(sp.amz)

## reading previous tables
reg.bufs.df <- read.csv("results/records_buffer.csv")
prop.mat <-  read.csv("results/proportion_of_species_records_per_buffer.csv")
prop.mat2 <-  read.csv("results/proportion_of_species_records_per_buffer2.csv")

names(prop.mat) <-  gsub("X", "", names(prop.mat))
names(prop.mat2) <-  gsub("X", "", names(prop.mat2))

prop.mat
prop.mat2

#### 1. making plots ####

cores <- c("grey30", wesanderson::wes_palette("Zissou1", 4, type = "continuous"))
#cores <- wesanderson::wes_palette("Zissou1", 5, type = "continuous")

png("figs/species_buffer.png", res=300, width=1800, height=1400)
matplot(y=prop.mat[,-1], x=prop.mat[,1], type='l',  
     las=1, bty='l', lty=1,
     xlab="Buffer size (km)", 
     ylab="Species with records inside fire", col=cores)
abline(v=10, col="grey80", lty=2)
abline(h=63, col="grey80", lty=2)
matplot(y=prop.mat[,-1], x=prop.mat[,1], type='p',  
        las=1, pch=19,
        xlab="Buffer size (km)", 
        ylab="Species with records inside fire", add=TRUE, 
        col=cores)
legend("topleft", legend=c("at least one record", paste(colnames(prop.mat)[c(-1, -2)], "%")), 
       pch=19, lty=1, col=cores, bty='n')
dev.off()

Nreg.buf <- bind_cols(registros.bufs[8:1])

lista.sp <- sp.10km[!duplicated(sp.10km$nome_cient), ]
table(lista.sp$categoria)

#### 2. creating the maps ####


# 2.1 Fire by municipality ####
#### creating map ####

fire.muni.df <- aggregate(SCAN ~ NOME_MUNI, data=fire.muni, FUN=length)

head(fire.muni.df)
dim(fire.muni)
dim(fire.muni.df)

hist(fire.muni.df$SCAN, plot=FALSE)

# getting county centroids 

head(muni.amz)

my.muni <- muni.amz[muni.amz$NOME_MUNI%in%fire.muni.df$NOME_MUNI,]

as.character(my.muni$NOME_MUNI)

muniCentroids <- gCentroid(my.muni,byid=TRUE)
plot(muni.amz)
points(muniCentroids,pch=19, col="red")

muniCentroids.df <- data.frame(NOME_MUNI=as.character(my.muni$NOME_MUNI), 
                               muniCentroids@coords)

fire.muni.df <- merge(fire.muni.df, muniCentroids.df, by="NOME_MUNI")

dim(fire.muni.df)

head(fire.muni.df)
names(fire.muni.df)[2] <- "Fire_freq"

dim(muni.amz)

muni.amz2 <- merge(as.data.frame(muni.amz), fire.muni.df[,1:2], by="NOME_MUNI", 
                   all.x=TRUE, all.y=FALSE)

muni.amz2 <- muni.amz2[!duplicated(muni.amz2),]

dim(muni.amz2)

muni.amz$Fire_freq <- muni.amz2$Fire_freq

head(muni.amz)

muni.amz

head(muni.amz)

muni.tidy <- tidy(muni.amz)

muni.amz$id <- row.names(muni.amz)

head(muni.amz)

muni.tidy <- dplyr::left_join(muni.tidy, muni.amz@data)

head(muni.tidy)

pal <- wesanderson::wes_palette("Zissou1", 100, type = "continuous")

ggplot(muni.tidy, aes(x = long, y = lat, group = group, fill=Fire_freq)) +
  geom_polygon(color = "black", size = 0.1) +
  scale_fill_gradientn(colours = pal)+
  coord_equal() +
  theme_void() +
  labs(title = "Fire in the Brazilian Amazon") 
  # theme(plot.title = element_text(margin = margin(t = 60, b = -40)))

mybreaks <- c(200, 500, 1000, 1500)

head(fire.muni.df)
head(muni.tidy)

head(fire.10km)

#visualize records

#png("figs/Fire_map.png", res=300, width=1800, height=1600)
ggplot() +
  geom_polygon(data=amz, aes(long, lat, group=group), fill="darkgreen", alpha=0.3) +
fire.tidy <- tidy(fire.10km)

mybreaks <- c(200, 500, 1000, 1500)
sp.tidy <- tidy(sp.10km)

#visualize records
ggplot() +
  geom_polygon(data=amz, aes(long, lat, group=group), fill="darkgreen", alpha=0.3) +
  geom_polygon(data=fire.tidy, aes(long, lat, group=group), fill="darkred", alpha=0.3) +
  geom_polygon(data=uc.amz[uc.amz$nome=="FLORESTA NACIONAL DO JAMANXIM",], 
               aes(long, lat, group=group), alpha=0.3) +
  geom_polygon(data=uc.amz[uc.amz$nome=="PARQUE NACIONAL DO ARAGUAIA",], 
               aes(long, lat, group=group), alpha=0.3) +
  geom_polygon(data=uc.amz[uc.amz$nome=="ESTAÇÃO ECOLÓGICA DA TERRA DO MEIO",], 
               aes(long, lat, group=group), alpha=0.3) +
  geom_point(aes(x=x, y=y, size=Fire_freq, col=Fire_freq),
             data=fire.muni.df, shape=20, stroke=FALSE) +
  geom_point(aes(x=POINT_X, y=POINT_Y),
             data=as.data.frame(sp.10km), shape=3, stroke=FALSE) +
  geom_point(aes(x=POINT_X, y=POINT_Y),
             data=as.data.frame(sp.10km), shape=3, stroke=FALSE) +
  # geom_point(aes(x=lon, y=lat, size=riq, color=riq, alpha=riq), shape=20, stroke=FALSE) +
  #scale_color_gradient(low="orange", high="red") +
  scale_color_gradientn(colours = heat.colors(4)[4:1], name="Fire frequency",  breaks=mybreaks) +
  # geom_point(aes(x=lon, y=lat, size=riq, color=riq, alpha=riq), shape=20, stroke=FALSE) +
  #scale_color_gradient(low="orange", high="red") +
  scale_color_gradientn(colours = heat.colors(5)[5:1]) +
  #scale_color_distiller(palette = "Reds", direction = 1, name="Fire frequency") +
  scale_size_continuous(name="Fire frequency",  breaks=mybreaks) +
  #scale_size_continuous(name="Number of fires") +
  #scale_alpha_continuous(name="Number of fires", breaks=mybreaks) +
  theme_minimal()  + 
  labs(x="Longitude", y="Latitude") +
  coord_map() + 
  #guides( colour = guide_legend()) +
  ggtitle("Fire activity in the Brazilian Amazon")
#dev.off()


# 5.2. Records inside buffer ####

aoo.fogo

head(fire.muni)

library(rasterVis)

myTheme <- rasterTheme(region = rev(heat.colors(n = 20)[1:11]))

levelplot(fire, contour = FALSE, margin=FALSE, par.settings=myTheme) +
  layer(sp.polygons(amz)) + 
  layer(sp.polygons(sp.10km, shape=19))

ggplot() +
  #geom_tile(fire, interpolate = TRUE) +
  geom_point(data = as.data.frame(sp.10km), mapping = aes(x = POINT_X, y = POINT_Y), 
             colour = "yellow",
             alpha = 0.3) +
  coord_fixed()

#png("figs/Fire_map_buffer.png", res=300, width=1800, height=1600)
ggplot() +
  geom_polygon(data=amz, aes(long, lat, group=group), fill="darkgreen", alpha=0.3) +
  geom_polygon(data=fire.10km, aes(long, lat, group=group), fill="white", colour="white", alpha=0.3) +
  geom_polygon(data=fire.10km, aes(long, lat, group=group), fill="yellow", colour="yellow", alpha=0.3) +
  geom_point(aes(x=LONGITUDE, y=LATITUDE),
             data=as.data.frame(fire.muni), shape=46, stroke=FALSE, fill="darkred", colour="darkred") +
  geom_point(aes(x=POINT_X, y=POINT_Y),
             data=as.data.frame(sp.10km), shape=3, stroke=FALSE) +
  # theme(
  #   legend.position = c(0.15, 0.2),
  #   text = element_text(color = "#22211d"),
  #   #plot.background = element_rect(fill = "#f5f5f2", color = NA), 
  #   #panel.background = element_rect(fill = "#f5f5f2", color = NA), 
  #   #legend.background = element_rect(fill = "#f5f5f2", color = NA),
  #   plot.title = element_text(size= 16, hjust=0.1, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
  # )  




ggplot() +
  geom_polygon(data=amz, aes(long, lat, group=group), fill="darkgreen", alpha=0.3) +
  geom_polygon(data=fire.tidy, aes(long, lat, group=group), fill="darkred", alpha=0.5) +
  geom_polygon(data=uc.amz, 
                aes(long, lat, group=group), alpha=0.3) +
  # geom_polygon(data=uc.amz[uc.amz$nome=="PARQUE NACIONAL DO ARAGUAIA",], 
  #              aes(long, lat, group=group), alpha=0.3) +
  # geom_polygon(data=uc.amz[uc.amz$nome=="ESTAÇÃO ECOLÓGICA DA TERRA DO MEIO",], 
  #              aes(long, lat, group=group), alpha=0.3) +
  # geom_point(aes(x=x, y=y, size=Fire_freq, col=Fire_freq),
  #            data=fire.muni.df, shape=20, stroke=FALSE) +
  geom_point(aes(x=POINT_X, y=POINT_Y),
             data=as.data.frame(sp.10km), shape=3, stroke=FALSE) +
  # geom_point(aes(x=lon, y=lat, size=riq, color=riq, alpha=riq), shape=20, stroke=FALSE) +
  #scale_color_gradient(low="orange", high="red") +
  #scale_color_gradientn(colours = heat.colors(5)[5:1]) +
  #scale_color_distiller(palette = "Reds", direction = 1, name="Fire frequency") +
  #scale_size_continuous(name="Fire frequency",  breaks=mybreaks) +
  #scale_size_continuous(name="Number of fires") +
  #scale_alpha_continuous(name="Number of fires", breaks=mybreaks) +
  theme_minimal()  + 
  labs(x="Longitude", y="Latitude") +
  coord_map() + 
  #guides( colour = guide_legend()) +
  ggtitle("Fire activity in the Brazilian Amazon")



ggplot() +
  geom_polygon(data = amz, aes(x=long, y = lat, group = group), fill="forestgreen", alpha=0.3) +
  geom_point(data = as.data.frame(fire.9km2), aes(x = LONGITUDE, y = LATITUDE),
             colour = "darkred", size = 0.5)

