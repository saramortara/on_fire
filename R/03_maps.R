library(ggplot2)
library(viridis)
###### Making a cool map #####
library(tidyverse)
library(maps)
library(ggrepel)
library(broom) # to use tidy
# to load data
library(rgdal)
library(raster)
library(rgeos)

# glaucia data
# g1 <- read.csv("data/Especies_fogo01.csv", sep=";")
# g2 <- read.csv("data/Especies_fogo02.csv", sep=";")

# head(g1)
# head(g2)

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

sp <- readOGR("data/shapefile/pontos_ameacadas_atualizado_portaria_443_2014/pontos_ameacadas_atualizado_portaria_443_2014.shp")

proj4string(sp)
proj4string(fire.muni)
proj4string(amz)

amz <- spTransform(amz, CRS=CRS("+proj=longlat +datum=WGS84"))
sp <- spTransform(sp, CRS=CRS("+proj=longlat +datum=WGS84"))
fire.muni <- spTransform(fire.muni, CRS=CRS("+proj=longlat +datum=WGS84"))

sp.amz <- sp[amz,]

head(sp.amz)

sp.fora <- sp[!sp$codigocncf%in%sp.amz$codigocncf,]

sum(sp.fora$codigocncf%in%sp.amz$codigocncf)

sp.restritas <- setdiff(sp.amz$nome_cient, sp.fora$nome_cient) %>% 
  length()

sp.tot <- unique(sp.amz$nome_cient) %>% length() 

sp.restritas/sp.tot

dim(sp.fora)+dim(sp.amz)
dim(sp)

sp.amz$nome_cient <- as.character(sp.amz$nome_cient)

listasp.amz <- as.character(unique(sp.amz$nome_cient))
listasp.all <- as.character(unique(sp$nome_cient))

registros.all <- as.data.frame(table(sp.amz$nome_cient))
names(registros.all)[2] <- "Freq.all"

dim(sp.amz)

head(sp)

# creating buffer
c1 <- 1
c2 <- 9
r1 <- sqrt(c1/pi)*1000
r2 <- sqrt(c2/pi)*1000
area10km <- pi*(10^2)

b1 <- 2000
b2 <- 5000
b3 <- 10000

# fire.2km <- buffer(fire.muni, width = b1, dissolve = TRUE)
# fire.5km <- buffer(fire.muni, width = b2, dissolve = TRUE)
fire.10km <- buffer(fire.muni, width = b3, dissolve = TRUE)

# fire.1km2 <- buffer(fire.muni, width = r1, dissolve = TRUE)
# fire.9km2 <- buffer(fire.muni, width = r2, dissolve = TRUE)

# sp inside fire only buffer 9km
# buf_list <- list(fire.2km, fire.5km, fire.10km)

# sp.2km <- sp.amz[fire.2km,]
# length(unique(sp.2km$nome_cient))
# dim(sp.2km)
# 
# sp.5km <- sp.amz[fire.5km,]
# length(unique(sp.5km$nome_cient))
# dim(sp.5km)

sp.10km <- sp.amz[fire.10km,]
length(unique(sp.10km$nome_cient))
dim(sp.10km)
registros.fogo <- as.data.frame(table(sp.10km$nome_cient))

# comparando os registros dentro e fora do fogo
registros <- merge(registros.all, registros.fogo, by="Var1")

sp.amz <- nrow(registros.all)
sp.fogo <- nrow(registros.fogo)

sp.fogo/sp.amz

registros$prop <- registros$Freq/registros$Freq.all

length(registros$Var1[registros$prop==1])

lista.sp <- sp.10km[!duplicated(sp.10km$nome_cient), ]
table(lista.sp$categoria)

# testing plot
plot(fire.10km, col="darkred")
# plot(fire.5km, col="orange", add=TRUE)
# plot(fire.2km, col="yellow", add=TRUE)
#plot(sp.10km, col="orange", pch=19, add=TRUE)

# sp.9km2.df <- as.data.frame(sp.9km2) 

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

png("figs/Fire_map.png", res=300, width=1800, height=1600)
ggplot() +
  geom_polygon(data=amz, aes(long, lat, group=group), fill="darkgreen", alpha=0.3) +
  geom_point(aes(x=x, y=y, size=Fire_freq, col=Fire_freq),
             data=fire.muni.df, shape=20, stroke=FALSE) +
  geom_point(aes(x=POINT_X, y=POINT_Y),
             data=as.data.frame(sp.10km), shape=3, stroke=FALSE) +
  # geom_point(aes(x=lon, y=lat, size=riq, color=riq, alpha=riq), shape=20, stroke=FALSE) +
  #scale_color_gradient(low="orange", high="red") +
  scale_color_gradientn(colours = heat.colors(4)[4:1], name="Fire frequency",  breaks=mybreaks) +
  #scale_color_distiller(palette = "Reds", direction = 1, name="Fire frequency") +
  scale_size_continuous(name="Fire frequency",  breaks=mybreaks) +
  #scale_size_continuous(name="Number of fires") +
  #scale_alpha_continuous(name="Number of fires", breaks=mybreaks) +
  theme_minimal()  + 
  labs(x="Longitude", y="Latitude") +
  coord_map() + 
  #guides( colour = guide_legend()) +
  ggtitle("Fire activity in the Brazilian Amazon")
dev.off()


#png("figs/Fire_map_buffer.png", res=300, width=1800, height=1600)
ggplot() +
  geom_polygon(data=amz, aes(long, lat, group=group), fill="darkgreen", alpha=0.3) +
  geom_polygon(data=fire.10km, aes(long, lat, group=group), fill="white", colour="darkred", alpha=0.3) +
  geom_polygon(data=fire.10km, aes(long, lat, group=group), fill="red", colour="darkred", alpha=0.3) +
  #geom_polygon(data=uc.amz, 
  #              aes(long, lat, group=group), alpha=0.3) +
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

