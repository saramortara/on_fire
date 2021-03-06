#### Script to create buffer from fire data ####

# loading packages
library(rgdal)
library(raster)
library(maptools)
library(lubridate)
library(maps)
library(stringr)

# reading legal amazon shp
amz <- readOGR("data/shapefile/brazilian_legal_amazon/brazilian_legal_amazon.shp",
               use_iconv = TRUE, 
               encoding = "UTF-8")
uc.amz <- readOGR("data/shapefile/outputs/uc_amz.shp",
                 use_iconv = TRUE, 
                 encoding = "UTF-8")
# uc
fire.uc <- readOGR("data/shapefile/outputs/fire_amz80_uc.shp", 
              use_iconv = TRUE, 
              encoding = "UTF-8")
# muni
fire.muni <- readOGR("data/shapefile/outputs/fire_amz80_muni.shp", 
                use_iconv = TRUE, 
                encoding = "UTF-8") 

# 1. Creating raster from fire data ####
# reading fire shapefile (legal amazon, >80 confidence)
fire80 <- readOGR("data/shapefile/outputs/fire_amz80.shp")
#fire80$DATE <- ymd(fire80$DATE)

range(fire80$DATE)

dim(fire80)

Nfire <- nrow(fire80)

# remove fire type NA
head(fire80)

# transforming into raster
fire80$id <- 1

rast <- raster()
my_ext <- extent(fire80)

extent(rast) <- my_ext

r <- rasterize(fire80, rast, fire80$id, fun='sum')

# inicial resolution
res_i <- res(r)
# final resolution
res_j <- c(0.008, 0.008)
# desired factor
fact <- round(res_i/res_j, 2)

fire.rast <- disaggregate(r, fact=fact)

plot(fire.rast)

# write raster
writeRaster(fire.rast,
            "data/raster/fire_raster.tif", overwrite=TRUE)

my_green <- rgb(53, 136, 86, max = 255, alpha = 100)
# ploting fire map
plot(fire.rast, col="red", legend=FALSE, las=1)
map(,,,add=TRUE)
plot(amz, col=NA, lwd=2, add=TRUE)


# 2. Calculating fire per UC ####
head(fire.uc)
head(uc.amz)

Nuc.fire <- length(unique(fire.uc$nome))
Nuc.amz <- length(unique(uc.amz$nome))

Nuc.fire/Nuc.amz

dim(fire80)

fire.uc.df <- aggregate(SCAN ~ nome, data=fire.uc, FUN=length)
fire.uc.df$nome <- as.character(fire.uc.df$nome)

dim(fire.uc.df) # 42 UCs

fire.uc.df[order(fire.uc.df$SCAN, decreasing=TRUE),]

uc.type <- c("FLORESTA NACIONAL", "PARQUE NACIONAL", "ESTAÇÃO ECOLÓGICA", 
             "ÁREA DE PROTEÇÃO AMBIENTAL", "RESERVA BIOLÓGICA", "RESERVA EXTRATIVISTA")

fire.uc.df$type <- NA

for(i in 1:length(uc.type)){
fire.uc.df$type[str_detect(fire.uc.df$nome, uc.type[i])] <- uc.type[i]
}

fire.uc.df

sum(fire.uc.df$SCAN)/Nfire # 5% do fogo em UC

table(fire.uc.df$type)

fire.uctype <- aggregate(SCAN ~ type, data=fire.uc.df, FUN=sum) 
fire.uctype <- fire.uctype[order(fire.uctype$SCAN, decreasing=TRUE),]

fire.uctype

sum(fire.uctype$SCAN) #1,207


# 2. Calculating fire per municipality ####
head(fire.muni)

fire.mu.df <- aggregate(SCAN ~ NOME_MUNI, data=fire.muni, FUN=length)
fire.mu.df$NOME_MUNI <- as.character(fire.mu.df$NOME_MUNI)

dim(fire.mu.df) #424

fire.mu.df <- fire.mu.df[order(fire.mu.df$SCAN, decreasing=TRUE),]

fire.mu.df$cum.sum <- cumsum(fire.mu.df$SCAN)
fire.mu.df$prop <- fire.mu.df$cum.sum/sum(fire.mu.df$SCAN)

tail(fire.mu.df)

head(fire.mu.df, 20)

