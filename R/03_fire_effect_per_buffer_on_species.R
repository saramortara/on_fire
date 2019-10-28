# loading packages
library(tidyverse)
library(sf)
library(dplyr)
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


#fire <- raster("data/raster/fire_raster.tif")

# plot(amz)
# plot(uc.amz, add=TRUE, col="grey60")
# plot(fire, col=heat.colors(10)[5:1], add=TRUE)

sp <- readOGR("data/shapefile/pontos_ameacadas_atualizado_portaria_443_2014/pontos_ameacadas_atualizado_portaria_443_2014.shp")

dim(sp)
head(sp)
length(unique(sp$nome_cient))

proj4string(sp)
proj4string(fire.muni)
proj4string(amz)

amz <- spTransform(amz, CRS = CRS("+proj=longlat +datum=WGS84"))
sp <- spTransform(sp, CRS = CRS("+proj=longlat +datum=WGS84"))
fire.muni <- spTransform(fire.muni, CRS = CRS("+proj=longlat +datum=WGS84"))

sp.amz <- sp[amz,]

head(sp.amz)

plot(sp.amz)

#### 2. Checking species data #### 

# 2.1 checking endemic species ####
sp.fora <- sp[!sp$codigocncf %in% sp.amz$codigocncf,]

sum(sp.fora$codigocncf %in% sp.amz$codigocncf)

sp.restritas <- setdiff(sp.amz$nome_cient, sp.fora$nome_cient) %>% 
  length()

sp.tot <- unique(sp.amz$nome_cient) %>% length() 

sp.restritas/sp.tot

nrow(sp.fora) + nrow(sp.amz)
nrow(sp)

dim(sp.fora) + dim(sp.amz)
dim(sp)

sp.amz$nome_cient <- as.character(sp.amz$nome_cient)

listasp.amz <- as.character(unique(sp.amz$nome_cient))
listasp.all <- as.character(unique(sp$nome_cient))

registros.amz <- as.data.frame(table(sp.amz$nome_cient))
names(registros.amz) <- c("nome_cient", "total")

head(registros.amz)

head(sp.amz)

df <- sp.amz@data 

head(df)

nome.cat <- df[!duplicated(df$nome_cient), c('nome_cient', 'categoria')]

head(nome.cat)

write.table(nome.cat, "results/especie_categoria.csv",
            row.names = FALSE,
            col.names = TRUE)

# 2.2 Calculating AOO ####
c.aoo <- 4
r.aoo <- sqrt(c.aoo/pi)*1000 

sp.aoo <- raster::buffer(sp.amz, width = r.aoo, dissolve = TRUE)

plot(sp.aoo)

#### 3. Creating fire buffer #### 
registros.all <- as.data.frame(table(sp.amz$nome_cient))
names(registros.all)[2] <- "Freq.all"

dim(sp.amz)

head(sp)

# creating buffer
# c1 <- 1
# c2 <- 9
# r1 <- sqrt(c1/pi)*1000
# r2 <- sqrt(c2/pi)*1000
# area10km <- pi*(10^2)
# b1 <- 2000
# b2 <- 5000

b3 <- 10000
fire.10km <- buffer(fire.muni, width = b3, dissolve = TRUE)

# 3.1. Creating multiple buffers ####

bufs <- function(b) {
  my.buf <- buffer(fire.muni, width = b, dissolve = TRUE)
}

bs <- c(1000, 1500, 2000, 3000, 5000, 7000, 10000, 14000)

fire.bufs <- lapply(bs, bufs) 

fire.bufs

#### 4. Species inside fire #### 

# 4.1 N of records inside fire ####
sp.10km <- sp.amz[fire.10km,]
length(unique(sp.10km$nome_cient))
dim(sp.10km)

writeOGR(sp.10km, 
         "data/shapefile/outputs", 
         "sp_10km", 
         driver = "ESRI Shapefile", overwrite_layer = TRUE)

registros.fogo <- as.data.frame(table(sp.10km$nome_cient))
names(registros.fogo)[2] <- "Freq.fire"

# comparando os registros dentro e fora do fogo
registros <- merge(registros.amz, registros.fogo, by="Var1")

nsp.amz <- nrow(registros.amz)
nsp.fogo <- nrow(registros.fogo)

nsp.fogo/nsp.amz

registros$prop <- registros$Freq.fire/registros$Freq

# 4.1. para os diferentes tamanhos de buffer ####
sp.bufs <- list()
for(i in 1:length(bs)){
  sp.bufs[[i]] <- sp.amz[fire.bufs[[i]],]
}

sp.buf.n <- sapply(sp.bufs, function(x) length(unique(x$nome_cient)))

lista.sp.buf <- lapply(sp.bufs, function(x) unique(x$nome_cient))
#lista.sp.buf[[7]]%in%lista.sp.buf[[8]]

sp.buf.df <- data.frame(buffer=bs/1000, 
                        prop.sp=round(sp.buf.n/nsp.amz, 2)*100)


sp.buf.df

mod <- lm(prop.sp ~ poly(buffer, 2), data=sp.buf.df)
mod.c <- coef(mod)
pred <- data.frame(buffer=1:15)
predicted.intervals <- predict(mod, pred,
                               interval='confidence',
                               level=0.99)

plot(sp.buf.df, las=1, bty='l', 
     xlab="Buffer size (km)", 
     ylab="Percentage of species affected")
lines(pred$buffer,predicted.intervals[,1],col='tomato',lwd=2)
lines(pred$buffer,predicted.intervals[,2],col='black',lwd=1, lty=1)
lines(pred$buffer,predicted.intervals[,3],col='black',lwd=1, lty=1)
abline(v=10, col="grey80", lty=2)
abline(h=50, col="grey80", lty=2)

reg.bufs <- list()
for(i in 1:length(bs)){
reg.bufs[[i]] <-as.data.frame(table(sp.bufs[[i]]$nome_cient))
#names(reg.bufs[[i]])[2] <- paste0("buffer_", bs[i])
reg.bufs[[i]]$Var1 <- as.character(reg.bufs[[i]]$Var1)
names(reg.bufs[[i]])[1] <- "nome_cient" 
}

reg.bufs.df <- bind_rows(reg.bufs, .id="buffer") %>%
    merge(., registros.amz, by="nome_cient", sort=FALSE) %>%
    merge(., nome.cat, by="nome_cient", sort=FALSE)

reg.bufs.df$prop <- reg.bufs.df$Freq/reg.bufs.df$total
head(reg.bufs.df)

write.table(reg.bufs.df, "results/records_buffer.csv",
            col.names=TRUE, row.names=FALSE, sep=",")

props <- c(0, 0.29, 0.49, 0.74, 0.89)
#prop.mat <- as.data.frame(matrix(NA, nrow=length(bs)*3, ncol=length(props)+2))
prop.mat <- as.data.frame(matrix(NA, nrow=length(bs), ncol=length(props)+1))
prop.mat[,1] <- bs/1000
#prop.mat[,2] <- rep(c("CR", "EN", "VU"), each=length(bs))
#names(prop.mat) <- c("buffer", "categoria", "1", "30","50", "75", "90")
names(prop.mat) <- c("buffer", "1", "30","50", "75", "90")
for(i in 1:length(props)){
    prop.mat[,i+1] <- aggregate(reg.bufs.df$prop,
                                list(reg.bufs.df$buffer), #, reg.bufs.df$categoria),
                                drop=FALSE,
                                function(x) sum(x>props[i]))$x
}

prop.mat

write.table(prop.mat, "results/proportion_of_species_records_per_buffer.csv",
            col.names=TRUE, row.names=FALSE, sep=",")


cores <- c("grey30", wesanderson::wes_palette("Zissou1", 4, type = "continuous"))
#cores <- wesanderson::wes_palette("Zissou1", 5, type = "continuous")

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


# 4.2 AOO inside fire ####

# calculando aoo dentro do fogo

head(registros)

sp.aoo
fire.10km

aoo.fogo <- raster::intersect(sp.aoo, fire.10km)

aoo.fogo


plot(sp.aoo, axes=TRUE); plot(aoo.fogo, add=TRUE, col="red")

aoo.fogo

plot(fire.10km, axes=T); plot(sp.aoo, add=T); plot(aoo.fogo, add=T, col='red')

# Extract areas from polygon objects then attach as attribute
sp.aoo$area <- area(sp.aoo) / 1000000
aoo.fogo$area <- area(aoo.fogo) / 1000000

aoo.fogo$area/sp.aoo$area # 26% da area total dentro do fogo

# testing plot
plot(fire.10km, col="darkred")
# plot(fire.5km, col="orange", add=TRUE)
# plot(fire.2km, col="yellow", add=TRUE)
#plot(sp.10km, col="orange", pch=19, add=TRUE)

# sp.9km2.df <- as.data.frame(sp.9km2) 
