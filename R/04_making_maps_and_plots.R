# loading packages
library(ggplot2)
library(gridExtra)
#library(viridis)
#library(awtools)
###### Making a cool map #####
library(tidyverse)
library(sf)
library(dplyr)
#library(maps)
#library(ggrepel)
#library(broom) # to use tidy
# to load data
library(rgdal)
library(raster)
library(rgeos)
library(caret)
library(reshape2)
library(wesanderson)

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
sp.10km <- readOGR("data/shapefile/outputs/sp_10km.shp",
                     use_iconv = TRUE, 
                     encoding = "UTF-8")

fire <- raster("data/raster/fire_raster.tif")

SA <- readOGR('data/shapefile/SA/SouthAmerica.shp')

# plot(amz)
# plot(uc.amz, add=TRUE, col="grey60")
# plot(fire, col=heat.colors(10)[5:1], add=TRUE)

sp <- readOGR("data/shapefile/pontos_ameacadas_atualizado_portaria_443_2014/pontos_ameacadas_atualizado_portaria_443_2014.shp")

proj4string(sp)
proj4string(fire.muni)
proj4string(amz)

amz <- spTransform(amz, CRS = CRS("+proj=longlat +datum=WGS84"))
sp <- spTransform(sp, CRS = CRS("+proj=longlat +datum=WGS84"))
fire.muni <- spTransform(fire.muni, CRS = CRS("+proj=longlat +datum=WGS84"))

sp.amz <- sp[amz,]

head(sp.amz)

plot(sp.amz)

## identifying endemic species
sp.fora <- sp[!sp$codigocncf %in% sp.amz$codigocncf,]
sp.restritas <- setdiff(sp.amz$nome_cient, sp.fora$nome_cient)
sp.amz$endemicas <- ifelse(sp.amz$nome_cient %in% sp.fora$nome_cient,
                           "non endemic",
                           "endemic")
lista.endemicas <- sp.amz[!duplicated(sp.amz$nome_cient),c("endemicas", "nome_cient")]
sp.10km <- merge(sp.10km, lista.endemicas, by = 'nome_cient')


nsp.restritas <- length(sp.restritas)
sp.tot <- unique(sp.amz$nome_cient) %>% length() 
nsp.restritas/sp.tot


## reading previous tables
reg.bufs.df <- read.csv("results/records_buffer.csv")
prop.mat <-  read.csv("results/proportion_of_species_records_per_buffer.csv")
prop.mat2 <-  read.csv("results/proportion_of_species_records_per_buffer2.csv")

buf.df <- data.frame(buffer.size = prop.mat$buffer, 
                     buffer = 1:nrow(prop.mat))

reg.bufs.end <- merge(reg.bufs.df,
                      sp.amz[,c('nome_cient', 'endemicas')],
                      by = "nome_cient") %>%
    merge(., buf.df, by = "buffer")

head(reg.bufs.end)

end10 <- filter(reg.bufs.end, buffer.size == 10) %>%
    .[!duplicated(.$nome_cient),]

head(sp.amz)

table(end10$endemicas)/nrow(end10)

#write.table(reg.bufs.end[,-1],
#            "results/registros_especies_fogo.csv",
#            col.names=TRUE, row.names=FALSE,
#            sep=",")

#write.table(data.frame(nome_cient=sort(unique(reg.bufs.end$nome_cient))),
#            "results/lista_especies_fogo.csv",
#            col.names=TRUE, row.names=FALSE,
#            sep=",")

names(prop.mat) <-  gsub("X", "", names(prop.mat))
names(prop.mat2) <-  gsub("X", "", names(prop.mat2))

## creating data.frames

prop.mat2

df <- melt(prop.mat, id.vars = "buffer")
df2 <- melt(prop.mat2[,-1], id.vars = "categoria")
df2$buffer <- prop.mat2$buffer

df2$variable <- gsub("1", "> 0", df2$variable)
df$variable <- gsub("1", "> 0", df$variable)


head(df2)


#### 1. making plots ####

#cores <- wes_palette("Cavalcanti1")[c(2,4,3)]
cores <- c("#2A363B", "#019875", "#99B898") # a_palette[1:3]
cores

## barplot
bp <- df2[df2$variable != 75 & df2$buffer == 10, ] %>%
  ggplot(., aes(fill = categoria, x = variable, y = value / 126)) +
  geom_bar(position = "fill", stat = "identity") +
  scale_fill_manual("Category",
                    values = cores) +
  theme_classic() +
  #ggtitle("B")+  ## Impact of 10 km buffer on endangered species)+
  labs(y = "Proportion of species loss",
       x = "Percentage of lost distribution") +
  theme(text = element_text(size = 15),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        axis.text.x = element_text(),
        axis.text.y = element_text())
#legend.position = c(0.5, 0.985),
#legend.direction = "horizontal")


bp + coord_flip()

colo <- wes_palette("GrandBudapest1")[2]

lcor <- cartography::carto.pal('grey.pal', 4)[4:1]
lcor

## line plot
lp <- df[df$variable != 75, ] %>%
  ggplot(., aes(
    x = buffer,
    y = value / 126,
    group = variable,
    colour = variable
  )) +
  #scale_colour_grey("% of lost records") +
  scale_colour_manual("% of lost records", values = lcor) +
  geom_vline(xintercept = 10,
             colour = colo,
             linetype = "dashed") +
  geom_hline(yintercept = 0.50,
             colour = colo,
             linetype = "dashed") +
  geom_point() +
  geom_line() +
  labs(x = "Distance from fire spots (km)",
       y = "Percentage of species loss") +
  theme_classic() +
  xlim(c(0, 14)) +
  ylim(c(0, 0.57)) +
  theme(text = element_text(size = 15),
    legend.position = c(0.5, 1),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    axis.text.x = element_text(),
    axis.text.y = element_text(),
    #legend.position = c(0.5, 0.985),
    legend.direction = "horizontal")

    
  lp

#### 2. creating the map ####

## manipulating data
fire.muni.df <- aggregate(SCAN ~ NOME_MUNI, data = fire.muni, FUN = length)

head(fire.muni.df)
dim(fire.muni)
dim(fire.muni.df)

### getting county centroids 
head(muni.amz)

my.muni <- muni.amz[muni.amz$NOME_MUNI %in% fire.muni.df$NOME_MUNI,]
muniCentroids <- gCentroid(my.muni,byid = TRUE)
#plot(muni.amz)
#points(muniCentroids,pch=19, col="red")

muniCentroids.df <- data.frame(NOME_MUNI = as.character(my.muni$NOME_MUNI), 
                               muniCentroids@coords)

fire.muni.df <- merge(fire.muni.df, muniCentroids.df, by = "NOME_MUNI")
head(fire.muni.df)
names(fire.muni.df)[2] <- "Fire_freq"

muni.amz2 <- merge(as.data.frame(muni.amz), fire.muni.df[, 1:2], by = "NOME_MUNI",
                   all.x = TRUE, all.y = FALSE)

muni.amz2 <- muni.amz2[!duplicated(muni.amz2),]

muni.amz$Fire_freq <- muni.amz2$Fire_freq


#png("figs/Fire_map.png", res=300, width=1800, height=1600)

fogo.cor <- cartography::carto.pal('orange.pal', 5)

mybreaks <- c(500, 700, 1000, 1500, 1800)

SAmap <- ggplot() +
  geom_polygon(data = SA, aes(long, lat, group = group)) +
  geom_polygon(data = amz, aes(long, lat, group = group),
               fill = cores[3],
               alpha = 0.7) + theme_void() +
  annotate(geom = "text", x = -60, y = -25, label = "South \n America", 
           color = "grey90", size = 3) #+
#annotate(geom = "text", x = -59, y = -7, label = "Brazilian Amazon", 
#    color = "grey90", size = 1.5)

SAmap 

#visualize records
map <- ggplot() +
  geom_polygon(data = amz, aes(long, lat, group = group), 
               fill = cores[3], alpha = 0.7) +
  geom_point(aes(x = x, y = y, size = Fire_freq, col = Fire_freq),
             data = fire.muni.df, shape = 20, stroke = FALSE) +
  geom_point(aes(x = POINT_X, y = POINT_Y, shape = endemicas),
             data = as.data.frame(sp.10km)) +
  scale_shape_manual(values = c(5, 3), name = "Species records") +
  scale_color_gradientn(colours = fogo.cor, name = "Number of fires",
                        breaks = mybreaks) +
  #scale_color_gradient(low="orange", high="red") +
  #scale_color_gradientn(colours = heat.colors(5)[5:1]) +
  #scale_color_distiller(palette = "Reds", direction = 1, name="Fire frequency") +
  #scale_size_continuous(name="Fire frequency",  breaks=mybreaks) +
  scale_size_area(max_size = 15, name = "Number of fires", breaks = mybreaks) +
  #scale_size_continuous(name="Number of fires") +
  #scale_alpha_continuous(name="Number of fires", breaks=mybreaks) +
  theme_void() + 
  #theme_minimal()  + 
  #labs(x="Longitude", y="Latitude") +
  coord_map() + 
  guides(colour = guide_legend()) +
  theme(#axis.text.x=element_text(),
    #axis.text.y=element_text(),
    legend.position = c(0.98, 0.17),
    text = element_text(size = 20),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12)) +
  ggtitle("A")
#dev.off()

map

####################################################
#### juntando todas as figuras em uma #############
####################################################

# 3000 e 3000
vp_inset <- grid::viewport(width = 0.3, height = 0.25, x = 0.000001, y = 1.01, just = c("left", "top"))


png("figs/combined_fig_vertical.png", res = 300, width = 3000, height = 3400)
grid.arrange(map,
             lp + ggtitle("B"),
             bp + coord_flip() + ggtitle("C"),
             #ncol=4,
             #nrow=5,
             #widths=c(10, 2, 2, 2),
             heights = c(5, 5, 5, 7, 1),
             layout_matrix = rbind(c(1, 1),
                                   c(1, 1),
                                   c(1, 1),
                                   #c(2, NA),
                                   c(2, 3),
                                   c(2, NA)))
print(SAmap, vp = vp_inset)
dev.off()

map

######################################################
#### outras coisas ###################################
######################################################
                                        # do not run :P

# 4000 e 2000
png("figs/combined_fig.png", res = 300, width = 4100, height = 2100)
grid.arrange(map,
             lp + ggtitle("B"),
             bp + coord_flip() + ggtitle("C"),
             ncol = 3,
             nrow = 5,
             widths = c(10, 8, 8),
             layout_matrix = rbind(c(1, 1, 2),
                                   c(1, 1, 2),
                                   c(1, 1, 2),
                                   c(1, 1, 3),
                                   c(1, 1, 3)))

dev.off()



### teste com o poligono dos municipios ### 
muni.tidy <- tidy(muni.amz)
muni.amz$id <- row.names(muni.amz)
muni.tidy <- dplyr::left_join(muni.tidy, muni.amz@data)

pal <- wesanderson::wes_palette("Zissou1", 100, type = "continuous")

ggplot(muni.tidy, aes(x = long, y = lat, group = group, fill=Fire_freq)) +
  geom_polygon(color = "black", size = 0.1) +
  scale_fill_gradientn(colours = pal) +
  coord_equal() +
  theme_void() +
  labs(title = "Fire in the Brazilian Amazon") 
  # theme(plot.title = element_text(margin = margin(t = 60, b = -40)))

mybreaks <- c(200, 500, 700, 1000, 1500)

# Records inside buffer ####

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

ggplot() +
  geom_polygon(data = amz, aes(x=long, y = lat, group = group), fill="forestgreen", alpha=0.3) +
  geom_point(data = as.data.frame(fire.9km2), aes(x = LONGITUDE, y = LATITUDE),
             colour = "darkred", size = 0.5)

