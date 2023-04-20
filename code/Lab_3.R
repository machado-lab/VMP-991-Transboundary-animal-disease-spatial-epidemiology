## final regression analysis from the data extracted from raster
library("rnaturalearth")
library("rnaturalearthdata")
# Load packages
library(ggpubr); library(tidyverse);
library(sf); library(dplyr)
library(viridis); 
library(mapview);library(ggmap);library(lubridate)
library(maptools)
library(raster)
library(plyr)
library(rgdal)
library(ggpubr); library(tidyverse);
library(sf); library(dplyr)
library(viridis); library(mapview)
library(ggmap)

#load data
load("Q:/My Drive/GitHub/VMP-991-Transboundary-animal-disease-spatial-epidemiology/data/data.RData")
## download data in sf format to merge with counts
world <- ne_countries(scale='medium',returnclass = 'sf')
class(world)

## data merging
data<-data %>%
  dplyr::rename(admin=Country)

## count cases per country
percon<-data%>%
  group_by(admin)%>%
  dplyr::count(ID, sort = TRUE)%>%
  filter(n>1)%>%
  ungroup() %>%
  arrange(desc(n))

## merge all
count_cases <- world %>%
  dplyr::left_join(percon, by = "admin")

# percon<-data%>%
  #group_by(admin)%>%
  #dplyr::count(ID, sort = TRUE)%>%
  #filter(n>1)%>%
  #ungroup() %>%
  #arrange(desc(n))

## merge all
#count_cases <- world %>%
#  dplyr::left_join(percon, by = "admin")

DR<-count_cases%>%
  filter(admin=="Dominican Republic")

DRadmin<-data%>%
  filter(admin=="Dominican Republic")

## download DR
ar11<-raster::getData("GADM", country="DOM", level=2)
names(ar11)[7]<-"admin1"
ar11<-st_as_sf(ar11) ## to merge

ggplot(data = ar11) +
  geom_sf(size = .05,color="grey")

names(DRadmin)[4]<-"admin1"

#write.csv(DRadmin, file="./data/DRadmin.csv")
library(readr)
DRadmin_new <- read.csv("./data/DRadmin_new.csv")
DRadmin_new$admin1 <- enc2utf8(DRadmin_new$admin1)
## merge all
count_muni <- DRadmin_new %>%
  dplyr::left_join(ar11, by = "admin1")

count_county<-count_muni%>%
  group_by(admin1)%>%
  dplyr::count(ID, sort = TRUE)%>%
  ungroup() %>%
  arrange(desc(n))

## merge all
count_muni <- ar11 %>%
  dplyr::left_join(count_county, by = "admin1")

## finally per county
ggplot(data = count_muni, aes(fill = n)) +
  geom_sf(size = .05,color="grey") +
  scale_fill_viridis(option = "plasma",name="Total-2005-2019")+
  theme(text = element_text(size = 13, face = "bold"),
        axis.text.x = element_text(face = "bold",size = 13),
        axis.text.y = element_text(face = "bold",size = 13))

## replace NA by zero
count_muni$n<-count_muni$n %>%
  replace_na(0)

### regression
## lay
elev   <- raster::getData("alt", country="DOM", level=2) 
elev <- crop(elev[[1]], count_muni)
plot(elev)
plot(sf::st_geometry(count_muni), add = TRUE)

for (i in 1:length(count_muni)){
  ex <- raster::extract(elev, count_muni, fun=mean, na.rm=TRUE, df=TRUE)
}

## add back
count_muni$alt<-ex$DOM_msk_alt
ggplot(data = count_muni, aes(fill =alt)) +
  geom_sf(size = .05,color="grey") +
  scale_fill_viridis(option = "plasma",name="Altitude")+
  theme(text = element_text(size = 13, face = "bold"),
        axis.text.x = element_text(face = "bold",size = 13),
        axis.text.y = element_text(face = "bold",size = 13))

## regression
space.and.ndvi <- lm(as.numeric(n)~as.numeric(alt)+ as.numeric(temp),
                     data=count_muni)
summary(space.and.ndvi)

## extracting predictions and residuals:
count_muni$m_pred_spacendvi <- as.numeric(predict(space.and.ndvi, type="response"))
count_muni$m_resid_spacendvi <- residuals(space.and.ndvi)


ggplot(data = count_muni, aes(fill = m_pred_spacendvi)) +
  geom_sf(size = .05,color="grey")+
  scale_fill_viridis(option = "plasma",name="alt")


## Plot lm altitude
ggplot(count_muni, aes(x = alt, y = n)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")
