#title: "VMP 9951 - Transboundary Animal Disease Spatial Epidemiology"-- Poland
### 1. Load data & exploratory analysis

library("rnaturalearth")
library("rnaturalearthdata")
# Load packages
library(ggpubr); library(tidyverse);
library(sf); library(dplyr)
library(viridis); 
library(mapview);library(ggmap);library(lubridate)

#load data
load("~/Desktop/NCSU CVM/Third Year/Spring 2022/Transboundary dz and spatial epi/VMP_991_Transboundary-animal-disease-spatial-epidemiology/data/data.RData")

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

## plot
ggplot(data = count_cases, aes(fill = n)) +
  geom_sf(size = .05,color="grey") +
  scale_fill_viridis(option = "plasma",name="Total-1996-2022")+
  theme(text = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(face = "bold",size = 14),
        axis.text.y = element_text(face = "bold",size = 14))

## save in a proper format
ggsave("./fig/count_map_globe.tiff", plot = last_plot(), 
dpi = 300, width = 200, height = 160, units = "mm")

### point maps
ggplot(data = count_cases) +
  geom_sf(size = .05,color="gray") +
  geom_point(data = data, aes(x = Longitude, y = Latitude), 
             color = "#444444", 
             fill = "#444444",
             size = 0.8)+
  theme(text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(face = "bold",size = 13),
        axis.text.y = element_text(face = "bold",size = 13))+
  ggtitle("Global ASF distribution")

## save in a proper format
ggsave("./fig/count_map_globe_point.tiff", plot = last_plot(), 
   dpi = 300, width = 200, height = 160, units = "mm")

#################################################################
## basic analysis
ggplot(data = count_cases) +
  geom_sf(size = .05,color="grey") +
  geom_point(data = data, aes(x = Longitude, y = Latitude), 
             color = "#444444", 
             fill = "#444444",
             size = 1.2)+
  stat_density2d(data=data, mapping=aes(x=Longitude, y=Latitude, fill=..level..), geom="polygon",      alpha=0.2) +
  scale_fill_viridis()

## save in a proper format
 ggsave("./fig/count_map_globe_density.tiff", plot = last_plot(), 
        dpi = 300, width = 200, height = 160, units = "mm")

## make heat maps
library(leaflet); library(leaflet.extras)
leaflet(data) %>%
  addTiles(group="OSM") %>%
  addCircleMarkers(data=data, lng=~Longitude , lat=~Latitude, radius=1 , color="black",      fillColor="red", stroke = TRUE, fillOpacity = 0.8, group="Red") %>% 
  addHeatmap(group="heat", lng=~Longitude, lat=~Latitude, max=.6, blur = 60)

## top region
data%>%
  group_by(admin)%>%
  drop_na()%>%
  dplyr::summarize(n=n())%>%   #summarize n = n == count
  mutate(prop=n/sum(n))%>%    # create new column named prop, which is % 
  arrange(desc(n))

#############################################################
### Here we decided to focus only on DR#######################  #start Poland
### Filter region for the top region
PL<-count_cases%>%
  filter(admin=="Poland")

PLadmin<-data%>%
  filter(admin=="Poland")


## PL describing 
data%>%
  filter(admin=="Poland")%>%
  mutate(date=as.Date(ymIN), format= "%m/%d/%y")%>%
  mutate(weekly_cases = floor_date(
    date,
    unit = "month")) %>%            #group everything per month
  dplyr::select(date, weekly_cases) %>%
  mutate(ncases=1)%>%
  arrange(date) %>%
  group_by(weekly_cases,ncases) %>%
  dplyr::count(ncases,weekly_cases , sort = TRUE)%>%
  ggplot(aes(weekly_cases,  n)) +
  geom_point(color = "orange") +
  geom_line(color = "orange") +
  scale_y_continuous(breaks = seq(1, 700, by = 10))+
  scale_x_date(date_breaks = "1 week", expand = c(0,0)) +
  theme(axis.text.x = element_text(angle=45, vjust=.1))+
  xlab("Epi week") +    #xlab to rename axis of x
  ggtitle("Total number of new infection per week")+
  ylab("Total counts") 

## save in a proper format
#ggsave("./fig/DR_cases_week.tiff", plot = last_plot(), 
     #   dpi = 300, width = 1000, height = 160, units = "mm")

## issue with width in above figure ^

## write files
#write.csv(DR, file="DR.csv")
#st_write(DR, "./data/DR.shp") # how to write a shape


## merge all
count_casesar <- PL %>%
  dplyr::left_join(PLadmin, by = "admin") %>% 
  drop_na(y)


count_casesar$y<- as.factor(count_casesar$y)

## without kernel
ggplot(data = count_casesar) +
  geom_sf(size = .05,color="grey") +
  geom_point(data = PLadmin, aes(x = Longitude, y = Latitude), 
             color = "#444444", 
             fill = "#444444",
             size = 1.2)

## save in a proper format
# ggsave("./fig/DR_cases.tiff", plot = last_plot(), 
#        dpi = 300, width = 200, height = 160, units = "mm")

# with kernel
ggplot(data = count_casesar) +
  geom_sf(size = .05,color="grey") +
  geom_point(data = PLadmin, aes(x = Longitude, y = Latitude), 
             color = "#444444", 
             fill = "#444444",
             size = 1.2)+
  stat_density2d(data=PLadmin, mapping=aes(x=Longitude, y=Latitude, fill=..level..),
                 geom="polygon", alpha=0.2)+
  scale_fill_viridis(option = "plasma")

### facet per year
ggplot(data = count_casesar) +
  geom_sf(size = .05,color="grey") +
  geom_point(data = PLadmin, aes(x = Longitude, y = Latitude), 
             color = "#444444", 
             fill = "#444444",
             size = 1.2)+
  facet_wrap(~ ymIN,ncol = 3 )
  
  
### facet per month year
ggplot(data = count_casesar) +
  geom_sf(size = .05,color="grey") +
  geom_point(data = PLadmin, aes(x = Longitude, y = Latitude), 
             color = "#444444", 
             fill = "#444444",
             size = 1.2)+
  facet_wrap(~ y,ncol = 3 )    # put month and year in the map, ncol= 3 -- put maps in max 3 columns

### facet per year
ggplot(data = count_casesar) +
  geom_sf(size = .05,color="grey") +
  geom_point(data = PLadmin, aes(x = Longitude, y = Latitude), 
             color = "#444444", 
             fill = "#444444",
             size = 1.2)+
  facet_wrap(y~ Species, nrow = 4)   #map for each year and each species


### 3. Distance based measurements-- just for DR (don't do Poland)
# 
# #Calculate distances between points
# pointcases_dr <- st_as_sf(DRadmin, coords = c("Longitude", "Latitude"), crs=32619)
# #crs = cordinate reference system, number that is specific for each country
# #st as sf - forces it to be in sf format; force CRS projection to be 84 and correct zone
# #Poland crs = 2179
# 
# ## adding a projection
# st_transform(pointcases_dr, crs=32619)
# 
# # Lets create an ID column to make it easier to identify
# pointcases_dr <- mutate(pointcases_dr, ID=row_number())
# 
# # Calculate the distance between all features
# dist <- st_distance(pointcases_dr)
# 
# # The melt function converts a matrix to a data frame
# dist <- data.table::melt(dist)
# rownames(dist) = pointcases_dr$ID
# colnames(dist) = pointcases_dr$ID
# 
# # Finds the nearest point to each feature
# nearest <- pointcases_dr[st_nearest_feature(pointcases_dr, parwise = T),]
# 
# dist2 <- st_distance(pointcases_dr,pointcases_dr)
# rownames(dist2) = pointcases_dr$ID
# colnames(dist2) = nearest$ID
# dist_near <- data.frame(col=colnames(dist2)[col(dist2)],
#                         row=rownames(dist2)[row(dist2)],
#                         dist=c(as.numeric(dist2)))
# 
# hist(dist_near$dist)
# # t <- dist_near %>% 
# #   group_by(col) %>% 
# #   filter(dist != 0) %>% 
# #   filter(dist == min(dist))
# 
# ## distances among all cases
# ## DR data
# common_crs <- 25832   #create vector and this number is in projection - represent DR; force all projections to be the same
# 
# datadr<-data%>%
#   filter(admin=="Dominican Republic")%>%
#   drop_na()
# 
# DT_sf = st_as_sf(datadr, coords = c("Longitude", "Latitude"), 
#                  crs = common_crs)
# 
# st_transform(pointcases_dr, crs=common_crs)
# 
# plot(DT_sf)
# 
# 
# ## here is where we will do by species for Poland
# distsf<-DT_sf %>%
#   #group_by(Species) %>%
#   mutate(
#     lead = geometry[row_number() + 1],
#     dist = st_distance(geometry, lead, by_element = T),
#   )
# 
# distsf$dist<-as.numeric(distsf$dist)
# 
# distsf %>%
#   ggplot( aes(x=dist)) +
#   geom_histogram( binwidth=0.1, fill="#69b3a2", color="#e9ecef", alpha=0.9)
# 
# hist(distsf$dist)
# 
# ## buffer -- try to draw buffers around every point; extract # of cases in each point so have control zone (0.2 km)
# DT_sf$IDb<-1:nrow(DT_sf)  # see what other farms are at distance
# booths_buffer = st_buffer(DT_sf, 0.2)    #draw buffer around 200m; holds a buffer around each farm (not the actual farms)
# booths_data = st_intersection(booths_buffer, DT_sf)     #intersect buffers and put points inside -- count points between buffers, etc
# 
# DT_sf %>% 
#   ggplot() + geom_sf(fill = 'white') + 
#   geom_sf(data = booths_buffer%>% filter(IDb==1),alpha = 0.09, fill = 'yellow') 
# 
# ### which farms
# booths_data%>% 
#   filter(IDb==1)



### 3. regression analysis- both DR and Poland


## final regression analysis from the data extracted from raster #go back to Poland
library(maptools)
library(raster)
library(plyr)
library(rgdal)


# DO = DR, poland= PL
## get county level map to merge with ASF counts
ar11<-raster::getData("GADM", country="PL", level=2)
names(ar11)[4]<-"admin1"
ar11<-st_as_sf(ar11) ## to merge


### Filter region for the top region
PL<-data%>%
  filter(admin=="Poland")%>%
  as.data.frame()

## make as factor
PL$admin<-as.factor(PL$admin) 

PLd<-data%>%
  filter(admin=="Poland")
ar11$admin1 <- as.factor(ar11$admin1)
## merge all
names(PL)[4]<-"admin1"
count_muni <- PL %>%
  dplyr::left_join(ar11, by = "admin1")

## count cases per county
count_county<-count_muni%>%
  group_by(admin1)%>%
  dplyr::count(ID, sort = TRUE)%>%
  ungroup() %>%
  arrange(desc(n));count_county

## merge all
count_muni <- ar11 %>%
  dplyr::left_join(count_county, by = "admin1")%>%
  group_by(admin1)%>%
  dplyr::count(ID, sort = TRUE)%>%
  ungroup() %>%
  arrange(desc(n))

## finally per county
ggplot(data = count_muni, aes(fill = n)) +
  geom_sf(size = .05,color="grey") +
  scale_fill_viridis(option = "plasma",name="Total-2014-2022")+
  theme(text = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(face = "bold",size = 14),
        axis.text.y = element_text(face = "bold",size = 14))

## replace NA by zero
count_muni$n<-count_muni$n %>%
  replace_na(0)


### regression (altitude)
## lay
elev<-raster::getData("alt", country="PL", level=1)
elev <- raster::crop(elev[[1]], count_muni)
plot(elev)
plot(sf::st_geometry(count_muni), add = TRUE)

## for loop to grab and calculate the mean for each county
for (i in 1:length(count_muni)){
  ex <- raster::extract(elev, count_muni, fun=mean, na.rm=TRUE, df=TRUE)
}

## add back
count_muni$elev<-ex$POL_msk_alt
ggplot(data = count_muni, aes(fill =elev)) +
  geom_sf(size = .05,color="grey") +
  scale_fill_viridis(option = "plasma",name="Average elevation")+
  theme(text = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(face = "bold",size = 14),
        axis.text.y = element_text(face = "bold",size = 14))

## regression
space.and.ndvi <- lm(as.numeric(n)~as.numeric(elev),
                     data=count_muni)
summary(space.and.ndvi)

## extracting predictions and residuals:
count_muni$m_pred_spacendvi <- as.numeric(predict(space.and.ndvi, type="response"))
count_muni$m_resid_spacendvi <- residuals(space.and.ndvi)

# plot
ggplot(data = count_muni, aes(fill = m_pred_spacendvi)) +
  geom_sf(size = .05,color="grey")+
  scale_fill_viridis(option = "plasma",name="elev")

#### climatic
temp   <- raster::getData('worldclim', var='tmin', res=10) 
temp <- crop(temp[[1]], count_muni)

plot(temp)
plot(sf::st_geometry(count_muni), add = TRUE)

tempm<-getData('worldclim', var='bio', res=10)
tempm <- crop(tempm[[1]], count_muni)

plot(tempm)
plot(sf::st_geometry(count_muni), add = TRUE)

# for loop to extract the values into the county level
for (i in 1:length(count_muni)){
  ex1 <- extract(tempm, count_muni, fun=mean, na.rm=TRUE, df=TRUE)
}

## add back
count_muni$tmin1<-ex1$tmax1
ggplot(data = count_muni, aes(fill =tmin1)) +
  geom_sf(size = .05,color="grey") +
  scale_fill_viridis(option = "plasma",name="Temperature")+
  theme(text = element_text(size = 13, face = "bold"),
        axis.text.x = element_text(face = "bold",size = 13),
        axis.text.y = element_text(face = "bold",size = 13))

## regression
space.and.ndvi <- lm(as.numeric(n)~as.numeric(tmin1),
                     data=count_muni)
summary(space.and.ndvi)

## extracting predictions and residuals:
count_muni$m_pred_spacendvi <- as.numeric(predict(space.and.ndvi, type="response"))
count_muni$m_resid_spacendvi <- residuals(space.and.ndvi)

## make a map of the model
ggplot(data = count_muni, aes(fill = m_pred_spacendvi)) +
  geom_sf(size = .05,color="grey")+
  scale_fill_viridis(option = "plasma",name="tmin1")


## Plot lm altitude
ggplot(count_muni, aes(x = elev, y = n)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")


## Plot lm temperature
ggplot(count_muni, aes(x = ex1$tmax1, y = n)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")

### 4. Extra correlation analysis

## all possible correlations
## main drivers for each disease
## lets use the data extracted to the points
#devtools::install_github("slowkow/ggrepel")
library(ggrepel)

# gradient
p<-ggplot(count_muni, aes(x = tmin1, y = elev))+
  geom_point(aes(color = n), size = 3) +
  scale_color_viridis()

library("ggExtra")
ggMarginal(p, type = "density")
# Change marginal plot type
ggMarginal(p, type = "boxplot")

# Hexagonal binning
ggplot(count_muni, aes(tmin1, elev)) +
  geom_hex(bins = 20, color = "white")+
  scale_fill_gradient(low =  "#00AFBB", high = "#FC4E07")+
  theme_minimal()


