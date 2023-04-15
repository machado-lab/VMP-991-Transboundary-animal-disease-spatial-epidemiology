### 1. Load data & exploratory analysis

library("rnaturalearth")
library("rnaturalearthdata")
# Load packages
library(ggpubr); library(tidyverse);
library(sf); library(dplyr)
library(viridis); 
library(mapview);library(ggmap);library(lubridate)

#load data
load("Q:/My Drive/GitHub/VMP_991_Transboundary-animal-disease-spatial-epidemiology/data/data.RData")

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
#ggsave("./fig/count_map_globe.tiff", plot = last_plot(), 
#      dpi = 300, width = 320, height = 300, units = "mm")

### point maps
ggplot(data = count_cases) +
  geom_sf(size = .05,color="gray") +
  geom_point(data = data, aes(x = Longitude, y = Latitude), 
             color = "#444444", 
             fill = "#444444",
             size = 0.8)+
  theme(text = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(face = "bold",size = 14),
        axis.text.y = element_text(face = "bold",size = 14))+
  ggtitle("Global ASF distribution")

## save in a proper format
#ggsave("./fig/count_map_globe_point.tiff", plot = last_plot(), 
#   dpi = 300, width = 200, height = 160, units = "mm")

#################################################################
## basic analysis
ggplot(data = count_cases) +
  geom_sf(size = .05,color="grey") +
  geom_point(data = data, aes(x = Longitude, y = Latitude), 
             color = "#444444", 
             fill = "#444444",
             size = 1.2)+
  stat_density2d(data=data, mapping=aes(x=Longitude, y=Latitude, fill=..level..), geom="polygon",alpha=0.2) +
  scale_fill_viridis()

## save in a proper format
# ggsave("./fig/count_map_globe_density.tiff", plot = last_plot(), 
#        dpi = 300, width = 200, height = 160, units = "mm")

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
  dplyr::summarize(n=n())%>%
  mutate(prop=n/sum(n))%>%
  arrange(desc(n))

#############################################################
### Here we decided to focus only on DR######################
### Filter region for the top region
DR<-count_cases%>%
  filter(admin=="Dominican Republic")

DRadmin<-data%>%
  filter(admin=="Dominican Republic")

## DR describing 
data%>%
  filter(admin=="Dominican Republic")%>%
  mutate(date=as.Date(ymIN), format= "%m/%d/%y")%>%
  mutate(weekly_cases = floor_date(
    date,
    unit = "week")) %>%            
  select(date, weekly_cases) %>%
  mutate(ncases=1)%>%
  arrange(date) %>%
  group_by(weekly_cases,ncases) %>%
  dplyr::count(ncases,weekly_cases , sort = TRUE)%>%
  ggplot(aes(weekly_cases,  n)) +
  geom_point(color = "orange") +
  geom_line(color = "orange") +
  scale_y_continuous(breaks = seq(1, 35, by = 2))+
  scale_x_date(date_breaks = "1 week", expand = c(0,0)) +
  theme(axis.text.x = element_text(angle=90, vjust=.1))+
  xlab("Epi week") +
  ggtitle("Total number of new infection per week")+
  ylab("Total counts") 

## save in a proper format
# ggsave("./fig/DR_cases_week.tiff", plot = last_plot(), 
#        dpi = 300, width = 200, height = 160, units = "mm")

## write files
#write.csv(DR, file="DR.csv")
#st_write(DR, "./data/DR.shp") # how to write a shape

## merge all
count_casesar <- DR %>%
  dplyr::left_join(DRadmin, by = "admin")

## without kernel
ggplot(data = count_casesar) +
  geom_sf(size = .05,color="grey") +
  geom_point(data = DRadmin, aes(x = Longitude, y = Latitude), 
             color = "#444444", 
             fill = "#444444",
             size = 1.2)

## save in a proper format
# ggsave("./fig/DR_cases.tiff", plot = last_plot(), 
#        dpi = 300, width = 200, height = 160, units = "mm")

# with kernel
ggplot(data = count_casesar) +
  geom_sf(size = .05,color="grey") +
  geom_point(data = DRadmin, aes(x = Longitude, y = Latitude), 
             color = "#444444", 
             fill = "#444444",
             size = 1.2)+
  stat_density2d(data=DRadmin, mapping=aes(x=Longitude, y=Latitude, fill=..level..),
                 geom="polygon", alpha=0.2)+
  scale_fill_viridis(option = "plasma")

### facet per year
ggplot(data = count_casesar) +
  geom_sf(size = .05,color="grey") +
  geom_point(data = DRadmin, aes(x = Longitude, y = Latitude), 
             color = "#444444", 
             fill = "#444444",
             size = 1.2)+
  facet_grid(.~ y)

### facet per month year
ggplot(data = count_casesar) +
  geom_sf(size = .05,color="grey") +
  geom_point(data = DRadmin, aes(x = Longitude, y = Latitude), 
             color = "#444444", 
             fill = "#444444",
             size = 1.2)+
  facet_wrap(~ myIN,ncol = 3 )

### facet per year
ggplot(data = count_casesar) +
  geom_sf(size = .05,color="grey") +
  geom_point(data = DRadmin, aes(x = Longitude, y = Latitude), 
             color = "#444444", 
             fill = "#444444",
             size = 1.2)+
  facet_wrap(y~ Species, nrow = 4)
```

### 3. Distance based measurements
```{r}
#Calculate distances between points
pointcases_dr <- st_as_sf(DRadmin, coords = c("Longitude", "Latitude"), crs=32619)

## adding a projection
st_transform(pointcases_dr, crs=32619)

# Lets create an ID column to make it easier to identify
pointcases_dr <- mutate(pointcases_dr, ID=row_number())

# Calculate the distance between all features
dist <- st_distance(pointcases_dr)

# The melt function converts a matrix to a data frame
dist <- data.table::melt(dist)
rownames(dist) = pointcases_dr$ID
colnames(dist) = pointcases_dr$ID

# Finds the nearest point to each feature
nearest <- pointcases_dr[st_nearest_feature(pointcases_dr, parwise = T),]

dist2 <- st_distance(pointcases_dr,pointcases_dr)
rownames(dist2) = pointcases_dr$ID
colnames(dist2) = nearest$ID
dist_near <- data.frame(col=colnames(dist2)[col(dist2)],
                        row=rownames(dist2)[row(dist2)],
                        dist=c(as.numeric(dist2)))
# t <- dist_near %>% 
#   group_by(col) %>% 
#   filter(dist != 0) %>% 
#   filter(dist == min(dist))

## distances among all cases
## DR data
common_crs <- 25832

datadr<-data%>%
  filter(admin=="Dominican Republic")%>%
  drop_na()

DT_sf = st_as_sf(datadr, coords = c("Longitude", "Latitude"), 
                 crs = common_crs)

st_transform(pointcases_dr, crs=common_crs)

plot(DT_sf)


## here is where we will do by species for Poland
distsf<-DT_sf %>%
  #group_by(Species) %>%
  mutate(
    lead = geometry[row_number() + 1],
    dist = st_distance(geometry, lead, by_element = T),
  )

distsf$dist<-as.numeric(distsf$dist)

distsf %>%
  ggplot(aes(x=dist)) +
  geom_histogram(binwidth=0.02, fill="#69b3a2", color="#e9ecef", alpha=0.9)

hist(distsf$dist)

## buffer
DT_sf$IDb<-1:nrow(DT_sf)
booths_buffer = st_buffer(DT_sf, 0.2)
booths_data = st_intersection(booths_buffer, DT_sf)

DT_sf %>% 
  ggplot() + geom_sf(fill = 'white') + 
  geom_sf(data = booths_buffer%>% filter(IDb==1),alpha = 0.09, fill = 'yellow') 

### which farms
booths_data%>% 
  filter(IDb==1)

