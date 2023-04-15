# Lab 1
library(ggpubr); library(tidyverse);
library(sf); library(dplyr)
library(viridis); library(mapview)
library(ggmap)

rm(list = ls()) # this is to clean all files 
cat("\014")

#setwd("C:/Users/gmachad/Google Drive/Nc_tenure/Teaching/My_TAD_selective_2019/Presentations/Map_in_R")

## merge all files
#list.filenames = list.files(pattern="*.csv")

##
#data <- data.frame()
#for (i in list.filenames) {
#  x <- read_delim(i, ";", escape_double = FALSE, locale = locale(encoding = "WINDOWS-1252"), trim_ws = TRUE)
#  data <-rbind(data,x)
#  print(i)
#  rm(x)
#}

## upload the data
data0<-read_csv("./data/ASF_overview-raw-data_202203160605.csv")# this will import the data

# simple description
summary(data0) # will summarize all columns

## renaming stuff
data0<-data0%>%
  rename(report_date = Report.date..dd.mm.yyyy.,observed_date = Observation.date..dd.mm.yyyy.)

## remove NA + select data we want to use and add a col.
names(data0) # this will print the names
data<-data0%>%
  dplyr::select(Region,Region:report_date)%>%
  drop_na()%>%
  mutate(ID = 1) # add a new column to my data

## add back number of animals dead
# data<-data%>%
#   dplyr::left_join(data0, by = "Id")

summary(data)
head(data)

## now free description
data%>%
  group_by(Species)%>%
  dplyr::count(ID, sort = TRUE)%>%
  drop_na()%>%
  dplyr::select(n,Species)%>%
  arrange(desc(n))%>%
  print(n = 10)

## and proportions
data%>%
  group_by(Country)%>%
  drop_na()%>%
  dplyr::summarize(n=n())%>%
  mutate(prop=n/sum(n))%>%
  arrange(desc(n))

## some fund description
# continent

data%>%
  group_by(Region)%>%
  dplyr::count(ID, sort = TRUE)%>%
  ungroup() %>%
  arrange(desc(n))%>% 
  ggplot(aes(reorder(Region,+ n), n)) +
  geom_bar(stat="identity") + coord_flip()+
  scale_y_continuous(breaks = seq(0,30000,by = 5000))+
  labs(x="Continent", y="Total number of cases")+
  theme(text = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 12))

ggsave("./fig/count_region.tiff", plot = last_plot(), 
       dpi = 300, width = 200, height = 160, units = "mm")

## country
data%>%
  dplyr::group_by(Country)%>%
  dplyr::count(ID, sort = TRUE)%>%
  ungroup() %>%
  arrange(desc(n))%>% 
  ggplot(aes(reorder(Country,+ n), n)) +
  scale_y_continuous(breaks = seq(0,30000,by = 5000))+
  geom_bar(stat="identity") + coord_flip()+
  labs(x="Country", y="Total number of cases")+
  theme(text = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 12))

ggsave("./fig/count_country.tiff", plot = last_plot(), 
       dpi = 300, width = 200, height = 160, units = "mm")

## fix label check the number of cases NAA as back
## host

data%>%
  group_by(Species)%>%
  dplyr::count(ID, sort = TRUE)%>%
  drop_na()%>%
  filter(n>1)%>%
  ungroup() %>%
  arrange(desc(n))%>% 
  ggplot(aes(reorder(Species,+ n), n)) +
  scale_y_continuous(breaks = seq(0,16715,by = 5000))+
  geom_bar(stat="identity") + coord_flip()+
  labs(x="Species", y="Total number of cases")+
  theme(text = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 12))

ggsave("./fig/count_host_species.tiff", plot = last_plot(), 
       dpi = 300, width = 200, height = 160, units = "mm")

### 2 x 2 combinations
data%>%
  group_by(Region,Species)%>%
  dplyr::count(ID, sort = TRUE)%>%
  filter(n>1)%>%
  ungroup() %>%
  arrange(desc(n))%>% 
  drop_na()%>%
  ggplot(aes(x = Region, y = n, fill = Species)) +
  geom_bar(stat = "identity")+
  theme(legend.position="bottom") +
  guides(fill=guide_legend(ncol=3))+
  labs(x="", y="Total number of cases")+
  theme(text = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 12))

ggsave("./fig/count_host_region.tiff", plot = last_plot(), 
       dpi = 300, width = 200, height = 160, units = "mm")

### time plots
library(scales)
data$ymIN<-as.Date(data$observed_date, "%d/%m/%Y", origin = "1904-01-01")

data$monthIN <- format(data$ymIN, "%m") # month
data$dayIN <- format(data$ymIN, "%d") # day
data$myIN <- format(data$ymIN, "%Y-%m") # month and year
data$y <- format(data$ymIN, "%Y")

## Temporal plots top species
data%>%
  group_by(Species)%>%
  dplyr::count(ID, sort = TRUE)%>%
  drop_na()%>%
  dplyr::select(n,Species)%>%
  arrange(desc(n))%>%
  print(n = 10)

data%>%
  group_by(y,Species)%>%
  dplyr::count(ID, sort = TRUE)%>%
  ungroup() %>%
  arrange(desc(n))%>%
  ggplot(aes(x=y, y=n, color=Species)) +
  geom_line(aes(group=Species, color=Species),size = 1) +
  geom_point(aes(color=Species, shape=Species),size = 2)+
  facet_wrap(~ Species)+
  #scale_x_discrete(breaks = 2015:2019)+
  labs(x = "Year", y = "Total number of cases") +
  theme(text = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_text(size = 12))

ggsave("./fig/count_host_time.tiff", plot = last_plot(), 
       dpi = 300, width = 200, height = 160, units = "mm")

#########################################################################################
###############################Lab2 mapping ############################################ ----
########################################################################################
library("rnaturalearth")
library("rnaturalearthdata")

## download data
world <- ne_countries(scale='medium',returnclass = 'sf')
class(world)

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

## finally map several options
# tiff("./Fig/map.tiff",  width = 400, height = 200, res = 300, units = "mm")
# ggplot(data = count_cases, aes(fill = n)) +
#   geom_sf(size = .08,color="grey") +
#   #scale_fill_viridis(option = "plasma")
#   scale_fill_viridis(na.value = "white")+
#  theme(text = element_text(size = 13, face = "bold"),
#       axis.text.x = element_text(face = "bold",size = 13),
#       axis.text.y = element_text(face = "bold",size = 13))
#   #scale_fill_viridis(breaks = c(10,50, 200, 500),
#    #                  label=c( 10,50, 200, 500))
# 
#   scale_fill_viridis(option = "viridis",
#                      alpha = 0.85, 
#                      discrete = F, 
#                      direction = -1, 
#                      guide = guide_legend(
#                        direction = "top",
#                        title.position = "top",
#                        title.hjust =0.5),name="Cases") 
# dev.off()
#  
## or
ggplot(data = count_cases, aes(fill = n)) +
  geom_sf(size = .05,color="grey") +
  scale_fill_viridis(option = "plasma",name="Total-1996-2022")+
  theme(text = element_text(size = 13, face = "bold"),
        axis.text.x = element_text(face = "bold",size = 13),
        axis.text.y = element_text(face = "bold",size = 13))

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


ggsave("./fig/count_map_globe_point.tiff", plot = last_plot(), 
       dpi = 300, width = 200, height = 160, units = "mm")

##################################################################
## basic analysis
ggplot(data = count_cases) +
  geom_sf(size = .05,color="grey") +
  geom_point(data = data, aes(x = Longitude, y = Latitude), 
             color = "#444444", 
             fill = "#444444",
             size = 1.2)+
  stat_density2d(data=data, mapping=aes(x=Longitude, y=Latitude, fill=..level..), geom="polygon", alpha=0.2) +
   scale_fill_viridis()

ggsave("./fig/count_map_globe_density.tiff", plot = last_plot(), 
       dpi = 300, width = 200, height = 160, units = "mm")

## make heatmaps
library(leaflet); library(leaflet.extras)
leaflet(data) %>%
   addTiles(group="OSM") %>%
   addCircleMarkers(data=data, lng=~Longitude , lat=~Latitude, radius=1 , color="black",  fillColor="red", stroke = TRUE, fillOpacity = 0.8, group="Red") %>% 
   addHeatmap(group="heat", lng=~Longitude, lat=~Latitude, max=.6, blur = 60)

## top region
data%>%
  group_by(admin)%>%
  drop_na()%>%
  dplyr::summarize(n=n())%>%
  mutate(prop=n/sum(n))%>%
  arrange(desc(n))


### Filter region for the top region
DR<-count_cases%>%
  filter(admin=="Dominican Republic")

DRadmin<-data%>%
  filter(admin=="Dominican Republic")

## write files
#write.csv(Ard, file="algeria.csv")
st_write(DR, "DR.shp") # how to write a shape

## merge all
count_casesDR <- DR %>%
  dplyr::left_join(DRadmin, by = "admin")

## without kernel
ggplot(data = count_casesDR) +
  geom_sf(size = .05,color="grey") +
  geom_point(data = DRadmin, aes(x = Longitude, y = Latitude), 
             color = "#444444", 
             fill = "#444444",
             size = 1.2)


ggsave("./fig/DR_cases.tiff", plot = last_plot(), 
       dpi = 300, width = 200, height = 160, units = "mm")

# with kernel
ggplot(data = count_casesDR) +
  geom_sf(size = .05,color="grey") +
  geom_point(data = DRadmin, aes(x = Longitude, y = Latitude), 
             color = "#444444", 
             fill = "#444444",
             size = 1.2)+
   stat_density2d(data=DRadmin, mapping=aes(x=Longitude, y=Latitude, fill=..level..),
                  geom="polygon", alpha=0.2)+
   scale_fill_viridis(option = "plasma")


### facet per year
ggplot(data = count_casesDR) +
  geom_sf(size = .05,color="grey") +
  geom_point(data = DRadmin, aes(x = Longitude, y = Latitude), 
             color = "#444444", 
             fill = "#444444",
             size = 1.2)+
  facet_grid(.~ y)

### facet per month year
ggplot(data = count_casesDR) +
  geom_sf(size = .05,color="grey") +
  geom_point(data = DRadmin, aes(x = Longitude, y = Latitude), 
             color = "#444444", 
             fill = "#444444",
             size = 1.2)+
  facet_wrap(~ myIN,ncol = 3 )

### facet per year
ggplot(data = count_casesDR) +
  geom_sf(size = .05,color="grey") +
  geom_point(data = DRadmin, aes(x = Longitude, y = Latitude), 
             color = "#444444", 
             fill = "#444444",
             size = 1.2)+
  facet_wrap(y~ Species, nrow = 4)


#1. Calculate distances between points
pointcases_dr <- st_as_sf(DRadmin, coords = c("Longitude", "Latitude"), crs=32619)
st_transform(pointcases_dr, crs=32619)

d <- pointcases_dr[st_nearest_feature(pointcases_dr, parwise = T),]

dist <- st_distance(pointcases_dr,pointcases_dr[d,], by_element = T)
      # dist = st_distance(pointcases_dr,nearest, by_element = T))

dist <- pointcases_dr %>% 
  mutate(
  lead=geometry[row_number()+1],
  dist=st_distance(geometry,lead,by_element = T))

#2. DAG and regression for example
#3. Bring more ideas

## final regression analysis from the data extracted from raster
library(maptools)
library(raster)
library(plyr)
library(rgdal)

ar11<-raster::getData("GADM", country="DZ", level=1)
names(ar11)[4]<-"admin1"
ar11<-st_as_sf(ar11) ## to merge

## merge all
count_muni <- Ard %>%
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
elev   <- raster::getData("alt", country="DZ", level=1) 
elev <- crop(elev[[1]], count_muni)
plot(elev)
plot(sf::st_geometry(count_muni), add = TRUE)

for (i in 1:length(count_muni)){
  ex <- raster::extract(elev, count_muni, fun=mean, na.rm=TRUE, df=TRUE)
}

## add back
count_muni$alt<-ex$DZA_msk_alt
ggplot(data = count_muni, aes(fill =alt)) +
  geom_sf(size = .05,color="grey") +
  scale_fill_viridis(option = "plasma",name="Total-2005-2019")+
  theme(text = element_text(size = 13, face = "bold"),
        axis.text.x = element_text(face = "bold",size = 13),
        axis.text.y = element_text(face = "bold",size = 13))

## regression
space.and.ndvi <- lm(as.numeric(n)~as.numeric(alt),
                      data=count_muni)
summary(space.and.ndvi)

## extracting predictions and residuals:
count_muni$m_pred_spacendvi <- as.numeric(predict(space.and.ndvi, type="response"))
count_muni$m_resid_spacendvi <- residuals(space.and.ndvi)


ggplot(data = count_muni, aes(fill = m_pred_spacendvi)) +
  geom_sf(size = .05,color="grey")+
  scale_fill_viridis(option = "plasma",name="alt")

#### climatic
temp   <- raster::getData('worldclim', var='tmin', res=10) 
temp <- crop(temp[[1]], count_muni)

plot(temp)
plot(sf::st_geometry(count_muni), add = TRUE)

# plot(temp)
# plot(sf::st_geometry(count_muni), add = TRUE)

for (i in 1:length(count_muni)){
  ex1 <- extract(temp, count_muni, fun=mean, na.rm=TRUE, df=TRUE)
}

## add back
count_muni$tmin1<-ex1$tmin1
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


ggplot(data = count_muni, aes(fill = m_pred_spacendvi)) +
  geom_sf(size = .05,color="grey")+
  scale_fill_viridis(option = "plasma",name="alt")

## Plot lm altitude
ggplot(count_muni, aes(x = alt, y = n)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")


## Plot lm temperature
ggplot(count_muni, aes(x = tmin1, y = n)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")

## regression
data<-read.csv("point.csv")

soco_OLS <- glm(FMD ~ precipitation, data=data)
summary(soco_OLS)

## all possible correlations
## main drivers for each disease
## lets use the data extracted to the points
install.packages("ggpmisc")
library(ggpmisc)
devtools::install_github("slowkow/ggrepel")
library(ggrepel)

# two variables
ggplot(count_muni, aes(x = tmin1, y = alt))+
  geom_point()+
  #geom_smooth(method = "lm") 
 geom_point(aes(color = HASC_1, shape = HASC_1)) +
  geom_rug(aes(color =HASC_1)) +
  #facet_wrap(~HASC_1) 
  geom_smooth(aes(color = HASC_1), method = lm, 
              se = FALSE, fullrange = TRUE)

## lable countries
ggplot(count_muni, aes(x = tmin1, y = alt))+
geom_point(aes(color = HASC_1)) +
  geom_text(aes(label = NAME_0), hjust = 1,  vjust = -1)

## ranges do lat long too
ggplot(count_muni, aes(x = tmin1, y = alt))+
geom_point(aes(color = HASC_1, size = n), alpha = 0.5) 

# gradient
p<-ggplot(count_muni, aes(x = tmin1, y = alt))+
geom_point(aes(color = n), size = 3) +
  scale_color_viridis()

library("ggExtra")
ggMarginal(p, type = "density")
# Change marginal plot type
ggMarginal(p, type = "boxplot")

# Hexagonal binning
ggplot(count_muni, aes(tmin1, alt)) +
  geom_hex(bins = 20, color = "white")+
  scale_fill_gradient(low =  "#00AFBB", high = "#FC4E07")+
  theme_minimal()
