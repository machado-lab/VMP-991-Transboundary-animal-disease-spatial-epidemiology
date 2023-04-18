### 1. Introduction

# Uncomment this part if you need to install packages
# install.packages(c("ggpubr", "tidyverse",
#                    "sf","viridis","mapview","ggmap") )

# Load packages
library(ggpubr); library(tidyverse);
library(sf); library(dplyr)
library(viridis); 
library(mapview);library(ggmap)

rm(list = ls())
cat("\014")

# Change this to the directory on your computer
#setwd("~/Felipe_R/VMP_991_Transboundary-animal-disease-spatial-epidemiology")


### 2. Load and explore data

# Load the data
data0<-read.csv("./data/ASF_overview-raw-data_202203160605.csv")

# Examine the data 
summary(data0) # will summarize all columns

# Renaming columns in our dataset
data0<-data0%>%
  rename(report_date = Report.date..dd.mm.yyyy.,
         observed_date = Observation.date..dd.mm.yyyy.)

## Remove NAs + select data we want to use and add a col.
names(data0) # this will print the names

data<-data0%>%
  dplyr::select(Region,Region:report_date)%>%
  drop_na()%>%
  mutate(ID = 1) # add a new column to my data

## add back number of animals dead
# data<-data%>%
#   dplyr::left_join(data0, by = "Id")

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


### 3.1. Descriptive plots

# Total number of cases by continent
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

# Total number of cases by country
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
## Total number of cases by host
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

# *Review
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

###3.2 Temporal Visualizations
library(scales)

# prepare the data for plotting 
data$ymIN<-as.Date(data$observed_date, "%d/%m/%Y", origin = "1904-01-01")

data$monthIN <- format(data$ymIN, "%m") # month
data$dayIN <- format(data$ymIN, "%d") # day
data$myIN <- format(data$ymIN, "%Y-%m") # month and year
data$y <- format(data$ymIN, "%Y")

###3.2.1. Create some time-series plots
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

### 3.2.2. save as rdata
save(data, file = "data.RData")