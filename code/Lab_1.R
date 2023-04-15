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
data0<-read.csv("../data/ASF_overview-raw-data_202203160605.csv")

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