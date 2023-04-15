######################################-#
# Inla comparison between Dr and Poland
#####################################-#
library(GADMTools); library(maptools)
library(raster); library(sf); library(tidyverse)
library(rnaturalearth);library(rnaturalearthdata)
library(INLA); library(elevatr);library(INLA)

# 1. DR and Poland grids ----

world <- ne_countries(scale='medium',returnclass = 'sf')

#1.1. Dominican Republic
DR <- world %>% 
  filter(admin == "Dominican Republic") %>% 
  select(geounit) %>% 
  rename(Country=geounit)

st_crs(DR) <- st_crs(4326)

DR <- DR %>% st_transform(crs=32619)

#1.1.a Create a 2km grid for the Dominican Republic from bounding box

bb <- st_bbox(DR)
x <- seq(bb$xmin - 1, bb$xmax + 1, by = 2000)
y <- seq(bb$ymin - 1, bb$ymax + 1, by = 2000)

dr_grid <- as.matrix(expand.grid(x, y))
plot(dr_grid, asp = 1)

dr <- st_as_sf(data.frame(x = dr_grid[, 1], y = dr_grid[, 2]),
              coords = c("x", "y"))

st_crs(dr) <- st_crs(32619)
ind <- st_intersects(DR, dr)
dr <- dr[ind[[1]], ]
plot(dr, asp = 1)

# 1.2. Poland
Poland <- world %>% 
  filter(admin == "Poland") %>% 
  select(geounit) %>% 
  rename(Country=geounit)

st_crs(Poland) <- st_crs(4326)

Poland <- Poland %>% st_transform(crs=32633)

#1.2.a Create a 2km grid for the Poland from bounding box

bb <- st_bbox(Poland)
x <- seq(bb$xmin - 1, bb$xmax + 1, by = 2000)
y <- seq(bb$ymin - 1, bb$ymax + 1, by = 2000)

po_grid <- as.matrix(expand.grid(x, y))
plot(po_grid, asp = 1)

po <- st_as_sf(data.frame(x = po_grid[, 1], y = po_grid[, 2]),
               coords = c("x", "y"))

st_crs(po) <- st_crs(32633)
ind <- st_intersects(Poland, po)
poland <- po[ind[[1]], ]
plot(poland, asp = 1)

#
# 2. Load points and create data frame for DR and Poland ----


datall<-read.csv("./data/ASF_overview-raw-data_202203160605.csv")

# rename date columns
datall<-datall%>%
  rename(report_date = Report.date..dd.mm.yyyy.,
         observed_date = Observation.date..dd.mm.yyyy.)


# Create columns for Y-M-D
datall$ymIN<-as.Date(datall$observed_date, "%d/%m/%Y", origin = "1904-01-01")

datall$monthIN <- format(datall$ymIN, "%m") # month
datall$dayIN <- format(datall$ymIN, "%d") # day
datall$myIN <- format(datall$ymIN, "%Y-%m") # month and year
datall$year <- format(datall$ymIN, "%Y") # year


#2.1. DR
DR_points <- datall %>% 
  filter(Country == "Dominican Republic") %>% 
  st_as_sf(coords= c("Longitude","Latitude"), crs=4326) %>% 
  st_transform(crs = 32619)

DR_points[, c("x", "y")] <- st_coordinates(DR_points)

ggplot(DR)+
  geom_sf()+
  coord_sf(datum=st_crs(32619))+
  geom_point(data=DR_points, aes(x=x, y=y))+
  theme_bw()

DR_points <- st_intersection(DR,DR_points) # 4 points removed from outside boundary

# Controls 
DR_controls <- st_sample(DR, 10000) %>%
  st_sf() %>%
  st_transform(32619)

plot(DR_controls)
plot(DR_points, col= "red",add = T)

# 2.2. Poland
Poland_points <- datall %>% 
  filter(Country == "Poland") %>% 
  st_as_sf(coords= c("Longitude","Latitude"), crs=4326) %>% 
  st_transform(crs=32633)
  
Poland_points[,c("x","y")] <- st_coordinates(Poland_points)

ggplot(Poland)+
  geom_sf()+
  coord_sf(datum=st_crs(32633))+
  geom_point(data=Poland_points, aes(x=x, y=y))+
  theme_bw()

Poland_points <- st_intersection(Poland,Poland_points)# 53 points outside

Po_controls <- st_sample(Poland, 10000) %>%
  st_sf() %>%
  st_transform(32633)

plot(Po_controls)
plot(Poland_points, col="red", add=T)


# 3. Point Extract ----
#4.1.a DR grid
DRpoly <- DR %>%
  st_make_grid(cellsize = 2000) %>%
  st_intersection(DR) %>%
  st_cast("MULTIPOLYGON") %>%
  st_sf() %>%
  mutate(id = row_number())

#4.1.b DR case and control extraction
DR_points21 <- DR_points %>% 
  filter(year==2021)

DR_points22 <- DR_points %>% 
  filter(year==2022)

DR_int21 <- DRpoly %>% 
  mutate(numcase =lengths(st_intersects(DRpoly,DR_points21)),
         numcontrols = lengths(st_intersects(DRpoly,DR_controls)))
DR_int21$year <- 2021    
 
DR_int22 <- DRpoly %>% 
  mutate(numcase =lengths(st_intersects(DRpoly,DR_points22)),
         numcontrols = lengths(st_intersects(DRpoly,DR_controls)))
DR_int22$year <- 2022

DR_comb <- rbind(DR_int21,DR_int22)


save.image("./data/DR_comb.RData")

# 4.2. Poland
#4.2.a Poland Grid
Popoly <- Poland %>%
  st_make_grid(cellsize = 2000) %>%
  st_intersection(Poland) %>%
  st_cast("MULTIPOLYGON") %>%
  st_sf() %>%
  mutate(id = row_number())

# Poland case and control extraction
#2014
Po_points14 <- Poland_points %>% 
  filter(year==2014)

Po_int14 <- Popoly %>% 
  mutate(numcase =lengths(st_intersects(Popoly,Po_points14)),
         numcontrols = lengths(st_intersects(Popoly,Po_controls)))
Po_int14$year <- 2014 

#2015
Po_points15 <- Poland_points %>% 
  filter(year==2015)

Po_int15 <- Popoly %>% 
  mutate(numcase =lengths(st_intersects(Popoly,Po_points15)),
         numcontrols = lengths(st_intersects(Popoly,Po_controls)))
Po_int15$year <- 2015 

#2016
Po_points16 <- Poland_points %>% 
  filter(year==2016)

Po_int16 <- Popoly %>% 
  mutate(numcase =lengths(st_intersects(Popoly,Po_points16)),
         numcontrols = lengths(st_intersects(Popoly,Po_controls)))
Po_int16$year <- 2016 

#2017
Po_points17 <- Poland_points %>% 
  filter(year==2017)

Po_int17 <- Popoly %>% 
  mutate(numcase =lengths(st_intersects(Popoly,Po_points17)),
         numcontrols = lengths(st_intersects(Popoly,Po_controls)))
Po_int17$year <- 2017 

#2018
Po_points18 <- Poland_points %>% 
  filter(year==2018)

Po_int18 <- Popoly %>% 
  mutate(numcase =lengths(st_intersects(Popoly,Po_points18)),
         numcontrols = lengths(st_intersects(Popoly,Po_controls)))
Po_int18$year <- 2018 

#2019
Po_points19 <- Poland_points %>% 
  filter(year==2019)

Po_int19 <- Popoly %>% 
  mutate(numcase =lengths(st_intersects(Popoly,Po_points19)),
         numcontrols = lengths(st_intersects(Popoly,Po_controls)))
Po_int19$year <- 2019 


# 2020
Po_points20 <- Poland_points %>% 
  filter(year==2020)

Po_int20 <- Popoly %>% 
  mutate(numcase =lengths(st_intersects(Popoly,Po_points20)),
         numcontrols = lengths(st_intersects(Popoly,Po_controls)))
Po_int20$year <- 2020 

# 2021
Po_points21 <- Poland_points %>% 
  filter(year==2021)

Po_int21 <- Popoly %>% 
  mutate(numcase =lengths(st_intersects(Popoly,Po_points21)),
         numcontrols = lengths(st_intersects(Popoly,Po_controls)))
Po_int21$year <- 2021 

Po_comb <- rbind(Po_int14,Po_int15,Po_int16,Po_int17,Po_int18,
                 Po_int19,Po_int20,Po_int21)


save.image("./data/Po_comb.RData")


cases_control_grid <- DRpoly %>%
  st_join(controls) %>%
  dplyr::group_by(id) %>%
  dplyr::summarize(num_cases = n())
ggplot(data = cases_control_grid, aes(fill = num_cases)) +
  geom_sf(size = .005,color="NA")+
  scale_fill_viridis(option = "plasma")





 
# 5. SPDE
# 4. old Inla mesh ----

#3.1. Dominican Republic
dr_coor <- cbind(DR_points$x, DR_points$y)
dr_bound <- inla.nonconvex.hull(st_coordinates(DR)[,1:2])
mesh_DR <- inla.mesh.2d(
  loc=dr_coor, boundary = dr_bound,
  max.edge =c(10000, 20000), cutoff = 2000
)
mesh_DR$n
plot(mesh_DR)
points(dr_coor, col="red")

#3.2. Poland
po_coor <- cbind(Poland_points$x, Poland_points$y)
po_bound <- inla.nonconvex.hull(st_coordinates(Poland)[,1:2])
mesh_po <- inla.mesh.2d(
  loc=po_coor, boundary = po_bound,
  max.edge =c(10000, 20000), cutoff = 2000
)
mesh_po$n
plot(mesh_po)
points(po_coor, col="red")




 
# 5. INLA ----

# Two-step process to retrieve coordinates from points
#Step 1
f <- st_centroid(DR_comb)
#Step 2
f <- st_coordinates(f)

# Add coordinates back to sf object
DR_comb$X <- f[,1]
DR_comb$Y <- f[,2]

# Convert boundry to spatialPolygonDataFrame
DR_spat <- as(DR,"Spatial")

# Convert to data frame
dr_coo <- as.data.frame(DR_comb)
# Remove geometry column
dr_coo <- select(dr_coo, -geometry)
# Convert back to matrix-INLA doesn't like dataframes??
dr_coo <- as.matrix(dr_coo[,5:6])

# Make the mesh
dr_mesh <- inla.mesh.2d(
  loc = dr_coo, boundary = DR_spat,
  max.edge =c(100000,200000),
  cutoff = 2000)
dr_mesh$n

plot(dr_mesh)


#==============================#
#   Set up INLA model object   #
#==============================#
# creates the projection matrix 

A <- inla.spde.make.A(dr_mesh, loc = dr_coo)

# Matern SPDE model object with PC prior for INLA
spde <- inla.spde2.pcmatern(mesh = dr_mesh,
                            prior.range = c(0.05, 0.01), # P(practic.range < 0.05) = 0.01
                            prior.sigma = c(1, 0.01)) # P(sigma > 1) = 0.01


#================================#
#   INLA formula for null model  # (no w)
#================================#
# set prior 
pcprec <- list(prior = 'pcprec', param = c(1, 0.01))



dr_coo$cases <- ifelse(dr_coo$numcase>0,1,0)


# Define the data stack for the INLA model
stk.dat <- inla.stack(
  data = list(y = dr_coo$cases),   # --------------------------- > aqui voltamos o desfecho 
  A = list(A,1),
  effects = list(list(s = 1:spde$n.spde), 
                 data.frame(Intercept = 1, 
                            gWest = inla.group(dr_coo[, 5]))),
  tag = 'dat') 


mod.inla.null  <- inla(y ~ + 0  ,
                       family = "binomial",
                       control.compute = list(dic = TRUE, waic = TRUE,config = TRUE),
                       data = inla.stack.data(stk.dat),
                       control.predictor = list(A = inla.stack.A(stk.dat), link = 1), verbose=TRUE)

summary(mod.inla.null)

#================================#
# INLA formula for ONLY spatial  # (no covariates)
#================================#

stk.dat <- inla.stack(
  data = list(y = dr_coo$cases),   # --------------------------- > aqui boltamos o desfecho 
  A = list(A,1),
  effects = list(list(s = 1:spde$n.spde), 
                 data.frame(Intercept = 1, 
                            gWest = inla.group(dr_coo[, 5]))),
  tag = 'dat') 

f.west1 <- y ~ -1 + Intercept + # f is short for formula
  f(gWest, model = 'rw1', # first random walk prior 
    scale.model = TRUE, # scaling this random effect
    hyper = list(theta = pcprec)) 


mod_inla_only_spatial <- inla(f.west1, family = 'binomial', # r is short for result
                              control.compute = list(cpo = TRUE, dic = T),
                              data = inla.stack.data(stk.dat), 
                              control.predictor = list(A = inla.stack.A(stk.dat), link = 1), verbose=TRUE) 

summary(mod_inla_only_spatial)

#save results 
saveRDS(mod.inla.null, "./code/mod_inla_null.rds")
saveRDS(mod_inla_only_spatial, "./code/mod_inla_only_spatial.rds")

