

DRpoly <- DR %>%
  st_make_grid(cellsize = 2000) %>%
  st_intersection(DR) %>%
  st_cast("MULTIPOLYGON") %>%
  st_sf() %>%
  dplyr::mutate(id = row_number())

controls <- st_sample(DR, size = 10000) %>%
  st_sf()

cases <- st_sample(DR, size = 200) %>%
  st_sf()

cases_control_grid <- DRpoly %>%
  st_join(controls) %>%
  dplyr::group_by(id) %>%
  dplyr::summarize(num_cases = n())


ggplot(data = cases_control_grid, aes(fill = num_cases)) +
  geom_sf(size = .005,color="NA")+
  scale_fill_viridis(option = "plasma")
