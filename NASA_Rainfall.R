source("~/Abidjan/load_path.R", echo=FALSE)
#source("C:/Users/hp/Abidjan/load_path.R", echo=FALSE) #leave commented#

Earthdata <- file.path(NASAdata, "EarthData")
EVIkm <- file.path(Earthdata, "MODIS-TERRA_VegetationIndex_EVI_1km_Monthly_2013-23")
EVIm <- file.path(Earthdata, "MODIS-TERRA_VegetationIndex_EVI_500m_16d_2013-23")
NDVIkm <- file.path(Earthdata,"MODIS-TERRA_VegetationIndex_NDVI_1km_Monthly_2013-23")
Rainfall2013_23 <- file.path(NASAdata, "Rainfall 2013-2023")
Climatedata <- file.path(NASAdata, "ClimateSERV")
Abidjanmap1 <- file.path(NASAdata, "Autonome D_Abidjan2.geojson")

Abidjanmap <- st_read(Abidjanmap1)
ggplot(data = Abidjanmap)+
  geom_sf(color = "black", fill = 	"#ece9f7")+
  map_theme()
summary(Abidjanmap)

Abidjan = Abi_shapefile[[3]] %>% filter(NAME_1 == "Abidjan")
df_abidjan1 = st_intersection(Abi_shapefile[[7]], Abidjan)

BuiltupDir <-  file.path(AbidjanDir, "Built up area")
Built_areas <- st_read(file.path(BuiltupDir, "Built up area.shp"))
view(Built_areas)


##############################################################################################################################################################
# RAINFALL
###############################################################################################################################################################
### mean rainfall 2013-2023

rainfiles_names10  = list.files( file.path(Rainfall2013_23), 
                                 pattern = ".tif", full.names = TRUE)

rainraster_data10 = lapply(seq_along(rainfiles_names10), 
                           function(x) raster::raster(rainfiles_names10[[x]]))

rain_data10 = rainraster_data10 %>%
  purrr::map(~raster::extract(., df_abidjan1,
                              buffer = buffer,
                              fun = mean, df =TRUE)) %>%
  purrr::reduce(left_join, by = c("ID"))

df_abidjan1$meanrainfall <- rowMeans(rain_data10, na.rm=TRUE)

rainfall_plottingdata10 <- df_abidjan1 %>%
  sf::st_as_sf() %>%
  mutate(class = cut(meanrainfall, c(0, 0.05, 0.1, 0.16, 0.2,
                                     0.25, 0.3, 0.4), include.lowest = T ))
ggplot(data = df_abidjan1) +
  geom_sf(color = "black", fill = "white") +
  geom_sf(data = rainfall_plottingdata10, aes(geometry = geometry, fill = meanrainfall)) +
  scale_fill_continuous(name="average rainfall", low = "#F6E0b3", high = "darkblue") +
  labs(subtitle = '', fill = "", x = NULL, y = NULL) +
  map_theme() 







