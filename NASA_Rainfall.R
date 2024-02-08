#source("~/Abidjan/load_path.R", echo=FALSE)
source("C:/Users/hp/Abidjan/load_path.R", echo=FALSE) #leave commented#

Earthdata <- file.path(NASAdata, "EarthData")
EVIkm <- file.path(Earthdata, "MODIS-TERRA_VegetationIndex_EVI_1km_Monthly_2013-23")
EVIm <- file.path(Earthdata, "MODIS-TERRA_VegetationIndex_EVI_500m_16d_2013-23")
NDVIkm <- file.path(Earthdata,"MODIS-TERRA_VegetationIndex_NDVI_1km_Monthly_2013-23")
Rainfall2013_23 <- file.path(NASAdata, "Rainfall 2013-2023")
Climatedata <- file.path(NASAdata, "ClimateSERV")
RainfallPluszip <- file.path(Climatedata, "ClimeServ_CHIRPS_Rainfall2013-2023", "ClimeServ_CHIRPS_AverageRainfall_FullData2013-23.zip")
RainfallPlus <- unzip(RainfallPluszip)
Abidjanmap1 <- file.path(NASAdata, "Autonome D_Abidjan2.geojson")

Abidjanmap <- st_read(Abidjanmap1)
ggplot(data = Abidjanmap)+
  geom_sf(color = "black", fill = 	"#ece9f7")+
  map_theme()
summary(Abidjanmap)

Abidjan = Abi_shapefile[[3]] %>% filter(NAME_1 == "Abidjan")
df_abidjan1 = st_intersection(Abi_shapefile[[7]], Abidjan)


##############################################################################################################################################################
# RAINFALL
###############################################################################################################################################################
### mean rainfall 2013-2023
rainfiles1323 <- list.files( file.path(Rainfall2013_23), 
                                 pattern = ".tif", full.names = TRUE)

raindata1323 <- lapply(seq_along(rainfiles1323), 
                           function(x) raster::raster(rainfiles1323[[x]]))

rain_data = raindata1323 %>%
  purrr::map(~raster::extract(., df_abidjan1,
                              buffer = buffer,
                              fun = mean, df =TRUE)) %>%
  purrr::reduce(left_join, by = c("ID"))

df_abidjan1$meanrainfall <- rowMeans(rain_data10, na.rm=TRUE)

rainfall_plotdata1323 <- df_abidjan1 %>%
  sf::st_as_sf() %>%
  mutate(class = cut(meanrainfall, c(0, 0.05, 0.1, 0.16, 0.2,
                                     0.25, 0.3, 0.4), include.lowest = T ))
ggplot(data = df_abidjan1) +
  geom_sf(color = "black", fill = "white") +
  geom_sf(data = rainfall_plotdata1323, aes(geometry = geometry, fill = meanrainfall)) +
  scale_fill_continuous(name="average rainfall", low = "#F6E0b3", high = "darkblue") +
  labs(title = 'Average rainfall in Abidjan 2013-2023', fill = "", x = NULL, y = NULL) +
  map_theme() 

########## Yearly rainfall############
########2013
pattern2013 <- paste0("2013[0-9]+.tif")
rainfall_2013 <- list.files(file.path(Rainfall2013_23), pattern = pattern2013, full.names = TRUE)
print(rainfall_2013)
rainfall_data13 = lapply(seq_along(rainfall_2013), 
                       function(x) raster::raster(rainfall_2013[[x]]))
raindata13 = rainfall_data13 %>%
  purrr::map(~raster::extract(., df_abidjan1,
                              buffer = buffer,
                              fun = mean, df =TRUE)) %>%
  purrr::reduce(left_join, by = c("ID"))

df_abidjan1$meanrain13 <- rowMeans(raindata13, na.rm=TRUE)

rainfall_plotdata13 <- df_abidjan1 %>%
  sf::st_as_sf() %>%
  mutate(class = cut(meanrain13, c(0, 0.05, 0.1, 0.16, 0.2,
                                  0.25, 0.3, 0.4), include.lowest = T))
library(ggrepel)
ggplot(data = df_abidjan1) +
  geom_sf(color = "black", fill = "white") +
  geom_sf(data = df_abidjan1, aes(fill = meanrain13)) + 
  scale_fill_continuous(name = "Average Rainfall", low = "grey", high = "darkblue") +
  #geom_text_repel(data = rainfall_plotdata13, aes(label = round(meanrain13, 2)), size = 3) +
  #geom_sf_text(data = rainfall_plotdata13, aes(geometry = geometry, label = meanrain13))+
  geom_sf_text(data = rainfall_plotdata13, aes(geometry = geometry, label = round(meanrain13, 2)))+
  #geom_sf_text_repel(aes(label = meanrain13), size = 3) +
  labs(title = "Average rainfall in Abidjan (2013)", fill = "", x = NULL, y = NULL) +
  map_theme()


#####Test for absent years######
pattern2014 <- paste0("2018[0-9]+.tif")
rainfall_2014 <- list.files(file.path(Rainfall2013_23), pattern = pattern2014, full.names = TRUE)
print(rainfall_2014)


