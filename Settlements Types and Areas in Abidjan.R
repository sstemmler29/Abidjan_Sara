source("~/Abidjan/load_path.R", echo=FALSE)
source("C:/Users/hp/Abidjan/load_path.R", echo=FALSE)

names(Abi_shapefile)

ggplot(data = Abi_shapefile[[7]])+ #health_district - 113
  geom_sf(color = "black", fill = 	"#ece9f7")+
  # geom_text_repel(
  #   data = Abi_shapefile[[7]],
  #   aes(label =  NOM, geometry = geometry),color ='black',
  #   stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  labs(title="All 113 health districts of Cote d' Ivoire", 
       fill = "", x = NULL, y = NULL)+
  map_theme()

#get Abidjan health district from the first file  
Abidjan = Abi_shapefile[[3]] %>% filter(NAME_1 == "Abidjan")
df_abidjan1 = st_intersection(Abi_shapefile[[7]], Abidjan)
ggplot(data = Abidjan)+
  geom_sf(data = df_abidjan1, color = "black", fill = 	"#ece9f7")+
  geom_text_repel(
    data = df_abidjan1,
    aes(label = str_to_sentence(NOM), geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  labs(title="All 15 health districts of Abidjan", 
       fill = "", x = NULL, y = NULL)+
  map_theme()

#layer with small settlements: Small settlements are likely rural/suburban areas
SmallsetDir <-  file.path(AbidjanDir, "Small settlement area")
small_set <- st_read(file.path(SmallsetDir, "Small settlement area.shp"))
view(small_set)

ggplot()+
  geom_sf(data = df_abidjan1)+
  geom_sf(color = "black", fill = "#ece9f7") +
  geom_sf(data = small_set, color = "red", fill = "transparent", size = 1) +  
  labs(title = "Abidjan Health Districts with Small Settlements", fill = "", x = NULL, y = NULL) +
  map_theme()
 

#plot Abidjan slums
slum_data <- read.csv(file.path(AbidjanDir, "Abidjan slums.csv"))
slum_sf <-  st_as_sf(slum_data, coords = c("Longitude", "Latitude"), crs = 4326)
view(slum_sf)

ggplot()+
  geom_sf(data = df_abidjan1)+
  geom_sf(data = slum_sf, color="purple", size =1)+
  geom_sf(color = "black", fill ="#ece9f7")+
  labs(title = "Slums in Abidjan", x = "Longitude", y = "Latitude") +
  map_theme()

#plot built up areas
BuiltupDir <-  file.path(AbidjanDir, "Built up area")
Built_areas <- st_read(file.path(BuiltupDir, "Built up area.shp"))
view(Built_areas)

ggplot()+
  geom_sf(data = df_abidjan1)+ 
  geom_sf(color = "black", fill = "#ece9f7") +
  geom_sf(data = Built_areas, color = "green", fill = "transparent", size = 1) +  
  labs(title = "Abidjan Health Districts with Built up Areas", fill = "", x = NULL, y = NULL) +
  map_theme()


#plot settlement types in Abidjan
descriptive_text <- paste("The slum communities identified and included in this map are:",
                          toString(slum_data[, "Slum.Name"]))
wrapped_text <- strwrap(descriptive_text, width = 50)  
wrapped_text <- paste(wrapped_text, collapse = "\n")

ggplot()+
  geom_sf(data = df_abidjan1, color = "black", fill = "#ece9f7" )+ 
  geom_sf(data = small_set, color = "red", fill = "transparent", size = 0.5) +
  geom_sf(data = slum_sf, color="purple", size = 1.2)+
  geom_sf(data = Built_areas, color = "green", fill = "transparent", size = 0.5) + 
  labs(title = "Housing Structure in Abidjan", fill = "", x = "Longitude", y = "Latitude", caption = wrapped_text)+
  theme(plot.caption = element_text(hjust = 0.5, margin = margin(t = 10, b = 10, unit = "pt")))+
  map_theme()

####alternate, will clean
descriptive_text <- paste("The slum communities identified and included in this map are:",
                          toString(slum_data[, "Slum.Name"]))
wrapped_text <- strwrap(descriptive_text, width = 50)  
wrapped_text <- paste(wrapped_text, collapse = "\n")
ggplot() +
  geom_sf(data = df_abidjan1, aes(fill = "Health Districts"), color = "black") +
  geom_sf(data = Built_areas, aes(fill = "Built-up Areas"), color = "green", size = 0.5) +
  geom_sf(data = small_set, aes(fill = "Small Settlements"), color = "red", size = 0.5) +
  geom_sf(data = slum_sf, aes(fill = "Slums"), color = "purple", size = 1.2) +
  labs(title = "Housing Structure in Abidjan", fill = "", x = "Longitude", y = "Latitude", caption = wrapped_text) +
  theme(plot.caption = element_text(hjust = 0.5, margin = margin(t = 10, b = 10, unit = "pt"))) +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5)) +
  scale_fill_manual(values = c("Health Districts" = "#ece9f7", "Small Settlements" = "red", "Slums" = "purple", "Built-up Areas" = "green")) +
  theme(legend.position = "right")




  
  
