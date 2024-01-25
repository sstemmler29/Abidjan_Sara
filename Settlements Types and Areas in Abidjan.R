source("~/Abidjan/load_path.R", echo=FALSE)

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

ggplot(data = Abidjan)+
  geom_sf(color = "black", fill = "#ece9f7") +
  geom_sf(data = small_set, color = "red", fill = "transparent", size = 1) +  
  labs(title = "Abidjan Health Districts with Small Settlements", fill = "", x = NULL, y = NULL) +
  map_theme()
 


#plot Abidjan slums
slum_data <- read.csv(file.path(AbidjanDir, "Abidjan slums.csv"))
slum_sf <-  st_as_sf(slum_data, coords = c("Longitude", "Latitude"), crs = 4326)

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


#plot built up areas + slums
ggplot()+
  geom_sf(data = df_abidjan1)+ 
  geom_sf(data = slum_sf, color="purple", size = 1)+
  geom_sf(color = "black", fill = "#ece9f7") +
  geom_sf(data = Built_areas, color = "green", fill = "transparent", size = 1) +  
  labs(title = "Abidjan Health Districts with Built up Areas", fill = "", x = NULL, y = NULL) +
  map_theme()


###
ggplot() +
  geom_sf(data = df_abidjan1, fill = "#ece9f7", color = "black") +
  geom_sf(data = slum_sf, color = "purple", size = 1) +
  geom_label_repel(
    data = df_abidjan1,
    aes(label = str_to_sentence(NOM), 
        x = st_coordinates(st_centroid(geometry))[,"X"],
        y = st_coordinates(st_centroid(geometry))[,"Y"]),
        #geometry = geometry),color ='black',
    #stat = "sf_coordinates",
    #size = 3.5,
    #segment.size = 0.2,
    segment.color = "black",
    arrow = arrow(type = "open", length = unit(0.1, "inches")),
    max.overlaps = Inf
  ) +
  labs(title = "Slums in Abidjan", x = "Longitude", y = "Latitude", color = "Legend") +
  map_theme()
