##Load Packages ----
library(sf)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(mapview)

##source folders -----
shps <- "insert_filepath_here"
csvs <- "insert_filepath_here"

### Read in Shapefile ----
boroughs <- st_read(shps, "boroughs")

###Read In CSV and Make Spatial ----
##https://data.cityofnewyork.us/Recreation/Parks-Properties-Map/krz2-j7bn
parks_properties <- read.csv(paste0(csvs, "/Parks_Properties.csv")) %>%
  st_as_sf(wkt="multipolygon", crs=st_crs(4326)) #convert to spatial object

##MAP ----
ggplot()+
  geom_sf(data = boroughs, fill="white")+
  geom_sf(data = parks_properties, fill="forestgreen", color=NA) +
  theme_classic()+ ##no background
  coord_sf(datum = NA)+ ##remove gridlines
  ggtitle("Parks Properties in NYC", subtitle = "October 2023")+
  theme(plot.title = element_text(hjust=.5, size = 25),
        plot.subtitle = element_text(size=17, hjust = 0.5))


###Explore the Data----
head(parks_properties)
colnames(parks_properties)

##distribution
library(data.table)
t <- as.data.table(xtabs(~TYPECATEGORY, data=parks_properties))

###visualize interactively
mapview(parks_properties, zcol="TYPECATEGORY")

h <- read.csv(paste0(csvs, "/subway_entrance.csv"))
subway_entrance <- read.csv(paste0(csvs, "/subway_entrance.csv")) %>%
  st_as_sf(wkt="entrance_georeference", crs=st_crs(4326)) #convert to spatial object

##select what to visualize
parks_map <- subset(parks_properties, !(TYPECATEGORY %in% c('Parkway','Lot','Undeveloped'
                                                              , 'Mall', 'Cemetery', 'Buildings/Institutions'
                                                              ,'Managed Sites', 'Strip', 'Triangle/Plaza')))

  
  ##Map of Playgrounds ----
ggplot()+
  geom_sf(data = boroughs, fill="white")+
  geom_sf(data = parks_map, aes(fill=TYPECATEGORY)) +
  theme_classic()+ ##no background
  coord_sf(datum = NA)+ ##remove gridlines
  ggtitle("Parks in NYC", subtitle = "October 2023")+
  theme(plot.title = element_text(hjust=.5, size = 25),
        plot.subtitle = element_text(size=17, hjust = 0.5))+
  labs(caption=paste0("Source: NYC Open Data\n https://data.cityofnewyork.us/Recreation/Parks-Properties-Map/krz2-j7bn 
                      \n Retrieved on ", "October 23, 2023"))+
  scale_fill_brewer(palette = "Paired", name="Property Type")

##Export to PDF ----
ggsave("parks_map.pdf",
       plot=last_plot(),
       width=8.5, height=11, units="in",
       dpi=1200)

