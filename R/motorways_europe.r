################################################################################
#                  Motorway map of Europe using OSM data
#                  Milos Popovic
#                  2021/12/12
################################################################################

if(!require("sf")) install.packages("sf")
if(!require("tidyverse")) install.packages("tidyverse")  

library(tidyverse, quietly=T)
library(sf, quietly=T)

windowsFonts(georg = windowsFont('Georgia'))

#download file
u <- "https://download.geofabrik.de/europe-latest.osm.pbf"
download.file(u, basename(u), mode="wb")

#query motorways
q = "SELECT highway 
    FROM lines 
    WHERE highway IN('motorway', 'motorway_link')"

mway <- st_read("europe-latest.osm.pbf", query = q)  %>% 
        st_transform(4326) %>% 
        st_as_sf()

#download and unrar free Eurostat 2013 country shapefile
url <- "https://gisco-services.ec.europa.eu/distribution/v2/countries/download/ref-countries-2013-01m.shp.zip" # location on the Eurostat website
download.file(url, basename(url), mode="wb") #download Eurostat country shapefiles
unzip("ref-countries-2013-01m.shp.zip") # unzip the boundary data
unzip("CNTR_RG_01M_2013_4326.shp.zip")

# download  and filter European iso2 codes
urlfile <-'https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv'
iso2 <- read.csv(urlfile) %>%
        filter(region=="Europe" | alpha.2 == "TR" | alpha.2 == "CY") %>% #filter Europe, Cyprus and Turkey
        select("alpha.2")

#load shapefile and filter only European countries
europe <- read_sf("CNTR_RG_01M_2013_4326.shp",
            stringsAsFactors = F) %>%
         st_transform(4326) %>% 
         st_as_sf() %>%
         mutate(FID = recode(FID, 'UK'='GB')) %>% 
         mutate(FID = recode(FID, 'EL'='GR')) %>%
         subset(FID%in%iso2$alpha.2)  #filter only European countries

# define Mercator and Lambert projections
crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs"
crsLAEA <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_defs"

# create a bounding box of Europe baed on Mercator coordinates
bb <- st_sfc(
  st_polygon(list(cbind(
    c(-10.6600, 36.5500, 36.5500, -10.6600, -10.6600), # x-coordinates (longitudes) of points A,B,C,D
    c(34.5000, 34.5000, 71.0500, 71.0500, 34.5000)     # y-coordinates (latitudes) of points A,B,C,D
    ))),
  crs = crsLONGLAT)

# transform the shp into |Lambert projection
laeabb <- st_transform(bb, crs = crsLAEA)
b <- st_bbox(laeabb)

# make the motorway map of europe :D
p <- ggplot() +
geom_sf(data=mway, color="#FFB115", size=0.15, fill=NA) +
geom_sf(data=europe, color="#07CFF7", size=0.1, fill=NA) +
    coord_sf(crs = crsLAEA, 
             xlim = c(b["xmin"], b["xmax"]), 
             ylim = c(b["ymin"], b["ymax"])) +
    theme_minimal() +
  theme(text = element_text(family = "georg", color = "#22211d"),
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_text(size=9, color="grey90", hjust=0.25, vjust=3),
    axis.title.y = element_blank(),
    legend.position = "none",
    legend.text = element_text(size=9, color="grey20"),
    legend.title = element_text(size=10, color="grey20"),
    panel.grid.major = element_line(color = "#010D1F", size = 0.2),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face="bold", size=16, color="grey90", hjust=.5),
    plot.caption = element_text(size=8, color="grey90", hjust=1, vjust=0),
    plot.subtitle = element_text(size=12, color="grey20", hjust=.5),
    plot.margin = unit(c(t=1, r=-2, b=-1, l=-2),"lines"), #added these narrower margins to enlarge maps
    plot.background = element_rect(fill = "#010D1F", color = NA), 
    panel.background = element_rect(fill = "#010D1F", color = NA), 
    legend.background = element_rect(fill = "#010D1F", color = NA),
    panel.border = element_blank()) +
    labs(x = "©2021 Milos Popovic (https://milospopovic.net)\n Data: © OpenStreetMap contributors", 
         y = NULL, 
         title = "European motorways", 
         subtitle = "", 
         caption = "")

ggsave(filename="eur_motorways.png", width= 7, height= 8.5, dpi = 600, device='png', p)