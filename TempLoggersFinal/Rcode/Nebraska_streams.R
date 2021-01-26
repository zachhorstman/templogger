library(tidyverse)
library(mapdata)
library(ggplot2)
library(ggmap)



suppressMessages({
  library(dplyr); # data munging and piping
  library(purrr); # for list functions
  library(ggplot2); # plotting
  library(ggrepel) # for labeling
  library(sf); # spatial simple features
  library(USAboundaries); # state/county data
  library(Imap); # nice mapping/color functions
  #library(geoknife); # USGS tool set (Next post)
  #library(dataRetrieval); # USGS tool set (next post)
  library(httr) # scraping webdata (to download flowlines)
})

states <- map_data("state")


Nebraska <- states %>%
  filter(region =="nebraska")

#ggplot(data = Nebraska) + 
  #geom_polygon(aes(x = long, y = lat, group = group), fill = "palegreen", color = "black") + 
  #coord_quickmap()



counties <- map_data("county")
NE_county <- counties %>%
  filter(region == "nebraska")

NE_base <- ggplot(data = Nebraska, mapping = aes(x = long, y = lat, group = group)) + 
  coord_quickmap() + 
  geom_polygon(color = "black", fill = "white")
NE_base + theme_void()

NE_base + theme_void() + 
  geom_polygon(data = NE_county, fill = NA, color = "black") +
  geom_polygon(color = "black", fill = NA)


myLocation <- c(lon=-99.087390, lat=40.667699)
myLocation <- c(-101, 40, -98.5, 41.5)
myMap <- get_map(location=myLocation,
                 source="stamen", maptype="watercolor", crop=FALSE)
ggmap(myMap)




#get_flowlines <- function(streamorder, mapRange){
#] postURL <- "https://cida.usgs.gov/nwc/geoserver/nhdplus/ows"

#  filterXML <- paste0('<?xml version="1.0"?>',
#                      '<wfs:GetFeature xmlns:wfs="http://www.opengis.net/wfs" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:gml="http://www.opengis.net/gml" service="WFS" version="1.1.0" outputFormat="shape-zip" xsi:schemaLocation="http://www.opengis.net/wfs http://schemas.opengis.net/wfs/1.1.0/wfs.xsd">',
#                      '<wfs:Query xmlns:feature="https://gov.usgs.cida/nhdplus" typeName="feature:nhdflowline_network" srsName="EPSG:4326">',
#                      '<ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">',
#                      '<ogc:And>',
#                      '<ogc:PropertyIsGreaterThan>',
#                      '<ogc:PropertyName>streamorde</ogc:PropertyName>',
#                      '<ogc:Literal>',streamorder-1,'</ogc:Literal>',
#                      '</ogc:PropertyIsGreaterThan>',
#                      '<ogc:BBOX>',
#                      '<ogc:PropertyName>the_geom</ogc:PropertyName>',
#                      '<gml:Envelope>',
#                      '<gml:lowerCorner>',mapRange[3]," ",mapRange[1],'</gml:lowerCorner>',
#                      '<gml:upperCorner>',mapRange[4]," ",mapRange[2],'</gml:upperCorner>',
#                      '</gml:Envelope>',
#                      '</ogc:BBOX>',
#                      '</ogc:And>',
#                      '</ogc:Filter>',
#                      '</wfs:Query>',
#                      '</wfs:GetFeature>')
#  
 # destination = file.path(tempdir(),"nhdflowline_network.zip")
#  file <- POST(postURL, body = filterXML, write_disk(destination, overwrite=T))
  
#  filePath <- tempdir()
#  print("unzipping...")
#  unzip(destination, exdir = filePath)
  
#  flowLines <- st_read(filePath, layer = 'nhdflowline_network')
  
#  return(flowLines)
#}

state_names <- c("nebraska")
co_names <- c("Lincoln0", "Dawson", "Phelps", "Buffalo", "Kearney", "Gosper")

# get STATE data
NE<-us_states(resolution = "high", states = state_names) %>%
  st_transform(crs = 4326)


counties_spec <- us_counties(resolution = "low", states=state_names) %>% # use list of state(s) here
  filter(name %in% co_names) %>% # filter to just the counties we want
  mutate(lon=map_dbl(geometry, ~st_centroid(.x)[[1]]), # add centroid values for labels
         lat=map_dbl(geometry, ~st_centroid(.x)[[2]])) # add centroid values for labels
mapRange1 <- c(range(st_coordinates(counties_spec)[,1]),range(st_coordinates(counties_spec)[,2]))



ggplot() + 
  geom_sf(data=NE, color = "gray30", lwd=2, fill=NA) +
  geom_sf(data=counties_spec, fill = NA, show.legend = F, color="gray50", lwd=0.4) +
  geom_label_repel(data=counties_spec, aes(x=lon, y=lat, label=name)) +
  coord_sf(xlim = mapRange1[c(1:2)], ylim = mapRange1[c(3:4)]) +
  theme_bw()

#fname <- system.file("C:/Davis_timesheets/hydrography_NHD24K_mbr_3640214_01.zip/hydrologic_units/wbdhu8_a_mbr.shp")
#huc8 <- read_sf(unzip("C:/Davis_timesheets/hydrologic_units_WBDHU8_mbr_3640214_02.zip"), quiet = F) %>%
#  st_transform(crs=4326) #%>% 







library(tidyverse)
library(maptools)
url.river_data <- url("http://sharpsightlabs.com/wp-content/datasets/usa_rivers.RData")
load(url.river_data)
summary(lines.rivers)
lines.rivers@data %>% glimpse()

levels(lines.rivers$FEATURE)
table(lines.rivers$FEATURE)

lines.rivers1 <- subset(lines.rivers, (FEATURE == "Stream"))
table(lines.rivers1$FEATURE)

lines.rivers <- subset(lines.rivers1, (STATE=="NE"))
table(lines.rivers$STATE)


df.usa_rivers <- fortify(lines.rivers)

map.usa_country <- map_data("usa")
map.usa_states <- map_data("state")





NE_base <- ggplot(data = Nebraska, mapping = aes(x = long, y = lat, group = group)) + 
  coord_quickmap() + 
  geom_polygon(color = "black", fill = "white")
NE_base + theme_void()

NE_base + theme_void() + 
  geom_polygon(data = NE_county, fill = NA, color = "black") +
  geom_polygon(color = "black", fill = NA)+ 
  geom_path(data = df.usa_rivers, aes(x = long, y = lat, group = group), color = "#8ca7c0", size = .08) 





#ggplot() +
#  geom_polygon(data = map.usa_country, aes(x = long, y = lat, group = group), fill = "#484848") +
#  geom_path(data = df.usa_rivers, aes(x = long, y = lat, group = group), color = "#8ca7c0", size = .08) +
#  coord_map(projection = "albers", lat0 = 30, lat1 = 40, xlim = c(-121,-73), ylim = c(25,51)) +
#  labs(title = "Rivers and waterways of the United States") +
#  annotate("text", label = "sharpsightlabs.com", family = "Gill Sans", color = "#A1A1A1"
#           , x = -89, y = 26.5, size = 5) +
#  theme(panel.background = element_rect(fill = "#292929")
#        ,plot.background = element_rect(fill = "#292929")
#        ,panel.grid = element_blank()
#        ,axis.title = element_blank()
#        ,axis.text = element_blank()
#        ,axis.ticks = element_blank()
#        ,text = element_text(family = "Gill Sans", color = "#A1A1A1")
 #       ,plot.title = element_text(size = 34)
#  ) 


states <- map_data("state")


Nebraska <- states %>%
  filter(region =="nebraska")

counties <- map_data("county")
NE_county <- counties %>%
  filter(region == "nebraska")

NE_base <- ggplot(data = Nebraska, mapping = aes(x = long, y = lat, group = group)) + 
  coord_quickmap() + 
  geom_polygon(color = "black", fill = "white")

NE_base + theme_void()
geom_polygon(data = NE_county, fill = NA, color = "black") +
  geom_polygon(color = "black", fill = NA)


NE<-us_states(resolution = "high", states = state_names) %>%
  st_transform(crs = 4326)




state_names <- c("nebraska")
co_names <- c("Lincoln", "Dawson", "Phelps", "Buffalo", "Kearney", "Gosper")

counties_spec <- us_counties(resolution = "low", states=state_names) %>% # use list of state(s) here
  filter(name %in% co_names) %>% # filter to just the counties we want
  mutate(lon=map_dbl(geometry, ~st_centroid(.x)[[1]]), # add centroid values for labels
         lat=map_dbl(geometry, ~st_centroid(.x)[[2]])) # add centroid values for labels
mapRange1 <- c(range(st_coordinates(counties_spec)[,1]),range(st_coordinates(counties_spec)[,2]))

ggplot() + 
  geom_sf(data=NE, color = "gray30", lwd=2, fill=NA) +
  #geom_sf(data=counties_spec, fill = NA, show.legend = F, color="gray50", lwd=0.4) +
  #geom_label_repel(data=counties_spec, aes(x=lon, y=lat, label=name)) +
  geom_path(data = df.usa_rivers, aes(x = long, y = lat, group = group), color = "#8ca7c0", size = .08) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.ticks.x=element_blank())+
  #theme(axis.title.y=element_blank(),  axis.text.y=element_blank(), axis.ticks.y=element_blank())+
  coord_sf(xlim = mapRange1[c(1:2)], ylim = mapRange1[c(3:4)]) +
  theme_bw()
GPS <- read.csv("LoggersGPS.csv")

## Nebraska with streams only
NE_base + theme_void() + 
  #geom_polygon(data = NE_county, fill = NA, color = "black") +
  geom_polygon(color = "black", fill = "black")+ 
  geom_path(data = df.usa_rivers, aes(x = long, y = lat, group = group), color = "#8ca7c0", size = .08)
  geom_point(data = GPS, aes(x=long, y=lat), size=2, alpha=0.2= color="yellow")

#map <- get_googlemap()
#qmap(mapRange1, zoom=6, source = "stamen", maptype = "toner")
#ggmap(map)


map <- get_googlemap(center=c(lon=-99.087390, lat=40.667699))

ggplot(data = Nebraska, mapping = aes(x = long, y = lat, group = group)) + 
  coord_quickmap() + 
  geom_polygon(color = "black", fill = "black") +
  theme_void() + 
  #geom_polygon(data = NE_county, fill = NA, color = "black") +
  geom_polygon(color = "black", fill = NA)+ 
  geom_path(data = df.usa_rivers, aes(x = long, y = lat, group = group), color = "#8ca7c0", size = .08) +
  geom_point( aes(x=Long, y=Lat, group=Stream.Name),color="green", GPS)

GPS <- read.csv("GPS.csv")

GPS <- as.data.frame(GPS)
