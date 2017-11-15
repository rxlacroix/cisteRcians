setwd("/home/romain/Documents")
list.files()

abb <- read.csv("abbayescist.csv", header = TRUE, sep = ",")

# n°568 Laurus deleted see J. Richard, 1971, "Laurum" : une abbaye cistercienne fantôme. Bibliothèque de l'école des chartes, 129 (2) pp. 409-410
# 559	Santa Maria de Caritate and 572 Santo Spirito della Valle ungeolocalized

# loading the required packages
library(ggplot2)
library(ggmap)
library(scales)
library(leaflet)
library(plotly)
library(MASS)
library(sp)
library(rgeos)
library(rgdal)
library(shiny)
library(htmlwidgets)
library(webshot)

#fondation per year

annees <- as.data.frame(table(abb$Date))
colnames(annees)[1]<-"Annee"
colnames(annees)[2]<-"Fondations"
annees$Annee <- as.numeric(as.character(annees$Annee))
g <- ggplot(data = annees, aes(x = annees$Annee, y = annees$Fondations)) +
  geom_line(alpha = 0.4) 
ggplotly(g)

# heatmap
mapabb <- get_map(location = c(lon = mean(abb$Lon), lat = mean(abb$Lat)), zoom = 4,
                  maptype = "toner-lite")
gm <- ggmap(mapabb)

SPDF <- readOGR(dsn = "/home/romain/Documents", layer = "Europe")
plot(SPDF)
SPDF <- spTransform(SPDF, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96"))
SPDF <- gSimplify(SPDF, tol = 0.00001)
# this is a well known R / GEOS hack (usually combined with the above) to 
# deal with "bad" polygons
SPDF <- gBuffer(SPDF, byid=TRUE, width=0)
# any bad polys?
sum(gIsValid(SPDF, byid=TRUE)==FALSE)
## [1] 0

SPDF <- spTransform(SPDF, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0 "))
#clipping
SPDF <- gUnaryUnion(SPDF)
plot(SPDF)

for(i in 1114:1300){
  df <- subset(abb, abb$Date <= i)
  hm <- stat_density2d(data = df, aes(x = df$Lon, y = df$Lat, fill = ..level.., alpha = ..level..), bins = 9, geom = "polygon") 
  gb <- ggplot_build(gm+hm)
  gb
  dat <- gb$data[[4]]
  SpatialPolygons(lapply(unique(dat$group), function(x) {
    pts <- dat[dat$group == x,]
    Polygons(list(Polygon(as.matrix(data.frame(x=pts$x, y=pts$y)))), as.character(x)) })) -> polys
  polys_dat <- SpatialPolygonsDataFrame(polys, 
                                        data.frame(id=sapply(slot(polys, "polygons"), slot, "ID"),
                                                   row.names=sapply(slot(polys, "polygons"), slot, "ID"),
                                                   stringsAsFactors=FALSE))
  latlong = "+init=epsg:4326"
  proj4string(polys_dat) = CRS(latlong)
  polys_dat <-  spTransform(polys_dat, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0 "))
  zones_clipped <- gIntersection(polys_dat, SPDF, byid=TRUE)
  map <- leaflet(abb) %>%
    setView(zoom = 5, lng=8, lat=48) %>%
    # Base groups
    addProviderTiles(providers$Hydda.Base, group = "Hydda (default)" ) %>%
    # Overlay groups
    addPolygons(data=zones_clipped, weight = 2, fillColor = 'red', stroke = FALSE) %>%
    addCircles(df$Lon, df$Lat, 
               weight = 4,  
               color="black", group ="Abbeys", opacity=1) %>%
    addLabelOnlyMarkers(lng = 27, lat = 57, label = paste(i), icon = NULL, labelOptions = labelOptions(noHide = T, direction = "top", 
                                                                                                       textOnly = T,
                                                                                   style = list(
                                                                                     "color" = "red",
                                                                                     "font-size" = "36px",
                                                                                     "stroke-width"= "10px",
                                                                                     "stroke-color"="white"
                                                                                   )))
  map
  saveWidget(map, "temp.html", selfcontained = FALSE)
  webshot("temp.html", file = paste(i,"_Cistercians.png", sep = ""),
          cliprect = "viewport")
 
}

system("cd /home/romain/Documents && convert -delay 10 -loop 0 *.png cistercians.gif",intern = TRUE)
system("cd /home/romain/Documents && convert -delay 30 -loop 0 *.png cistercians_slow.gif",intern = TRUE)

