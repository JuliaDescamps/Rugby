# ENVIRONNEMENT ---------
library(leaflet)
library(sf)
library(dplyr)
library(stringr)
library(oceanis) # add title to map
library(tidyr)
library(leaflegend)
library(animation)
library(mapview) # save map to png
library(htmlwidgets) # save map to html
library(magick) # read images in R 

'%!in%' <- function(x,y)!('%in%'(x,y))
webshot::install_phantomjs()

setwd("/Users/julia/Documents/DESCAMPS 2022/rugby")

# DATA ---------

map <- read_sf(
  dsn = "data-es.shp", 
  layer = "data-es")

data <- read.csv2("data-es.csv")

map <- rename(map, annee = carac118,
              long = coordgpsx,
              lat = coordgpsy)

?addTiles
map <- subset(map, as.numeric(map$dep_code) < 100)

table(map$annee)
map$annee <- as.numeric(map$annee)
summary(map$annee)
map$annee_r <- cut(map$annee, c(1901, 1950, 1970, 1980, 1990, 2000, 2010, 2020), include.lowest = TRUE, right = TRUE)
table(map$annee_r)
map$annee_r <- factor(map$annee_r, labels = c("1901-1949", "1950-1969", "1970-1979", "1980-1989", "1990-1999", "2000-2009", "2010-2020"))

pal <- colorFactor(
  palette = "RdBu",
  domain = map$annee_r,
  reverse = TRUE,
  na.color = "transparent"
)



leaflet(map) %>% addTiles() %>%
  addCircleMarkers(~long, ~lat, popup = ~paste(as.character(commune), ":", annee, sep = " "),
                   radius = 2,
                   color = ~pal(map$annee_r))


map_year <- function(year){
  leaflet(map[map$annee <= year,]) %>% 
    setView(lng = 1.8, lat = 46.6, zoom = 6) %>%
    addTiles() %>%
    addProviderTiles("Esri.WorldStreetMap") %>%
    addCircleMarkers(~long, ~lat, popup = ~paste(as.character(commune), ":", annee, sep = " "),
                     radius = 5,
                     stroke = TRUE,
                     color = "black",
                     weight = 0.5,
                     fillColor = ~pal(map[map$annee <= year,]$annee_r), 
                     fillOpacity = 1) %>%
    add_titre(titre = "Terrains de rugby en France", sousTitre = paste("Année", year, sep = " ")) %>%
    addLegend(position = "topleft",
              title = "Année de mise en service",
              pal = pal, values = map[map$annee <= year,]$annee_r, na.label = "Année inconnue",
              opacity = 1) 
}




saveWidget(map_year(2020), file="Carte_final.html")



setwd("/Users/julia/Documents/DESCAMPS 2022/rugby/images")

img <- vector()
for (i in 1901:2020)
{
  name <- paste("annee_", i, sep = "")
  img_name <- paste(name, ".png", sep = "")
  mapshot(map_year(i), file = img_name)
  year_map <- image_read(img_name)
  img <- append(img, year_map)
}


image_append(image_scale(img))
my.animation<-image_animate(image_scale(img), fps = 10, dispose = "previous")
image_write(my.animation, "/Users/julia/Documents/DESCAMPS 2022/rugby/Gif_par_annee.gif")

?image_animate

for (i in 1901:2020)
  {
    animation::saveGIF(
      expr = {
        map_year(1901)
        map_year(1902)
      },
      movie.name = "test"
    )
  }


