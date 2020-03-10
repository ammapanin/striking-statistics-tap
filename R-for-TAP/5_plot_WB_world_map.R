## Sample code to plot World Bank map

library(countrycode)
library(tidyverse)
library(sf)

world.shapefile.path <- file.path("datasets",
                                  "WB country polygons",
                                  "WB_CountryPolys.shp")
world.boundaries <- read_sf(world.shapefile.path)

continental.african.countries <- world.boundaries$ISO_Codes %>%
  unique() %>%
  data.frame() %>%
  rename("country" = 1) %>%
  mutate(continent = countrycode(sourcevar = .$country,
                                 origin = "iso3c",
                                 destination = "continent"),
         country = countrycode(sourcevar = .$country,
                               origin = "iso3c",
                               destination = "iso3c")) %>%
  filter(continent == "Africa") %>%
  pull(country)

## Not used for anything for the moment
african.boundaries <- world.boundaries %>%
    filter(ISO_Codes %in% continential.african.countries)

map.plot <- ggplot(world.boundaries) +
       geom_sf()

## View with; will take a while
map.plot
