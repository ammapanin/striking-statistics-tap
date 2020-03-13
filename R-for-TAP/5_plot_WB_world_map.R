## Sample code to plot data using World Bank shapefiles
## Updates on 13 March 2020


library(countrycode) ## for the countrycode commands
library(sf) ## for working with geo data
library(tidyverse)


## Read-in the shapefile; this was downloaded by Alexis according
## to the World Bank's official guidelines
##
## world.boundaries return an 'sf' ofbject which is a special
## type of data.frame with a column called 'geometry'
world.shapefile.path <- file.path("datasets",
                                  "WB country polygons",
                                  "WB_CountryPolys.shp")
world.boundaries.df <- read_sf(world.shapefile.path)

## We can already plot the outlines of the world countries using
## ggplot
## geom_sf is the layer to add for geographical data; just as
## geom_point says we want a scatter chart or
## geom_col says that we want a bar char
## geom_sf says that we want a map
## and then we can customise it as ususal! (as we will do below)
world.plot <- ggplot(data = world.boundaries.df)+
    geom_sf()

## Now we will try to plot Africa, highlighting landlocked
## countries. We will need to do two things to achieve this
##
## 1. get a subset of the world.boundaries to contain only
##    African countries
## 2. attach the landlocked data

### 1. Get a character vector of African country names ---------------

## ** About the countrycode function **
## We are going to use the`countrycode` function from the
##  countrycode package
## This function allows us to convert any geographical string
##  representation into another e.g. to get from "Ghana" to "GHA"
## It is good practice to use a function like this when working across
##   datasets to ensure that names are consistent

## Below, I am going to make a data frame with two columns
## -- 'country' will be a column of the ISO3c code, e.g. "GHA"
## -- 'continent' will be the name of the continent
## Then, I will filter the data frame to only contain African rows
## Then, I will select the column that has names

## I get unique codes just to be sure
world.iso.codes <- world.boundaries.df$ISO_Codes %>%
    unique()

world.continents <- countrycode(sourcevar = world.iso.codes,
                                origin = "iso3c",
                                destination = "continent")

world.names.df <- data.frame(country = world.iso.codes,
                             continent = world.continents)

## I use pull here rather than  because I want the vector is contained
## in the 'country' column rather than the list
## It's a factor and I prefer to work  with characters until I need the
## factors (e.g. for plotting) so I convert to charater at the end
african.country.names <- world.names.df %>%
    filter(continent ==  "Africa") %>%
    pull(country) %>%
    as.character()


### 2. Subset the geo data to contain only African countries and  ----
###    add a column with landlocked status ---------------------------

africa.boundaries.df <- world.boundaries.df %>%
    filter(ISO_Codes %in% african.country.names)

## We can already plot the Africa outline!
africa.plot <- ggplot(data = africa.boundaries.df)+
    geom_sf()

## Now load a WB dataset on landlocked countries
## -- fyi, data downloaded from here:
##  https://tcdata360.worldbank.org/indicators/e8615298

landlocked.dt.path <- file.path("datasets",
                                "landlocked_countries.csv")

landlocked.df.in <- read.csv(landlocked.dt.path)

## The dataset actually contains a lot of information on FCV status
## across a number of years
## -- we only want to select the data about being landlocked
## -- and there is only landlocked data for the year 2017

landlocked.df <- landlocked.df.in %>%
    filter(Indicator == "Landlocked") %>%
    select(Country.ISO3,
           is.landlocked = X2017)


## Now we merge the two data frames: the africa boundaries and
##  the landlocked information
## all.x = TRUE ensures that we only keep matches that occur in
##  africa.boundaries.df
data.to.plot <- merge(africa.boundaries.df,
                      landlocked.df,
                      by.x  = "ISO_Codes",
                      by.y = "Country.ISO3",
                      all.x = TRUE)


## Now we can plot African countries and fill in a colour for the
## landlocked ones!

## I like to add theme_minimal because it takes away the gray of the
## default ggplot
landlocked.africa.plot <- ggplot(data.to.plot) +
    geom_sf(aes(fill = is.landlocked)) +
    theme_minimal()

## View with
landlocked.africa.plot

## Potential further customisation
## - change the colours
## - let ggplot know that is landlocked is a binary variable
## - countrycode did not find Western Sahara, manually fix that
## - remove lines of latitude and longitude, etc.
## - ...
