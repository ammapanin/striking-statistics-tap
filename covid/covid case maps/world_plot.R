##  Plot infection map

library(countrycode) ## for the countrycode commands
library(sf) ## for working with geo data
library(tidyverse)



main.dir <- file.path(normalizePath("~"),
                      "Dropbox",
                      "Work Documents",
                      "World Bank",
                      "amma_striking_stats_local",
                      "covid")

shared.data.path <- file.path(main.dir, "shared data")
final.figures.path <- file.path(main.dir,"final figures")


world.plot.path <- file.path(final.figures.path,
                             "world_covid_cases.png")

world.shapefile.path <- file.path(shared.data.path,
                                  "WB country polygons",
                                  "WB_CountryPolys.shp")
world.boundaries.df <- read_sf(world.shapefile.path)


covid.dt <- dt  %>%
    filter(date == latest.date)

category.names <- c("less than 10",
                    "10 to 100",
                    "100 to 1000",
                    "1000 to 10000")

covid.map.df <- world.boundaries.df %>%
    merge(covid.dt,
          by.y = "ccode",
          by.x = "ISO_Codes") %>%
    mutate(count.categories = cut(total_cases,
                                  breaks = c(0, 10 ^ seq(1, 5))))


caption.text.base <- paste("Source: Our World in Data.",
                           "Last accessed on %s.")

caption.text <- sprintf(caption.text.base, latest.date.text)


world.plot <- ggplot(data = covid.map.df) +
    geom_sf(aes(fill = total_cases)) +
    theme_minimal() +
    labs(caption = caption.text) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          panel.grid = element_blank(),
          legend.position =  "bottom",
          legend.direction = "horizontal",
          plot.caption = element_text(hjust = 0),
          plot.caption.position = "plot") +
    scale_fill_gradient(name = "Total number of cases",
                        trans = "log10",
                        low = "grey95",
                        high = "royalblue4",
                        breaks = c(1, 100, 10000),
                        labels = c("0", "100", "10000"),
                        guide = guide_colourbar(barwidth = 10,
                                                units = "cm"))

plot.final <- add.e4t.branding(plot = world.plot,
                               plot.name = "world_covid_cases",
                               width.in = 24.64,
                               height.in = 17.05)



bob <- read_excel("/Users/ammapanin/Downloads/OxCGRT_Download_latest_data.xlsx")
