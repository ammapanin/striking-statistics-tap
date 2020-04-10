##  Plot infection map

library(countrycode) ## for the countrycode commands
library(sf) ## for working with geo data
library(tidyverse)


other.virus.plot.path <- file.path(final.figures.path,
                             "world_covid_cases.png")


mers.url <- "https://www.who.int/emergencies/mers-cov/en/"

avian.url <- "https://www.who.int/influenza/human_animal_interface/2020_01_20_tableH5N1.pdf?ua=1"

sars.url <- "https://www.who.int/csr/sars/country/2003_07_11/en/"


covid.cum <- dt.today %>%
    filter(location == "World")

covid.countries <- dt.today %>%
    filter(total_cases > 0) %>%
    pull(location) %>%
    unique() %>%
    length() %>%
    `-`(1)

virus.df.base <- do.call(rbind,
    list(c("Avian Flu", 861, 455, 17),
         c("SARS",8437,813,29),
         c("MERS", 2494, 858, 27),
         c("COVID-19", covid.cum$total_cases,
           covid.cum$total_deaths,
           covid.countries)))  %>%
    data.frame()

names(virus.df.base) <- c("virus","cases", "deaths", "countries")



For more: Coronavirus infections news
27

(SARS, Avian Flu, and MERS) in terms of the number of infected cases and fatalities. As of 27 March 2020, the number of confirmed cases worldwide exceeds 600,000 —an amount that is significantly higher than the 8096 cases of SARS in 2002. The number of fatalities surpasses the 25,000 deaths —which is higher than the 858 deaths from MERS in 2012. Finally, SARS and MERS affected 29 and 28 countries and territories, respectively —as opposed to the 19 countries and terr


plot.final <- add.e4t.branding(plot = world.plot,
                               plot.name = "world_covid_cases",
                               width.in = 24.64,
                               height.in = 17.05)


