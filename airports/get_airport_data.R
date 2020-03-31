### TODO:tidy upcode

library(rvest)
library(tidyverse)

airports <- read.csv("airports.csv")
countries <- read.csv("countries.csv")


african.airports <- airports %>%
    filter(continent == "AF")

open.african.airports <- african.airports %>%
    filter(type != "closed")

start.date <- 17
available.dates <- seq(start.date, start.date + 6)
available.hours <- seq(0, 22, 2)

table.columns <- c("flight",
                   "departure_time",
                   "arrival_time",
                   "origin")

make.airport.url <- function(airport.code,
                             date.in,
                             hour.in){

    airport.url.base <- paste0("https://www.flightstats.com/",
                               "v2/flight-tracker/departures/",
                               "%s/?year=2020&month=3&",
                               "date=%s&hour=%s&minute=0")

    airport.url <- sprintf(airport.url.base,
                           airport.code,
                           date.in,
                           hour.in)
    return(airport.url)
}

problem.urls <- c()

get.flights.from.url <- function(url.in){
    output <- tryCatch({
        html <- read_html(url.in)
        details <- html_nodes(html, ".KlAnq")
        cells <- html_text(details, trim = TRUE)
        flights.df <- matrix(cells, ncol = 4, byrow = TRUE) %>%
            data.frame()
        names(flights.df) <- table.columns
        return(flights.df)
    },

    error = function(error.msg){
        message(error.msg)
        new.problem.urls <- c(problem.urls, url.in)
        assign("problem.urls", new.problem.urls, envir= .GlobalEnv)
        return(NULL)
    }
    )}

get.flights.per.day <- function(date.in, airport.code.in){
    urls <- lapply(
        available.hours,
        function(xhour){
            make.airport.url(airport.code.in,
                             date.in,
                             xhour)
        })

    flight.dfs <- lapply(urls,get.flights.from.url)
    day.df <- do.call(rbind, flight.dfs) %>%
        unique() %>%
        mutate(day = date.in,
               month = 3,
               year = 2020)

    return(day.df)
}

get.flights.by.airport <- function(airport.in){
    print(sprintf("Working on airport: %s", airport.in))

    airport.day.dfs <- lapply(available.dates,
                              get.flights.per.day,
                              airport.code.in = airport.in)

    airport.df <- do.call(rbind, airport.day.dfs) %>%
        mutate(airport = airport.in)

    return(airport.df)
}


large.african.airport.codes <- open.african.airports %>%
    filter(type == "large_airport") %>%
    pull(iata_code) %>%
    as.character()


african.flights <- lapply(large.african.airport.codes,
                          get.flights.by.airport)

saveRDS(african.flights, "african_flight_list_of_dfs.RDS")



african.flights.departures <- lapply(large.african.airport.codes,
                                     get.flights.by.airport)

problem.departure.airports <- c("ROB", "NIM", "QUO")



african.flights.departures <- lapply(large.african.airport.codes,
                                     get.flights.by.airport)


extra.departures <- lapply(problem.departure.airports,
                           get.flights.by.airport)

african.flights.df <- do.call(rbind, african.flights)

african.flights.departures.df <- do.call(rbind,
                                         c(extra.departures,
                                           african.flights.departures))

names(african.flights.departures.df)[4] <- "destination"



write.csv(x = african.flights.df,
          file = "african_arrivals.csv",
          row.names = FALSE)

write.csv(x = african.flights.departures.df%>%
              unique(),
          file = "african_departures.csv",
          row.names = FALSE)
