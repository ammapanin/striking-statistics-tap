library(tidyverse)
library(countrycode)
library(readxl)
library(magick)


### Setup paths

covid.path <- file.path(normalizePath("~"),
                        "Dropbox",
                        "Work Documents",
                        "World Bank",
                        "amma_striking_stats_local",
                        "covid")
setwd(covid.path)

final.figures.path <- "final figures"


#trend.plot.path <- file.path(final.figures.path)



full.data.link <- "https://covid.ourworldindata.org/data/ecdc/full_data.csv"
full.dt.in <- read.csv(full.data.link)


### Get a list of SSA countries

wb.path <- file.path(shared.data.path, "wb_country_codes.csv")

wb.in <- read.csv(wb.path)

ssa.codes <- wb.in %>%
    filter(region == "Sub-Saharan Africa") %>%
    pull(code)  %>%
    as.character()


### Prepare dataset for plotting

comparison.countries <- c("United States", "China")

dt <- full.dt.in %>%
    mutate(ccode = countrycode(
               sourcevar = .$location,
               origin = "country.name",
               destination ="iso3c"),
           is.ssa =  ifelse(ccode %in% ssa.codes,
                            TRUE, FALSE),
           date = as.Date(date))

dt.ssa <- dt %>%
    filter((is.ssa == TRUE |
            location %in% comparison.countries))

latest.date <- max(dt$date)
latest.date.text <- format(latest.date, "%d %B %Y")

dt.today <- dt %>%
    filter(date== latest.date)


### Define some general parameters

general.theme <- theme_minimal() +
    theme(axis.text.x = element_text(size = 6))


### Plot cases

latest.cases <- dt.ssa %>%
    filter(date == latest.date) %>%
    ggplot(aes(x = ccode, y = total_cases)) +
    geom_point() +
    scale_y_continuous(trans  = "log10") +
    general.theme



### Make the trend plots
translate.date <- function(date.col){
    seq_along(date.col) - 1
}

get.earliest.date <- function(country.dt, n.cases = Ncases){
    earliest.date <- country.dt %>%
        mutate(more.than.n = total_cases >= n.cases) %>%
        filter(more.than.n ==  TRUE) %>%
        pull(date)  %>%
        min()

    names(earliest.date) <- country.dt$location %>%
        unique()  %>%
        as.character()

    return(earliest.date)
}

get.days.since.n <- function(country.dt, n.cases = Ncases){

    earliest.date <- get.earliest.date(country.dt, n.cases)

    more.cases.df <- country.dt %>%
        filter(date >= earliest.date) %>%
        mutate(date.zeroed = translate.date(date))

    return(more.cases.df)
}

double.day.function <- function(doubling.days,
                                time.vec,
                                n.cases = Ncases){
    n.cases * (2  ^ (time.vec/doubling.days))
}

get.doubling.plot.coords <- function(ddf,
                                     max.cases.in = max.cases.plot,
                                     max.days.in =  max.days.plot){
    coords.df.out <- ddf %>%
        filter(number_cases < max.cases.in) %>%
        filter(date.zeroed == max.days.in)

    return(coords.df.out)
}

pretty.doubling.names <- function(xstr){
    paste(gsub("days",
               "doubling every ",
               xstr),
          "days")
}

make.doubling.df <- function(doubling.days.list,
                             time.vec.in,
                             ncases.in = Ncases){

    doubling.times.list <- lapply(
        doubling.days.list,
        double.day.function,
        time.vec = time.vec.in,
        n.cases = ncases.in)

    names(doubling.times.list) <- paste0("days",
                                         doubling.days.list)

    doubling.df <- data.frame(doubling.times.list) %>%
        mutate(date.zeroed = time.vec.in) %>%
        pivot_longer(cols = starts_with("days"),
                     names_to = "doubling_time",
                     values_to = "number_cases")

    plot.names.df <- doubling.df %>%
        group_by(doubling_time) %>%
        group_modify(~get.doubling.plot.coords(.x)) %>%
        ungroup() %>%
        mutate(line_name = pretty.doubling.names(.$doubling_time))

    return(list(doubling.df, plot.names.df))

}

get.levels.of.factor <- function(df.in, factor.name, value.name){
    ordered.out <- df.in %>%
        arrange_(value.name) %>%
        select_(factor.name) %>%
        pluck(1)  %>%
        unique() %>%
        as.character()

    return(rev(ordered.out))
}


add.e4t.branding <- function(plot, plot.name,
                             width.in, height.in){

    plot.png <- paste0(plot.name, ".png")

    ggsave(plot.png,
           width = width.in,
           height = height.in,
           units = "cm",
           plot = plot)

    stats.png <- image_read(plot.png)

    logo <- image_read("e4t_logo.png")%>%
        image_resize("x150")

    twitter <- image_read("e4t_twitter.png") %>%
        image_resize("x90")

    logo.offset <- paste0("+",
                          round((width.in - 1)*100, 0),
                          "+",
                          "0")

    print(logo.offset)
    plot.img <- image_composite(stats.png, logo,
                                offset = logo.offset) #%>%
      #  image_composite(twitter, offset = "+1250+1950")

    image_write(plot.img,
                path = file.path(final.figures.path, plot.png))

    return(plot.img)
}

### Check when different countries crossed the N threshold

Ncases <- 150

country.cross.n.list <- dt %>%
    group_by(location) %>%
    group_map(~get.earliest.date(.x, n.cases = Ncases),
              keep = TRUE)

country.cross.n <- do.call("c", country.cross.n.list)

N.more.than.n <- dt.today %>%
    filter(total_cases > Ncases) %>%
    pull(location) %>%
    as.character() %>%
    length() %>%
    `-`(1)

ssa.N <- dt.ssa %>%
    group_by(location) %>%
    group_modify(~get.days.since.n(.x, n.cases = Ncases)) %>%
    ungroup() %>%
    mutate(location = as.character(location),
           location = factor(
               location,
               levels = get.levels.of.factor(
                   df.in = filter(., date == latest.date),
                   "location", "total_cases")),
           ordered = TRUE)


ssa.only.df <- ssa.N %>%
    filter(is.ssa)

max.cases.plot <- ssa.only.df %>%
    pull(total_cases)  %>%
    max() %>%
    `+`(20)

min.cases.plot <- ssa.only.df %>%
    pull(total_cases)  %>%
    min() %>%
    `-`(1)

max.days.plot <- ssa.only.df %>%
    pull(date.zeroed) %>%
    max() %>%
    `+`(1)


N.countries <- length(ssa.N$location %>% unique())
days.since.n <- ssa.N$date.zeroed %>% unique()

doubling.days.plot <- c(1, 5, 10)
doubling.df.list <- make.doubling.df(doubling.days.plot,
                                     days.since.n)

doubling.df <- doubling.df.list[[1]]
doubling.df.names <- doubling.df.list[[2]]


plot.title.base  <- paste("%s African countries have more",
                          "than %s confirmed COVID-19 cases")

caption.text <- paste(
    "Source: Our World in Data. Plot inspired by FT.",
    sprintf("Last accessed on %s", latest.date.text))

xlab.text <- paste(
    sprintf("Days since case %s was reported", Ncases))

plot.title <- sprintf(plot.title.base, N.countries, Ncases)

trend.plot <- ssa.N %>%
    ggplot(aes(x = date.zeroed,
                   y = total_cases, colour = location)) +
    geom_point() +
    geom_line() +
    geom_line(data = doubling.df,
              inherit.aes = FALSE,
              aes(x = date.zeroed,
                  y = number_cases,
                  linetype = doubling_time),
              colour = "gray") +
    geom_text(data = doubling.df.names,
              inherit.aes  = FALSE,
              aes(x = date.zeroed,
                  y = number_cases,
                  label = line_name),
              colour = "grey67",
              hjust = "right",
              nudge_x = -0.5) +
    scale_y_continuous(trans = "log10",
                       limits = c(min.cases.plot, max.cases.plot)) +
    scale_x_continuous(limits = c(0, max.days.plot)) +
    labs(caption = caption.text)  +
    xlab(xlab.text) +
    ylab("Total number of cases") +
    guides(linetype = "none")  +
    general.theme +
    theme(legend.title = element_blank(),
          plot.title.position = "plot",
          plot.caption.position = "plot",
          plot.caption = element_text(
              hjust = 0,
              size = 7,
              margin = margin(t = 0.5, unit = "cm")))


plot.final <- add.e4t.branding(trend.plot,
                               "SSA_more_than_100a",
                               width.in = 26.9,
                               height.in = 15.5)
