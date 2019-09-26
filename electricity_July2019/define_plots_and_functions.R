### Code to produce electricity statistics for CEoG
### Contact: aserwaahpanin@worldbank.org
### Created: 19 July 2019, last updated: 26 July 2019

##! Code assumes that R session is running from the local directory
## if not, setwd(~folder_containing_this_script)

### Load libraries  --------------------------------------------------
library(tidyverse)
library(waffle)

electricity.general.path <- "electricity_general_data.csv"
electricity.sources.path <- "electricity_sources_data.csv"

general.dt <- read.csv(electricity.general.path)
sources.dt <- read.csv(electricity.sources.path)

africa.in <- read.csv("africa_total_iea.csv")
gWh.to.kWh <- 10 ^ -3

africa <- africa.in %>%
    pivot_longer(cols = -1,
                 names_to = "year",
                 values_to = "amount",
                 names_prefix = "X") %>%
    group_by(year) %>%
    mutate(production = amount * gWh.to.kWh,
           total.production = sum(production),
           pc.from.source = production / total.production,
           country = "Africa",
           ccode = "AFR",
           series = "electricity source",
           unit = "percent")

### General plotting themes ------------------------------------------
anchor.size <- 18
axis.text.size <- 0.8 * anchor.size
axis.ticks.size <- 0.55 * anchor.size
legend.title.size <- 0.7 * anchor.size
legend.text.size <- 0.65 * anchor.size

base.theme <- theme(axis.title = element_text(
                        size = axis.text.size),
                    axis.title.y = element_text(
                        margin = margin(t = 0, r = 10, b = 0, l = 0,
                                        unit = "pt")),
                    axis.text = element_text(colour = "black",
                                             size = axis.ticks.size),
                    legend.text = element_text(size = legend.text.size),
                    legend.title = element_text(
                        size = legend.title.size),
                    panel.background = element_rect(fill = "white"),
                    panel.grid.major = element_line(
                        colour = "grey",
                        linetype = "dotted",
                        size = 0.3),
                    axis.ticks = element_line(colour = "grey",
                                              linetype = "dotted",
                                              size = 0.3))


### Make figures of total production by energy type ------------------

production.dt <- merge(sources.dt %>%
                       select(-unit),
                       general.dt %>%
                       filter(series == "generation") %>%
                       select(country.year,
                              unit,
                              total.production = value,
                              population),
                       by = "country.year",
                       all.x = TRUE)%>%
    mutate(production = value * total.production) %>%
    group_by(country) %>%
    select(-c(country.year, ccode.year, series,
              unit,
              total.production)) %>%
    rename(pc.from.source = value) %>%
    group_by(country, year) %>%
    mutate(total.production=sum(production, na.rm = TRUE)) %>%
    ungroup() %>%
    plyr::rbind.fill(africa) %>%
    group_by(country) %>%
    mutate(avg.total.production = mean(total.production, na.rm = TRUE),
           max.total.production = max(total.production, na.rm = TRUE)) %>%
    ungroup()

ordered.country.names <- production.dt %>%
    select(country, avg.total.production) %>%
    unique() %>%
    arrange(avg.total.production) %>%    pull(country) %>%
    as.character()

aggregate.dt <- production.dt %>%
    mutate(country = factor(country,
                            levels = ordered.country.names,
                            ordered = TRUE))

low.production.df <- filter(aggregate.dt,
                            max.total.production <= 5)

med.production.df <- filter(aggregate.dt,
                            max.total.production > 5 &
                            max.total.production <= 10)

high.production.df <- filter(aggregate.dt,
                             max.total.production > 10 &
                             max.total.production <= 30)

mega.production.df <- filter(aggregate.dt,
                             max.total.production > 30)

dfs.in <- list(low  = low.production.df,
               med = med.production.df,
               high = high.production.df,
               mega = mega.production.df)

breaks.in <- list(low = seq(0, 5.5, 1),
               med =  seq(0, 10.5, 2.5),
               high  =  seq(0, 30, 5),
               mega = seq(0, 825,  50))


## Plot production and energy mix

plot.energy.mix <- function(category){
    df.in <- dfs.in[[category]]
    break.in <- breaks.in[[category]]
    plot.out <- ggplot(df.in,
                       aes(x = year,
                           y = production,
                           fill = electricity.source)) +
        geom_col() +
        facet_wrap(~country, ncol = 6) +
        scale_y_continuous(breaks = break.in)+
        scale_x_discrete(breaks = c("2000", "2005", "2010", "2015")) +
        theme(axis.text.x = element_blank()) +
        base.theme +
        labs(fill = "Electricity Source") +
        ylab(label = "Electricity produced in one year, Billions of Kwh")+
    theme(axis.text.x = element_text(size = 0.4*legend.text.size),
          axis.title.x = element_blank())

    return(plot.out)
}

energy.mix.plots <- lapply(names(dfs.in), plot.energy.mix)


### Make figure of percentage energy mix

energy.mix.percentages.plot <- ggplot(sources.dt,
                                      aes(x = year,
                                          y = value,
                                          fill = electricity.source)) +
    geom_col() +
    facet_wrap(~country, ncol = 5) +
    base.theme +
    ylab(label = paste0("Percentage of total electricity produced from",
                        "a given source")) +
    #scale_x_discrete(breaks = c(2000, 2005, 2010, 2015)) +
    theme(panel.grid.major = element_line(colour = "grey",
                                          size = 0.3)) +
    theme(axis.text.x = element_text(size = 0.6*legend.text.size),
          axis.title.x = element_blank()) +
    labs(fill = "Electricity Source")


plot.for.blog <- energy.mix.percentages.plot +
    labs(title = "Energy mix for different sub-Saharan African Countries, 1999 to 2016")


### Create per-capita figures

per.capita.dt.start <- production.dt %>%
    filter(ccode != "AFR") %>%
    mutate(production.per.capita = 10 ^ 6 * production / population,
           total.production.per.capita  = 10 ^ 6 *total.production / population) %>%
    group_by(country) %>%
    mutate(avg.production.per.capita = mean(total.production.per.capita,
                                            na.rm = TRUE)) %>%
    ungroup()

seychelles.dt <- data.frame(year = as.character(seq(2000, 2016)),
                            production.per.capita = 3600/1000,
                            avg.production.per.capita = 3600/1000,
                            country = "Seychelles\n(average)",
                            comparator = "seychelles")

per.capita.dt.seychelles <- plyr::rbind.fill(per.capita.dt.start, seychelles.dt)

per.capita.ordered.countries <- per.capita.dt.seychelles %>%
    arrange(avg.production.per.capita) %>%
    select(avg.production.per.capita, country) %>%
    unique() %>%
    pull(country)

per.capita.dt <- per.capita.dt.seychelles %>%
    mutate(country = factor(country,
                            levels = per.capita.ordered.countries,
                            ordered = TRUE),
           comparator = ifelse(country %in% c("Malaysia",
                                              "Indonesia",
                                              "Vietnam"),
                               "comparator",
                               "africa"),
           comparator = ifelse(country == "Seychelles\n(average)",
                               "seychelles",
                               comparator))
## add.seychelles average


per.capita.plot <- per.capita.dt %>%
    ggplot(aes(x = year,
               y = production.per.capita,
               fill = comparator)) +
    geom_col() +
    facet_wrap(~country)+
    scale_x_discrete(breaks = c("2000", "2005", "2010", "2015")) +
    base.theme +
    xlab(label = "Year") +
    ylab(label = "Total electricity generated per capita, Thousands of kWh") +
    theme(axis.text.x = element_text(size = 0.6*legend.text.size),
          axis.title.x = element_blank(),
          legend.position = "none")+
    scale_fill_manual(values =  c("grey56", "navajowhite", "lavender"))


### Create some individual figures

country.names <- sources.dt$country%>%
    unique() %>%
    as.character()

country.list <- seq_along(country.names)
names(country.list) <- country.names

capacity.df <- general.dt %>%
    filter(series == "capacity")

generation.df <- general.dt %>%
    filter(series == "generation")

consumption.df <- general.dt %>%
    filter(series == "consumption")

wb.access <- general.dt %>%
    filter(series ==  "access")

make.country.capacity.plot <- function(country.in){
    country.df <- capacity.df %>%
        filter(country == country.in)

    plot.out <- ggplot(country.df,
                       aes(x = year, y  = value)) +
        geom_col() +
        ylab(label = "Total generating capacity available per year, mKWh")

    return(plot.out)
}

make.country.consumption.plot <- function(country.in){
    country.df <- consumption.df %>%
        filter(country == country.in)

    plot.out <- ggplot(country.df,
                       aes(x = year, y  = value)) +
        geom_col() +
        ylab(label = "Total consumption per year, Billion KWh")+
        theme(axis.title.x = element_blank())

    return(plot.out)
}

make.country.mix.plot <- function(country.in){
    country.df <- aggregate.dt  %>%
        filter(country == country.in)

    ggplot(country.df)
}

make.country.waffle.plot <- function(country.in){
    access <- wb.access %>%
        filter(country == country.in & year == 2015) %>%
        pull(value)

    waffle.colours <- c("#fdc086", "#386cb0")

    pc <- round(access * 100, 0)

    access.ratio <- c(pc, 100 - pc)
    names(access.ratio) <- c("electricity access", "no access")
    waffle.out <- waffle(parts = access.ratio,
                         colors = waffle.colours)
    return(waffle.out)
}

calculate.but_access.fact <- function(country.in){
    access <- wb.access %>%
        filter(country == country.in & year == 2015) %>%
        pull(value) %>%
        as.character() %>%
        as.numeric()

    pc <- round(access * 10, 0)

    return(pc)
}

calculate.killer.fact <- function(country.in){

    country.df <- consumption.df  %>%
        filter(country == country.in)

    last.pop.df <- filter(country.df,
                          !is.na(population))  %>%
        tail(1) %>%
        mutate(fridge =   (5 * 10 ^ 9 * value / population) /500)

    facts.out <- list(year = last.pop.df$year,
                      country = country.in,
                      nationality =  country.in,
                      fridges = last.pop.df$fridge)
    return(facts.out)
    }


calculate.export <- function(country.in){

    country.import <- general.dt %>%
        filter(year == 2015  &
               country == country.in &
               series == "net imports") %>%
        pull(value) %>%
        as.character() %>%
        as.numeric()

    export.status <- if(country.import < 0){
                         "a net exporter"
                     }else{
                         if(country.import > 0){
                             "a net importer"
                         }else{
                             "neither a net importer nor a net exporter"
                         }}
    return(export.status)
    }



make.production.plot <- function(country.in){
    cols.to.keep <- c("imports",
                  "distribution losses",
                  "net imports",
                  "consumption",
                  "generation",
                  "exports",
                  "generation.local")

    negative.energy.use <- c("distribution losses",
                             "exports")

    positive.energy.use <- c("imports",
                             "generation.local")


    energy.use <- c(negative.energy.use,
                positive.energy.use)


    generation.total <- c("distribution losses",
                          "exports",
                          "generation.local")
    consumption.total <- c("imports",
                       "generation.local")

    tdt <- general.dt %>%
        filter(year == 2015  &
               country == country.in &
               series != "capacity") %>%
        pivot_wider(names_from = series, values_from = value) %>%
        mutate(generation.local = generation - exports) %>%
        pivot_longer(cols = one_of(cols.to.keep),
                     names_to = "series",
                     values_to = "value") %>%
        filter(series %in% energy.use) %>%
        mutate(series = factor(series,
                               levels = c(energy.use,  "total"),
                               ordered  = TRUE),
               negative = ifelse(series %in% negative.energy.use,
                                 TRUE,
                                 FALSE),
               value = ifelse(series %in% negative.energy.use,
                              -1 * value,
                              value))  %>%
        arrange(series)

    tdt.generation <- tdt %>%
        mutate(value = ifelse(series %in% generation.total,
                              value, NA),
               country = "generation",
               series = "total")

    tdt.consumption <- tdt %>%
        mutate(value = ifelse(series %in% consumption.total,
                              value, NA),
               country = "consumption",
           series = "total")

    tdt.total <- rbind(tdt.consumption, tdt.generation, tdt)  %>%
        mutate(series = factor(series,
                               levels = c(energy.use,  "total"),
                               ordered  = TRUE),
               country = factor(country,
                                levels = c(country.in,
                                           "generation",
                                           "consumption"),
                                ordered = TRUE))

    commapos <- function(x, ...) {
        format(abs(x), big.mark = ",", trim = TRUE,
               scientific = FALSE, ...)
    }

    my.colours <- c('#a1dab4',
                    '#41b6c4',
                    '#2c7fb8',
                    '#253494',
                    "#999999")

    my.labels <- data.frame(
        country = c("consumption",
                    "generation"),
        label = c("Total consumed", "Total generated"))

    plot.out <- ggplot() +
        geom_bar(data = filter(tdt.total, negative == TRUE),
                 aes(x = country,
                     y = value,
                     fill = series),
                 stat = "identity") +
        geom_bar(data = filter(tdt.total, negative == FALSE),
                 aes(x = country,
                     y = value,
                     fill = series),
                 stat = "identity") +
        scale_y_continuous(labels = commapos) +
        coord_flip() +
        scale_fill_manual(values = my.colours)  +
        geom_text(data = my.labels,
                  aes(x = country,
                      y = 0.2,
                      label = label), hjust = 0) +
        ylab(label = "Electricity production, Billion kWh") +
        scale_x_discrete(breaks = c(country.in)) +
        base.theme +
        theme(legend.title = element_blank(),
              axis.title.x = element_text(
                  size = 1.2 *  axis.text.size),
              axis.text.y = element_text(
                  size = 1.3 * axis.text.size),
              legend.text = element_text(
                  size = 1.4 * legend.text.size),
              axis.title.y = element_blank())

    return(plot.out)
}


killer.fridge <- "In %s, the entire economy of %s consumed as much electricity as if every %s household were running %d fridges for 1 year."

but.access <- "But only %s in 10 households have access to electricity"

net.export.text <- "%s is %s"



## library(grid)
## library(gtable)

## gt1 <- ggplot_gtable(ggplot_build(energy.mix.plots[[1]]))
## fixed.height <- gt1$heights[7]

## gt <- ggplot_gtable(ggplot_build(energy.mix.plots[[2]]))
## gt$heights[7] <- 0.1 * fixed.height


## grid.draw(gt)

