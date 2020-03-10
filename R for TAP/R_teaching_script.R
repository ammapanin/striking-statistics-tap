
## We will use tools from the "tidyverse" packages
## In R, packages are loaded with the function 'library'
library(tidyverse)
library(magick)

## Define the path relative to the working directory
## Read the data into an R dataframe using read.csv
dt.export.path <- "data/time_to_export.csv"
landlocked.path <- "data/landlocked_countries.csv"

dt.export.path <- "data/export_documentary_compliance.csv"
export.in <- read.csv(dt.export.path)
landlocked.dt <- read.csv(landlocked.path)

## It is good practice to inspect data before using it
## The following functions will print output to your console

## 'dim' gives the number of rows and columns of a dataframe
## 'head' prints out the first six rows of the dataframe

dim(export.in)
head(export.in)

export.renamed <- export.in
ncol.export <- ncol(export.in)
year.names <- names(export.in)[5:ncol.export]
new.year.names <- paste0("y", substr(year.names, 2, 5))
names(export.renamed)[5:ncol.export] <- new.year.names

landlocked.countries <- landlocked.dt %>%
    filter(Indicator == "Landlocked") %>%
    select(ccode = Country.ISO3,
           is.landlocked = X2017)

regions <- c("Sub-Saharan Africa",
             "East Asia & Pacific",
             "Latin America & Caribbean",
             "European Union",
             "Euro area")

plot.order <- c("Sub-Saharan African countries" ="ssa.country",
                "Comparator regions" = "comparator.region",
                "IDA & IBRD countries only" = "ida.region")

plot.labels <- data.frame(x = rep(c("LAB", "LAB2"), each = 3),
                          y = 0,
                          text = c(names(plot.order), rep("",3)),
                          plot.group = plot.order)

export.use <- export.renamed %>%
    select(country = Country.Name,
           ccode = Country.Code,
           time.hours = y2016) %>%
    filter(ccode != "") %>%
    merge(landlocked.countries,
          by.x = "ccode",
          by.y = "ccode",
          all.x = TRUE) %>%
    mutate(time.hours = as.numeric(as.character(time.hours)),
           is.landlocked = ifelse(is.na(is.landlocked), 0,
                                  is.landlocked),
           ida.region = grepl("IDA & IBRD", country),
           country = gsub(" \\(IDA & IBRD countries\\)",
                          "",
                          country),
           comparator.region = country %in% regions,
           country = as.character(country),
           ccode = as.character(ccode),
           ccode = ifelse(ida.region == TRUE,
                          country,
                          ccode),
           ccode =  ifelse(comparator.region == TRUE,
                           country,
                           ccode)) %>%
    filter(!is.na(time.hours)) %>%
    filter(ccode != "Euro area")

ordered.countries.list <- export.use %>%
    arrange(time.hours)  %>%
    select(country) %>%
    pluck(1)

ordered.countries.char <- c(as.character(ordered.countries.list),
                            "LAB",
                            "LAB2") %>%
    unique()

export.plot.df <- export.use %>%
    mutate(country = factor(country,
                            levels = ordered.countries.char,
                            ordered = TRUE),
           plot.group = ifelse(ida.region == TRUE,
                               "ida.region",
                        ifelse(comparator.region == TRUE,
                               "comparator.region",
                               "ssa.country")),
           plot.group = recode(plot.group, !!!plot.order),
           plot.group = factor(plot.group,
                               levels = plot.order,
                               ordered = TRUE),
           is.landlocked = ifelse(is.landlocked == 0,
                                  "Has sea port",
                                  "Landlocked"))%>%
    plyr::rbind.fill(data.frame(country = rep(c("LAB","LAB2"),3),
                                time.hours = NA,
                                plot.group = plot.order))

plot.blues <- c("dodgerblue4",
                "grey56")

names.to.plot <- setdiff(ordered.countries.char,
                         c("LAB",  "LAB2"))

axis.title <- paste("Hours needed to complete documentary",
                    "compliance for export (2016)")

chart.title <- paste("Is Africa ready for AfCTA?")
chart.subtitle <-  paste(
    "Documentary compliance for exports",
    "varies widely",
    "— it is not a problem of being landlocked",
    "or poor.")

chart.notes <- paste(
    "Data from World Development Indicators.",
    "\nSeries name and code:",
    "Time to export, documentary compliance (hours),",
    "IC.EXP.TMDC")


export.plot <- ggplot(export.plot.df) +
    geom_col(aes(x = country,
                 y = time.hours,
                 fill = is.landlocked),  width = 0.8) +
    coord_flip() +
    facet_grid(rows = vars(plot.group),
               space = "free_y",
               scales = "free_y") +
    ylab(axis.title) +
    labs(title = chart.title,
         subtitle = chart.subtitle,
         caption = chart.notes) +
    scale_fill_manual(values = plot.blues) +
    scale_y_continuous(expand = c(0,0)) +
    scale_x_discrete(breaks = names.to.plot)+
    geom_text(data = plot.labels,
              aes(x =  x, y = y, label = text,
                  size = 2),
              nudge_x = 1,
              hjust = 0,
              size = 3)  +
    theme_minimal()  +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_text(size = 6),
          axis.title.x = element_text(
              margin = margin(t = 0.2, unit = "cm")),
          strip.text = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.direction = "horizontal",
          legend.position =  c(0.6, 0.7),
          legend.title = element_blank(),
          plot.caption = element_text(
              hjust = 0,
              size = 7,
              margin = margin(t = 0.5, unit = "cm")),
          plot.subtitle = element_text(
              margin = margin(b = 0.5, unit = "cm")),
          plot.title.position = "plot",
          plot.caption.position = "plot")


add.e4t.branding <- function(plot, plot_name){
    ggsave("stats_map.png",
           width = 21.5,
           height = 17.5,
           units = "cm",
           plot = plot)

    stats.png <- image_read("stats_map.png")

    logo <- image_read("e4t_logo.png")%>%
        image_resize("x150")

    twitter <- image_read("e4t_twitter.png") %>%
        image_resize("x90")

    fred <- image_composite(stats.png, logo,
                            offset = "+2050+0") %>%
        image_composite(twitter, offset = "+1950+1950")

    return(fred)
}


