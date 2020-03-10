## Script to followalong for introductory R TAP Tutorial
## March 2020
## apanin@worldbank.org

## This is script 2/4 in the series. We will do the following:
## --load the data as it is downloaded from the WDI
## --wrangle it into a form that we can use for easy analysis


## Clear the workspace
rm(list = ls())

## Load the tidyverse package
library(tidyverse)

## Define the paths relative to the working directory
## Read the data into an R dataframe using read.csv
## Do some basic data checks
export.df.path <- "datasets/export_times_clean.csv"
export.df.in <- read.csv(export.df.path)

dim(export.df.in)
head(export.df.in)

### Prepare the data frame for plotting -----------------------------
## There will be more steps compared to '2_plot_intermediate'
## -- we will remove 'IDA&IBRD'from country names since it is redundant
## -- we will add manual labels for the facets to make them readable
##    this is a hack:
##    we will add some fake countries to the plot
##    and rather than adding a bar, we will add our desired labels

## First, create the list of countries ordered by the time in hours
## we will use for the country name factor
## -- this time, also add two fake countries to the list
ordered.countries <- export.df.in %>%
    arrange(time.hours)  %>%
    select(country) %>%
    pluck(1) %>%
    as.character() %>%
    unique()

ordered.countries.list <- c(ordered.countries,
                            "LAB",
                            "LAB2")

## Ultimately, we will not want to show the words LAB and LAB2 on the
## plot so we need a list of only the countries we want to show
## 'setdiff' returns the difference of two lists
countries.to.plot <- setdiff(ordered.countries.list,
                             c("LAB", "LAB2"))

## Order the regional groupings
ssa.region.lab <- "Sub-Saharan African countries"
comparator.region.lab <- "Comparator regions"
ida.region.lab <- "IDA & IBRD countries only"

## This will be used later
plot.order <- c(ssa.region.lab,
                comparator.region.lab,
                ida.region.lab)

## Now begin preparing the data frame for plotting
export.df.to.plot.base <- export.df.in %>%
    mutate(
        ## begin working with the 'country' variable as a character
        country = as.character(country),
        ## Remove the phrase "(IDA & IBRD countries)"
        ## from the actual names usig gsub
        ## The additional '\\' next to the brackets are there becuase
        ## we are using a 'regular expression'
        ## Don't worry about it too much for themoment
        country = gsub(" \\(IDA & IBRD countries\\)",
                       "",
                       country),
        ## Now turn country back into a factor with the levels
        ## we defined above
        country = factor(country,
                         levels =  ordered.countries,
                         ordered = TRUE),
        plot.group = factor(plot.group,
                            levels =plot.order,
                            ordered = TRUE))


## Create and add the label data frame with 'useless' data
##  this data frame provides data to plot 'NA' at LAB and LAB2
## By plotting nothing, we leave space for the custon labels
## This dummy data frame should have the same columns as the main
##  export data frame

## Note use of the 'rep' function which creates a vector
##  that repeats the first argument n times
additional.labels.df <- data.frame(country = rep(c("LAB","LAB2"), 3),
                                   time.hours = NA,
                                   plot.group = plot.order,
                                   ccode = NA,
                                   is.landlocked = NA)

## The function'rbind' sticks data frames together
##  by stacking them vertically to extend the number of rows
##  ('cbind' sticks data frames together by extending the number of
#     columns)
export.df.to.plot <- rbind(export.df.to.plot.base,
                           additional.labels.df)


### Begin plotting ---------------------------------------------------

## Let's choose some of our own colors!
## A list of colour names
##    http://sape.inf.usi.ch/quick-reference/ggplot2/colour

plot.blues <- c("dodgerblue4", "grey56")

## Define some labels for the chart
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

## We finally also need a data frame of labels to plot
## These will be used to label the facets
labels.df.to.plot <- data.frame(
    x = rep(c("LAB", "LAB2"), each = 3),
    y = 0,
    text = c(plot.order, rep("",3)),
    plot.group = plot.order)

## Make the plot -----------------------------------------------------
export.plot.0 <- ggplot(export.df.to.plot) +
    geom_col(aes(x = country,
                 y = time.hours,
                 ## fill tells ggplot which variable in the dataset
                 ##  to assign to the colours
                 fill = is.landlocked),  width = 0.8) +
    coord_flip() +
    ## facet_grid creates separate plot areas according to a variable
    ##  in the dataset
    facet_grid(rows = vars(plot.group),
               space = "free_y",
               scales = "free_y") +
    ## change the y axis title and add title, subtitle etc.
    ylab(axis.title) +
    labs(title = chart.title,
         subtitle = chart.subtitle,
         caption = chart.notes)

export.plot.1 <- export.plot.0 +
    ## Use geom_text to add manual labels
    geom_text(data = labels.df.to.plot,
              aes(x = x, y = y,label = text, size = 2),
              nudge_x = 1,
              hjust = 0,
              size = 3)

export.plot.2 <- export.plot.1 +
    ## Tell 'fill' to use our own colours
    scale_fill_manual(values = plot.blues) +
    ## Remove some of the extra space on the y axis
    scale_y_continuous(expand = c(0,0)) +
    ## Ensure that the x axis only shows the countries we want
    scale_x_discrete(breaks = countries.to.plot)

export.plot.3 <- export.plot.2 +
    ## Customise the themes!
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


### Add the e4t logo and things --------------------------------------

## This is a function I have written that requires the 'magick'
## package
## use install.packages("magick") if you don't have it already

add.e4t.branding <- function(plot, plot.name){
    library(magick)

    plot.png <- paste0(plot.name, ".png")
    ggsave(plot.png,
           width = 21.5,
           height = 17.5,
           units = "cm",
           plot = plot)

    stats.png <- image_read(plot.png)

    logo <- image_read("e4t_logo.png")%>%
        image_resize("x150")

    twitter <- image_read("e4t_twitter.png") %>%
        image_resize("x90")

    plot.img <- image_composite(stats.png, logo,
                                offset = "+2050+0") %>%
        image_composite(twitter, offset = "+1950+1950")

    image_write(plot.img,
                path = plot.png)
    return(plot.img)
}


plot.final <- add.e4t.branding(export.plot.3,
                               "E4T_export_compliance")
