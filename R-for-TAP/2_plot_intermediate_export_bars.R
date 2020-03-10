## Script to followalong for introductory R TAP Tutorial
## March 2020
## apanin@worldbank.org

## This is script 2/4 in the series. We will do the following:
## Make a more customized chart, including data customization
## -- sort countries according to time taken
## -- add labels
## -- add colours to show landlocked status

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

###  Prepare the data frame for plotting -----------------------------

export.df.to.plot.base <- export.df.in %>%
    ## arrange the data frame terms of hours of compliance
    arrange(time.hours)

## This will not be enough! The actual variable of country names
## has to be an ordered factor in order for the plot to be arranged
## To see this, try to replace 'export.df.to.plot' below with
#   export.df.to.plot.base


## First, create a list of countries ordered by the time in hours
ordered.countries <- export.df.to.plot.base %>%
    arrange(time.hours)  %>%
    select(country) %>%
    pluck(1) %>%
    as.character() %>%
    unique()

## Then use this list as the levels of a factor made out of the country
## column
export.df.to.plot <- export.df.to.plot.base %>%
    mutate(country = factor(country,
                            levels =  ordered.countries,
                            ordered = TRUE))

### Define some labels for the chart----------------------------------

axis.title <- paste("Hours needed to complete documentary",
                    "compliance for export (2016)")

chart.title <- paste("Is Africa ready for AfCTA?")
chart.subtitle <-  paste(
    "Documentary compliance for exports",
    "varies widely",
    "— it is not a problem of being landlocked",
    "or poor.")

##  '\n' means it will start on a new line
chart.notes <- paste(
    "Data from World Development Indicators.",
    "\nSeries name and code:",
    "Time to export, documentary compliance (hours),",
    "IC.EXP.TMDC")

### Make the actual plot ----------------------------------------------
## It is easy to build on a ggplot by adding other layers to it
## We will make the final plot in three stages

export.plot.0 <- ggplot(export.df.to.plot) +
    geom_col(aes(x = country,
                 y = time.hours,
                 ## fill tells ggplot which variable in the dataset
                 ##  to assign to the colours
                 fill = is.landlocked),  width = 0.8) +
    coord_flip()

## enter this in the console to see your plot
export.plot.0


export.plot.1 <- export.plot.0 +
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

## enter this in the console to see your plot
export.plot.1


export.plot.2 <- export.plot.1 +
    ## theme allows us to customize almost all aspects of a ggplot
    ## here we do the following
    ## --remove the name of the y axis (now flipped)
    ## --change the size of the country labels
    ## --take away horizontal grid lines
    ## --make the legend horizontal and remove the legend title
    theme(axis.title.y = element_blank(),
          axis.text.y = element_text(size = 6),
          axis.title.x = element_text(
              margin = margin(t = 0.2, unit = "cm")),
          panel.grid.major.y = element_blank(),
          legend.direction = "horizontal",
          legend.position =  c(0.6, 0.7),
          legend.title = element_blank())


## enter this in the console to see your plot
export.plot.2
