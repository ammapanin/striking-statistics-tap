## Script to followalong for introductory R TAP Tutorial
## March 2020
## apanin@worldbank.org

## This is the first script in the series
## You will do the following:
## --load export compliance data from a comma separated values file into R
## --create a bar plot of compliance time for different african countries


## We will use tools from the "tidyverse" package
## In R, packages are loaded with the function 'library'
library(tidyverse)
library(magick)

## Define the path relative to the working directory
## Read the data into an R dataframe using read.csv
export.df.path <- "datasets/export_times_clean.csv"
export.df.in <- read.csv(export.df.path)


## It is good practice to inspect data before using it
## The following functions will print output to your console

## 'dim' gives the number of rows and columns of a dataframe
## 'head' prints out the first six rows of the dataframe

dim(export.df.in)
head(export.df.in)
summary(export.df.in)

### Make the bar chart -----------------------------------------------
## The first argument of ggplot is a data frame
## --then we add the col aesthetic with to plot the number of hours
##   for each country
## --we also flip the coordinates so that the countries appear
##   horizontally

export.plot <- ggplot(data = export.df.in) +
    geom_col(aes(x = country,
                 y = time.hours)) +
    coord_flip()
