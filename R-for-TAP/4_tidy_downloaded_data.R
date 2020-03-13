## Script to followalong for introductory R TAP Tutorial
## March 2020
## apanin@worldbank.org

## We will do the following in this scrip:
## --load the data as it is downloaded from the WDI
## --wrangle it into a form that we can use for easy analysis


## We will use tools from the "tidyverse" package
## In R, packages are loaded with the function 'library'
library(tidyverse)

## Define the paths relative to the working directory
## Read the data into an R dataframe using read.csv
export.dt.path <- "datasets/export_documentary_compliance.csv"
landlocked.dt.path <- "datasets/landlocked_countries.csv"

## It is good practice to read ina version of the dataset that
##  is not going to be written over in the script
##  I ususally put the suffix 'dt.in'
export.dt.in <- read.csv(export.dt.path)
landlocked.dt.in <- read.csv(landlocked.dt.path)

## Inspect the data
dim(export.dt.in)
head(export.dt.in)

dim(landlocked.dt.in)
head(landlocked.dt.in)


### Get readable names for the downloaded data frame -----------------

## Rename the columns of the export data into something meaningful
## First print the names to the console
names(export.dt.in)

## We will want to change the names of the yearly data
## Year  names are stored in columns 5 to 24
## Create a new data frame (rather than modifying export.dt.in)
export.renamed <- export.dt.in

## Rather than hardcoding the number `24` (what if we went back
##  and downloaded more data?), store the number of columns
##  as a new variable
ncol.export <- ncol(export.renamed)

## Save the old year names as a character vector
year.names <- names(export.renamed)[5:ncol.export]

## Make the new year names by taking the characters in the
##  positions of 2-5 of each column name
new.year.names <- paste0("y", substr(year.names, 2, 5))

## Rename the columns of the data frame using the new names
names(export.renamed)[5:ncol.export] <- new.year.names


### Prepare the dataset on landlocked countries ----------------------
## This will be merged with the export data later
landlocked.countries <- landlocked.dt.in %>%
    filter(Indicator == "Landlocked") %>%
    select(ccode = Country.ISO3,
           is.landlocked = X2017)

### Prepare the final dataset for analysis ---------------------------

## These character strings willbe used later. Keep them in mind!
## Define the text names of some groups of countries
ssa.region.lab <- "Sub-Saharan African countries"
comparator.region.lab <- "Comparator regions"
ida.region.lab <- "IDA & IBRD countries only"

## This will be used later
plot.order <- c(ssa.region.lab,
                comparator.region.lab,
                ida.region.lab)

## Define acharacter vector that lists regions
regions <- c("Sub-Saharan Africa",
             "East Asia & Pacific",
             "Latin America & Caribbean",
             "European Union",
             "Euro area")

export.use.simple <- export.renamed %>%
    ## Select only a few columns from the export data, and rename them
    select(country = Country.Name,
           ccode = Country.Code,
           time.hours = y2016) %>%
    ## Remove empty rows in the data
    filter(ccode != "") %>%
    ## Merge the export data with the landlocked data
    merge(landlocked.countries,
          by.x = "ccode",
          by.y = "ccode",
          all.x = TRUE) %>%
    ## Create some new columns and change the type of some existing
    ##  ones
    mutate(time.hours = as.numeric(as.character(time.hours)),
           ## The regions do not have is.landlocked variables,
           ## --therefore replace their is.landlocked value with 0
           ## --and then change is.landlocked to text
           is.landlocked = ifelse(is.na(is.landlocked), 0,
                                  is.landlocked),
           is.landlocked = ifelse(is.landlocked == 0,
                                  "Has sea port",
                                  "Landlocked"),
           ## Create a TRUE/FALSE column for if the row represents
           ##  a region rather than a country
           comparator.region = country %in% regions,
           ## Create a TRUE/FALSE column for if the country name
           ##  in a row contains the phrase'IDA&IBRD'
           ida.region = grepl("IDA & IBRD", country),
           ## Create a character vector listing the group of data
           plot.group.char = ifelse(ida.region == TRUE,
                                    ida.region.lab,
                             ifelse(comparator.region == TRUE,
                                    comparator.region.lab,
                                    ssa.region.lab)),
           ## Turn the character vector into a factor
           plot.group = factor(plot.group.char,
                               levels = plot.order,
                               ordered = TRUE)) %>%
    ## Putting '-' in front of columns means that we
    ##  drop these columns. Good, since we will no longer use them
    select(-c(plot.group.char, ida.region, comparator.region)) %>%
    filter(!is.na(time.hours)) %>%
    filter(ccode != "Euro area")

export.use.out.path <- "datasets/export_times_clean.csv"
write.csv(x = export.use.simple,
          file =  export.use.out.path,
          row.names = FALSE)

### Create a panel version of the exports dataset for the animation
## added on 13 March 2020

years.14.to.19 <- paste("y", seq(2014, 2019), sep = "")

export.panel.df <- export.renamed %>%
    select(country = Country.Name,
           ccode = Country.Code,
           one_of(years.14.to.19)) %>%
    ## Remove empty rows in the data
    filter(ccode != "") %>%
    ## Merge the export data with the landlocked data
    merge(landlocked.countries,
          by.x = "ccode",
          by.y = "ccode",
          all.x = TRUE) %>%
    ## Create some new columns and change the type of some existing
    ##  ones
    mutate(## The regions do not have is.landlocked variables,
           ## --therefore replace their is.landlocked value with 0
           ## --and then change is.landlocked to text
           is.landlocked = ifelse(is.na(is.landlocked), 0,
                                  is.landlocked),
           is.landlocked = ifelse(is.landlocked == 0,
                                  "Has sea port",
                                  "Landlocked"),
           ## Create a TRUE/FALSE column for if the row represents
           ##  a region rather than a country
           comparator.region = country %in% regions,
           ## Create a TRUE/FALSE column for if the country name
           ##  in a row contains the phrase'IDA&IBRD'
           ida.region = grepl("IDA & IBRD", country),
           ## Create a character vector listing the group of data
           plot.group.char = ifelse(ida.region == TRUE,
                                    ida.region.lab,
                             ifelse(comparator.region == TRUE,
                                    comparator.region.lab,
                                    ssa.region.lab)),
           ## Turn the character vector into a factor
           plot.group = factor(plot.group.char,
                               levels = plot.order,
                               ordered = TRUE)) %>%
    ## Putting '-' in front of columns means that we
    ##  drop these columns. Good, since we will no longer use them
    select(-c(plot.group.char, ida.region, comparator.region)) %>%
    filter(ccode != "Euro area") %>%
    pivot_longer(cols = starts_with("y2"),
                 names_to = "year",
                 names_prefix = "y",
                 values_to = "time.hours")%>%
    mutate(time.hours = as.numeric(as.character(time.hours)))

export.panel.out.path <- "datasets/export_times_panel.csv"
write.csv(x = export.panel.df,
          file =  export.panel.out.path,
          row.names = FALSE)


