## Script to followalong for introductory R TAP Tutorial
## March 2020
## apanin@worldbank.org

## This is the first script in the series
## We will do the following:
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







## Create a list of countries ordered by the time in hours
ordered.countries.list <- export.use %>%
    arrange(time_hours)  %>%
    select(country) %>%
    pluck(1) %>%
    as.character() %>%
    unique()

## We will plot some 'fake countries' called LAB 1 and LAB 2
## This is a way of leaving space on the plot for the labels
ordered.countries <-


### Create the data frame that will be used for plotting -------------




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


