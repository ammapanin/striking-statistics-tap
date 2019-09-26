### Quick look at EconLit data
### -- titles and abstracts of econ papers
### Last updated: 26 September 2019


library(XML)
library(maptools) # to get a simple world map

###  Read in data ----------------------------------------------------

## Slow process to read in and translate xml, comment out to read
## Data already translated into a dataframe, so may not be
## necessary to load the raw xml

#records <- xmlParse("data/top5_plus_aej_1990_to_2019.xml")
#records_data <- xmlToList(records)

## Extract useful information from xml and make a
## data frame
## Comment out the following code to avoid creating the data frame
## every time

# abstract.dt.list <- lapply(records_data, get_useful_info)
# abstract.dt <- do.call(rbind, abstract.dt.list)

## write.csv(x = abstract.dt,
##           file = "data/journal_abstracts.csv",
##           row.names = FALSE)

abstracts.dt <- read.csv("data/journal_abstracts.csv")

## Get list of countries and regions
## (List is Amma's csv containing country names and codes
##  across datasets, use the World Bank names as a first pass)

country.codes <- read.csv("data/country_codes.csv",
                          stringsAsFactors = FALSE)

all.countries <- country.codes %>%
    pull(wb) %>%
    as.character()

africa.countries <- country.codes %>%
    filter(region == "Sub-Saharan Africa") %>%
    pull(wb) %>%
    as.character()

get_useful_info <- function(xml.in){
    ## EconLit exports data in xml form
    ## Go through the stored data and return a dataframe
    ## containing the title, abstract and a title + abstract
    ## string called 'both'

    xml.useful <- xml.in[[1]]
    abstract <- xml.useful$controlInfo$artinfo$ab
    title <- xml.useful$controlInfo$artinfo$tig$atl

    title <- ifelse(is.null(title), "", title)
    abstract <- ifelse(is.null(abstract),"", abstract)
    both = paste(title, abstract)

    dt.out <- data.frame(title = title,
                         abstract =  abstract,
                         both,
                         stringsAsFactors = FALSE)

    return(dt.out)
}

check_country_mention <- function(txt.to.check,
                                  country.list){
    ## Wrapper function to check if a set of countries in
    ## 'country.list' occur in 'txt.to.check'

    in.country.true <- sapply(country.list,
           function(x){
               grepl(paste0("\\b", tolower(x), "\\b"),
                     tolower(txt.to.check))
           })
    return(sum(in.country.true) > 0)
}

count_mentions <- function(country.list.in, txt.list.in){
    ## Count how many times any of the countries in
    ## 'country.list.in' occur in the  txt.list.in

    count.list <- sapply(txt.list.in,
                         check_country_mention,
                         country.list = country.list.in,
                         USE.NAMES = FALSE)
    return(count.list)
}

## Count articles by country, first in list, then make a dataframe
## n.by.country <- sapply(all, function(x){
##     tf.list <- count_mentions(c(x),  abstracts.dt$both)
##     n <- sum(tf.list)
##     return(n)
## })

## n.country.dt <- data.frame(country = names(n.by.country),
##                             n = n.by.country) %>%
##      mutate(country = recode(country,
##                              "United States" = "USA"))

## write.csv(x = n.country.dt,
##           file = "n_articles_by_country.csv",
##           row.names = FALSE)

n.country.dt <- read.csv("n_articles_by_country.csv")


## Create frame with world map and paper counts
world.counts.df <- map_data("world") %>%
    merge(n.country.dt,
          by.x = "region",
          by.y = "country",
          all.x = TRUE) %>%
    filter(region != "Antarctica") %>%
    mutate(paper.breaks =
               cut(n,
                   breaks = c(0, 1, 5, 10, 50, 100, 500),
                   include.lowest = TRUE),
           paper.breaks.ch = as.character(paper.breaks),
           paper.count = factor(
               ifelse(is.na(paper.breaks.ch),
                      "missing",
                      paper.breaks.ch),
               levels = c(levels(paper.breaks), "missing"),
               ordered = TRUE)) %>%
    arrange(order)

### Plot counts per country ------------------------------------------

green.colour.scale <- c('#c7e9c0',
                        '#a1d99b',
                        '#74c476',
                        '#41ab5d',
                        '#238b45',
                        '#005a32',
                        "grey")

legend.title <-  paste("Number of abstracts or titles",
                       "containing country name")

count.plot <- world.counts.df %>%
    ggplot(aes(x = long,
               y = lat, group = group,
               fill = paper.count)) +
    geom_polygon() +
    scale_fill_manual(
        values = green.colour.scale,
        guide = guide_legend(title =legend.title,
                             direction = "horizontal",
                             nrow = 1)) +
    labs(caption = paste("Abstracts taken from Top 5 journals",
                         " and AEJ's, 1990 to 2019")) +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.title =  element_blank(),
          legend.position = "top")


ggsave("quick_country_count.pdf")

