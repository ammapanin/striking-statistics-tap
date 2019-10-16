### Quick look at EconLit data
### -- titles and abstracts of econ papers
### Last updated: 26 September 2019

library(XML)
library(tidyverse)
library(stringr)
library(maptools) # to get a simple world map
library(countrycode)

###  Read in data ----------------------------------------------------

## Slow process to read in and translate xml, comment out to read
## Data already translated into a dataframe, so may not be
## necessary to load the raw xml

get_useful_info <- function(xml.in){
    ## EconLit exports data in xml form
    ## Go through the stored data and return a dataframe
    ## containing the title, abstract and a title + abstract
    ## string called 'both'
    ## (Maybe also dates?)

    xml.useful <- xml.in$header$controlInfo
    artinfo <- xml.useful$artinfo

    abstract <- artinfo$ab
    title <- artinfo$tig$atl

    journal <- xml.useful$jinfo$jtl

    date_text <- xml.useful$pubinfo$dt$text
    date <- as.Date(date_text, format = "%Y%m%d")

    year <- format(date, "%Y")
    month <- format(date, "%m")
    day <- format(date, "%d")

    topics <- artinfo[names(artinfo) == "su"] %>%
        str_trim(side = "both") %>%
        paste(collapse ="XXX")

    abstract <- ifelse(is.null(abstract),"", abstract)
    both <- paste(title, abstract)

    dt.out <- data.frame(journal,
                         title = title,
                         abstract = abstract,
                         both,
                         year,
                         month,
                         day,
                         topics,
                         stringsAsFactors = FALSE)

    return(dt.out)
}

open_xml_to_dataframe <- function(xml.path){
    records <- xmlParse(xml.path)
    records_data <- xmlToList(records)

    ## Extract useful information from xml and make a
    ## data frame
    extracted_df <- plyr::ldply(records_data,
                                get_useful_info,
                                .progress = "text")
    return(extracted_df)
}


### Process XML files ------------------------------------------------

### Journal classifications
journal.recodes <- c(
    "World Development" = "dev",
    "Journal of Development Studies" = "dev",
    "Journal of Development Economics" = "dev",
    "World Bank Research Observer" =  "dev",
    "Development Policy Review" = "dev",
    "World Bank Economic Review" = "dev",
    "Economic Development & Cultural Change" = "dev",
    "Journal of African Economies" = "dev",
    "Journal of Development Effectiveness" = "dev",
    "IZA Journal of Development and Migration"="dev",
    "European Economic Review" = "general",
    "Journal of the European Economic Association" = "general",
    "Review of Economics & Statistics" = "general",
    "Economic Journal" = "general",
    "American Economic Review" = "top5",
    "American Economic Journal: Economic Policy"= "general",
    "American Economic Journal: Microeconomics"= "general",
    "American Economic Journal: Macroeconomics"= "general",
    "American Economic Journal: Applied Economics"= "general",
    "American Economic Review: Insights"= "general",
    "Journal of Political Economy" = "top5",
    "Review of Economic Studies"  = "top5",
    "Quarterly Journal of Economics" = "top5",
    "Econometrica" = "top5",
    "Journal of Economics & Management Strategy" = "extra")

short.category.names <- c("dev", "general", "top5")
long.category.names <- c("Development journals",
                        paste("Other general interest",
                              "economics journals"),
                        "Top 5 economics journals")
names(long.category.names) <- short.category.names


### Count country mentions in abstracts and titles -------------------
## Get list of countries and regions
## Use helpful github page https://github.com/mledoze/countries

country.codes <- read.csv("data/countries.csv",
                          stringsAsFactors = FALSE) %>%
    mutate(names_list = paste(name, demonym, sep = ","),
           names_list = gsub(",,",  ",",  names_list))

wb.codes <- read.csv("data/amma_country_codes.csv",
                     stringsAsFactors = FALSE) %>%
    select(code, region)

africa.region.codes <- filter(wb.codes,
                              region == "Sub-Saharan Africa") %>%
    pull(code)

all.countries <- country.codes %>%
    select(cca3, names_list) %>%
    rbind(data.frame("Africa,African", "AFR") %>%
          rename(names_list = 1, cca3 = 2))

is_country_in_abstract <- function(names_list, abstract.in){
    ## Wrapper function to check if a set of countries in
    ## 'country.list' occur in 'txt.to.check'

    name_variants <- names_list %>%
        strsplit(",") %>%
        pluck(1) %>%
        unique() %>%
        tolower()

    in.country.true <- sapply(
        name_variants,
        function(name_variant){
            grepl(paste0("\\b", name_variant, "\\b"),
                  tolower(abstract.in))
        })
    return(sum(in.country.true) > 0)
}

check_countries_in_abstract <- function(abstract.in){
    country.checked.list <- sapply(all.countries$names_list,
                                   is_country_in_abstract,
                                   abstract.in = abstract.in)
    names(country.checked.list) <- all.countries$cca3
    return(which(country.checked.list == TRUE))
}

check_abstract_list <- function(abstracts.in){
    abstract.mentions <- plyr::llply(
                                   abstracts.in,
                                   check_countries_in_abstract,
                                   .progress = "text")
    return(data.frame(I(abstract.mentions)))
}

count_mentions_of_country <- function(country.list.in){
    ## Count how many times any of the countries in
    ## 'country.list.in' occur in the  txt.list.in
    tab <- table(unlist(country.list.in))
    names(tab) <- all.countries[names(tab), "cca3"]

    tab.df <- data.frame(tab) %>%
        rename("country" = "Var1",
               "frequency" = "Freq")

    return(tab.df)
}

### Only run once to produce data frame!------------------------------

## xml.list <- list.files("data",
##                        pattern = "xml",
##                        full.names =  TRUE)

## journal.data.dfs <- plyr::ldply(xml.list,
##                                 open_xml_to_dataframe,
##                                 .progress = "text") %>%
##    mutate(journal_cat = recode(journal, !!!journal.recodes))

## total.counts <- check_abstract_list(journal.data.dfs$both)

## journal.data.dfs.with.counts <- journal.data.dfs

## journal.data.dfs.with.counts <- data.frame(
##     journal.data.dfs,
##     country_mention_counts = I(total.counts[[1]]))

## saveRDS(total.counts, file = "data/total_counts.Rds")
## saveRDS(journal.data.dfs.with.counts,
##         file = "data/journals_with_counts.Rds")

## Read-in general data ----------------------------------------------
dt <- readRDS("data/journals_with_counts.Rds")%>%
    filter(journal_cat  !=  "extra")

## Create frame with world map and paper counts  ---------------------
world.counts.df <- map_data("world") %>%
    mutate(country = countrycode(region,
                                 origin = "country.name",
                                 destination =  "iso3c"))

count.breaks <- c(0, 5, 10, 50, 100, 500, 1000)

merge.with.map <- function(df.in,
                           suggested.breaks = count.breaks){

    df.base <- world.counts.df %>%
        merge(df.in,
              by = "country",
              all.x = TRUE) %>%
        filter(region != "Antarctica") %>%
        mutate(paper.breaks =
                   cut(frequency,
                       breaks = suggested.breaks,
                       include.lowest = FALSE)) %>%
        arrange(order)

    breaks.ch <- df.base %>%
        pull(paper.breaks) %>%
        levels()

    if(length(suggested.breaks) > 1){
        n.end.suggested <- length(suggested.breaks)
        n.end.breaks <- length(breaks.ch)

        end.bracket <- breaks.ch[[n.end.breaks]]
        plus.text <- paste("more than",
                           suggested.breaks[(n.end.suggested-1)])

        recode.breaks <- c(plus.text)
        names(recode.breaks) <- end.bracket

        print(end.bracket)

        breaks.ch[n.end.breaks] <- plus.text
        breaks.levels <- append(breaks.ch, "0", after = 0)

        print(breaks.levels)
        print(recode.breaks)

        df.out <- df.base %>%
            mutate(
                paper.breaks.ch = as.character(paper.breaks),
                paper.breaks.ch = recode(paper.breaks.ch,
                                         !!!recode.breaks),
                paper.breaks.ch = ifelse(is.na(paper.breaks.ch),
                                         0,
                                         paper.breaks.ch),
                paper.count = factor(paper.breaks.ch,
                                     levels = breaks.levels,
                                     ordered = TRUE)) %>%
            arrange(order)
    }

    return(df.out)
}

### Break up data according to journal classification ----------------
merge.and.add.journal <- function(ix, dfs.list, names.list){
    df.to.edit <- dfs.list[[ix]]
    category <- names.list[[ix]]
    df.out <- df.to.edit %>%
        merge.with.map() %>%
        mutate(journal_cat = category)

    return(df.out)
}

counts_by_journal_category.list <-
    plyr::dlply(dt,
                .variables = "journal_cat",
                .fun = function(xdf){
                    vout <- count_mentions_of_country(
                        xdf[["country_mention_counts"]])
                })

journal_category_names <- attr(counts_by_journal_category.list,
                               "split_labels")[[1]]

counts_by_journal_category.modified_list <-
    plyr::llply(seq_along(counts_by_journal_category.list),
                .fun = merge.and.add.journal,
                dfs.list = counts_by_journal_category.list,
                names.list = journal_category_names)

counts_by_journal_category.df <- do.call(
    rbind,
    counts_by_journal_category.modified_list) %>%
    mutate(journal_cat =
               recode(journal_cat, !!!long.category.names),
           journal_cat = factor(journal_cat,
                                levels = rev(long.category.names),
                                ordered = TRUE))

### Break up data according to year ----------------------------------

counts_by_year.df <-
    plyr::dlply(dt,
                .variables = c("journal_cat", "year"),
                .fun = function(xdf){
                    vout <- count_mentions_of_country(
                        xdf[["country_mention_counts"]])
                }) %>%
    plyr::ldply(identity) %>%
    merge(wb.codes,
          by.x = "country",
          by.y = "code",
          all.x = TRUE)


### Plot counts per country ------------------------------------------
### !!!Perhaps later change this to use haromized data frame
colour_function <- colorRampPalette(c("#E2E2E2", "#005a32"))
count.palette <- count.breaks
count.palette[[1]] <- 1
scaled.count.palette <- round(2 ^ log(count.palette, base = 10))
all.colours <- colour_function(max(scaled.count.palette))

green.colour.scale <- all.colours[scaled.count.palette]


chart_subtitle <- expression(
    paste("Presumably, the continent is of ",
          "interest to",
          italic(" some "),
          "economists. ",
          "We need their voices."))

chart_caption <- expression(
    paste(atop(
        paste(italic("Note "),
              "Abstracts downloaded from EconLit for all articles ",
              "published in the top 5, ",
              "other general interest, and "),
        paste("development ",
              "field journals between ",
              "1990 to 2019. Code, data and list of journals ",
              "at github.com/ammapanin"))))

legend.title <-  paste("Number of abstracts or titles\n",
                       "containing country name")

count.plot <- counts_by_journal_category.df %>%
    ggplot(aes(x = long,
               y = lat, group = group,
               fill = paper.count)) +
    geom_polygon() +
    scale_fill_manual(
        values = green.colour.scale,
        guide = guide_legend(
            title = legend.title,
            title.position = "top",
            direction = "vertical",
                                        #nrow = 2,
                                        #byrow = TRUE,
            override.aes = list(size = 1, shape = 3),
            keywidth = unit(.4, "cm"),
            keyheight = unit(.4, "cm"))) +
    labs(caption = chart_caption,
         title = paste("Africa is not of general interest",
                       "to the economics profession"),
         subtitle = chart_subtitle) +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.title =  element_blank(),
          legend.title = element_text(size = 6),
          legend.text = element_text(size = 6),
          legend.margin = margin(t = 0.8, b = 0.5, unit='cm'),
          legend.position = "right",
          plot.caption = element_text(size = 7,
                                      hjust = 0),
          plot.title = element_text(hjust = 0,
                                    face = "bold"),
          plot.subtitle = element_text(hjust = 0, size = 9)) +
    facet_wrap(~journal_cat, nrow = 3)

count.plot


ggsave("country_count_by_outlet.png",
       width =13.7, height = 16.6, units = "cm")



### Plot counts per year per region ----------------------------------

dt.year <- counts_by_year.df %>%
    group_by(year, journal_cat) %>%
    mutate(year_category_total = sum(frequency)) %>%
    group_by(region, year, journal_cat, year_category_total) %>%
    summarise(reg.year.tot = sum(frequency)) %>%
    mutate(cat_freq = reg.year.tot / year_category_total)


africa.dt.year <- dt.year %>%
    filter(region == "Sub-Saharan Africa")

short.category.names <- c("dev", "general", "top5")
journal.year.names <- c("development",
                        "other general interest",
                        "top 5")%>%
    setNames(short.category.names)


plot.dt <- africa.dt.year %>%
    ungroup() %>%
    mutate(year = factor(year,
                         levels = seq(1990, 2025)))


journal.labs.df <- data.frame(africa.dt.year %>%
                              filter(year == 2019)%>%
                              select(cat_freq, journal_cat)) %>%
    mutate(journal_lab = recode(journal_cat, !!!journal.year.names))

year.plot <- plot.dt %>%
    ggplot(aes(x = year,
               y = cat_freq,
               colour = journal_cat,
               group = journal_cat)) +
    scale_x_discrete(breaks = seq(1990, 2029, by = 5),
                     drop = FALSE) +
    scale_colour_brewer(type = "qual", palette ="Set1") +
    geom_line() +
    geom_point() +
    labs(title = paste("Has the proportion of economics",
                       "journal abstracts",
                       "mentioning Africa changed over the years?"),
         x = element_blank(),
         y = "Proportion of abstracts in journal category")+
    geom_text(data =  journal.labs.df,
              aes(x = year, y=cat_freq, label = journal_lab),
              hjust = 0,
              nudge_x = 0.5) +
    theme_minimal() +
    theme(legend.position = "none",
          plot.title =  element_text(face = "bold"))

year.plot
