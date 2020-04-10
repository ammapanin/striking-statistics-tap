
library(tidyverse)
library(countrycode)

### Define useful paths ----------------------------------------------

main.dir <- file.path(normalizePath("~"),
                      "Dropbox",
                      "Work Documents",
                      "World Bank",
                      "amma_striking_stats_local",
                      "covid")

shared.data.path <- file.path(main.dir, "shared data")
final.figures.path <- file.path(main.dir,"final figures")

beds.path <- file.path(shared.data.path, "physicians_and_beds.csv")


doctors.str <- "Physicians (per 1,000 people)"
beds.str <- "Hospital beds (per 1,000 people)"

beds.in <- read.csv(beds.path)

beds.long <- beds.in %>%
    rename_at(.vars = vars(starts_with("X")),
              function(x){
                  paste0("y", str_sub(x, start = 2, end = 5))}) %>%
    mutate_at(.vars = vars(starts_with("y")),
              function(x){
                  as.numeric(as.character(x))}) %>%
    pivot_longer(cols = starts_with("y"),
                 names_to = "year",
                 names_prefix = "y") %>%
    mutate(series = ifelse(Series.Name == doctors.str,
                           "doctors",
                    ifelse(Series.Name == beds.str,
                           "beds",
                           "the end of the world as we know it"))) %>%
    select(country = Country.Name,
           ccode = Country.Code,
           series,
           year,
           value)

beds <- beds.long %>%
    group_by(country, series) %>%
    group_modify(.f = ~{
        row.ix <- max(which(!is.na(.x$value)))
        slice(.x, row.ix)
    }) %>%
    select(-year) %>%
    pivot_wider(names_from= series, values_from = value) %>%
    mutate(doc.per.bed = doctors / beds) %>%
    pivot_longer(cols = one_of(c("beds", "doctors", "doc.per.bed")),
                 names_to = "series")

comparison.countries <- c("USA", "CHN", "ITA")
ssa.codes <- wb.countries %>%
    filter(region == "Sub-Saharan Africa") %>%
    pull(code) %>%
    as.character()

### Is Gabon an outlier?
ccode.with.suspicious.data <- c("GAB")

beds.only.order <- beds %>%
    filter(series == "beds") %>%
    arrange(value) %>%
    pull(ccode) %>%
    as.character()

beds.only.order <- beds %>%
    filter(series == "beds") %>%
    arrange(value) %>%
    pull(country) %>%
    as.character()


series.names <- c("Beds per 1000",
                  "Physicians per 1000")
names(series.names) <- c("beds","doctors")


beds.plot.df <- beds %>%
    ungroup() %>%
    filter(ccode %in% c(comparison.countries, ssa.codes)) %>%
    filter(!ccode %in% ccode.with.suspicious.data) %>%
    mutate(country = factor(country,
                          levels = beds.only.order,
                          ordered = TRUE),
           series = recode(series, !!!series.names),
           african = ccode %in% ssa.codes)


beds.title <- paste("Solutions that rely on scaled",
                    "capacity face three problems",
                    "i) not just beds, specialised beds",
                    "ii) scale up healthcare workers")


nice.blue <- "#2f8396"
plot.cols <- c("gray75", nice.blue)

beds.plot <- beds.plot.df %>%
    filter(series != "doc.per.bed") %>%
    ggplot(aes(y = country,
               x = value,
               fill = african)) +
    scale_fill_manual(values = plot.cols) +
    geom_col(width = 0.7) +
    scale_x_continuous(expand = c(0,0)) +
    facet_wrap(~series, ncol = 2) +
    theme_minimal() +
    labs(title = beds.title) +
    guides(fill = FALSE)+
    theme(axis.text.y = element_text(size = 5,
                                     colour = "black"),
          axis.title = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank())
