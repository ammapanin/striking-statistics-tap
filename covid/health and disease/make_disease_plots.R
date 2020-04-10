library(tidyverse)
library(countrycode)
library(cowplot)
library(colorspace)

### Define useful paths ----------------------------------------------

main.dir <- file.path(normalizePath("~"),
                      "Dropbox",
                      "Work Documents",
                      "World Bank",
                      "amma_striking_stats_local",
                      "covid")

shared.data.path <- file.path(main.dir, "shared data")
final.figures.path <- file.path(main.dir,"final figures")


###

tb.path <- file.path(shared.data.path,
                     "tuberculosis-incidence-per-100000-people.csv")

hiv.path <- file.path(shared.data.path,
                      "incidence-of-hiv-sdgs.csv")

undernourishment.path <- file.path(shared.data.path,
                                   "prevalence-of-undernourishment.csv")

age.path <- file.path(shared.data.path,
                      "population-by-broad-age-group.csv")

tb.in <- read.csv(tb.path)
hiv.in <- read.csv(hiv.path)
un.in <- read.csv(undernourishment.path)
age.in <- read.csv(age.path)


region.names <- c("East Asia & Pacific",
                  "Europe & Central Asia",
                  "Latin America & Caribbean",
                  "Middle East & North Africa",
                  "North America",
                  "South Asia",
                  "Sub-Saharan Africa")

region.codes <- c("EAS",
                  "ECS",
                  "LCN",
                  "MEA",
                  "NAC",
                  "SAS",
                  "SSF")

names(region.codes) <- region.names

tb.dt <- tb.in %>%
    filter(Year == 2016) %>%
    filter(Entity  %in% region.names) %>%
    select(region = Entity,
           value = starts_with("Incidence")) %>%
    mutate(region.code = recode(region, !!!region.codes))%>%
    merge(pop.data,
          by = "region.code") %>%
    mutate(value_pc = value / 100,
           disease = "tb") %>%
    filter(region !=  "North America") %>%
    select(region.code, region.name = region, pop2019, disease, value_pc)

hiv.dt <- hiv.in %>%
    filter(Year == 2016) %>%
    filter(Code != "") %>%
    filter(!Code %in% OWID.aggregates) %>%
    merge(wb.country.to.regions,
          by.x = "Code",
          by.y = "country.code",
          all.x = TRUE) %>%
    rename_at(vars(starts_with("X3")),
              function(x){"hiv1000"})%>%
    group_by(region.code, region.name) %>%
    summarise(hiv1000.region = sum(hiv1000)) %>%
    merge(pop.data,
          by = "region.code") %>%
    mutate(multiplier = pop2019/1000,
           hiv.abs = hiv1000.region * multiplier,
           value_pc = 100 * hiv.abs / pop2019,
           disease = "hiv")%>%
    select(region.code, region.name, pop2019, disease, value_pc)

un.dt <- un.in %>%
    filter(Year == 2016) %>%
    filter(Code != "") %>%
    filter(!Code %in% OWID.aggregates) %>%
    merge(wb.country.to.regions,
          by.x = "Code",
          by.y = "country.code",
          all.x = TRUE) %>%
    rename_at(vars(starts_with("Suite")),
              function(x){"unourish"})%>%
    group_by(region.code, region.name) %>%
    summarise(value_pc = mean(unourish, na.rm = TRUE)) %>%
    merge(pop.data,
          by = "region.code") %>%
    mutate(disease = "un")%>%
    select(region.code, region.name, pop2019, disease, value_pc)

age.cats <- c("Under.5s",
              "X5.14.years",
              "X15.24.years",
              "X25.64.years",
              "X65..years")

age.names <- c("Under 5",
               "5 to 14 years",
               "15 to 24 years",
               "25 to 64 years",
               "More than 65 years")
names(age.names) <- age.cats

age.dt <- age.in %>%
    filter(Year == 2015) %>%
    filter(Code != "") %>%
    filter(!Code %in% OWID.aggregates) %>%
    merge(wb.country.to.regions,
          by.x = "Code",
          by.y = "country.code",
          all.x = TRUE) %>%
    filter(!is.na(region.name)) %>%
    group_by(region.name, region.code) %>%
    summarise_at(4:8, list(total = function(x){sum(x)})) %>%
    ungroup() %>%
    mutate(region.total = rowSums(.[3:7])) %>%
    mutate_at(3:7,
              list(pc = ~100 *./!!as.symbol("region.total"))) %>%
    select(region.name, region.code,
           ends_with("pc")) %>%
    pivot_longer(cols = 3:7,
                 names_to = "age_group",
                 values_to = "value_pc") %>%
    mutate(age_group = gsub("_total_pc", "", age_group),
           age_group = recode(age_group, !!!age.names))


nice.blue <- "#2f8396"

age.bars <- age.dt %>%
    filter(age_group == "More than 65 years") %>%
    ggplot(aes(y = region.name, x = value_pc)) +
    scale_x_continuous(expand = c(0, 0)) +
    geom_col(width = 0.8, fill = nice.blue) +
    theme_minimal() +
    labs() +
    xlab("Percent of the population aged 65 or more")+
    theme(axis.title.y = element_blank(),
          axis.title.x = element_text(hjust = 0))


diseases.to.plot <- c("Incidence of tubercolosis",
                      "Incidence of HIV",
                      "Prevalence of undernourishment")
disease.shortnames <- c("tb", "hiv", "un")
names(diseases.to.plot) <- disease.shortnames


total.dt <- rbind(tb.dt, hiv.dt, un.dt) %>%
    mutate(disease = recode(disease, !!!diseases.to.plot),
           shortened.names = ifelse(region.code =="LCN",
                                    "LAC",
                             ifelse(region.code == "MEA",
                                    "MENA",
                                    as.character(region.name))),
           name.to.paste = ifelse(disease %in%
                                  tail(diseases.to.plot, 2),
                           ifelse(
                               region.code != "SSF",
                               "",
                               as.character(shortened.names)),
                           as.character(shortened.names)),
           label = paste(name.to.paste,
                         round(value_pc, digits = 1),
                         sep = "\n"),
           disease = factor(disease,
                            levels = diseases.to.plot,
                            ordered = TRUE))


my.cols <- rev(sequential_hcl(10, palette  = "Peach"))


disease.title <- paste(
    "Sub-Saharan Africa has many aged less than 65",
    "but biological ages are likely to be older and the",
    "region has a high\ndisease burden")

disease.caption <- paste(
    "Plot areas represent relative population",
    "sizes of different regions.",
    "\nSources: World Bank – World Development Indicators, UNAIDS",
    "UN Food and Agricultural Organization")

disease.plot <- total.dt %>%
    ggplot(aes(area = pop2019,
               fill = value_pc,
               label = label)) +
    treemapify::geom_treemap(alpha =0.9) +
    treemapify::geom_treemap_text(place="top") +
    facet_wrap(~disease) +
    scale_fill_gradient(low = my.cols[[1]],
                        high = my.cols[[10]]) +
    guides(fill = guide_colourbar(
               title =
                   "Percent of total population")) +
    guides(fill = FALSE) +
    #labs(title = disease.title,
    #     caption = disease.caption) +
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.direction = "horizontal",
          plot.caption = element_text(hjust = 0))

plots.only <- plot_grid(age.bars, disease.plot, rel_widths = c(2, 3))

title <- ggdraw() +
  draw_label(
    disease.title,
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

final.image <- plot_grid(title, plots.only, ncol = 1,
                         rel_heights = c(1, 8))

ggsave(filename = file.path(final.figures.path,
                            "high_disease_burden.png"),
       plot = final.image,
       width = 29,
       height = 9,
       units = "cm")


