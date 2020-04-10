## Code to reproduce estimates of COVID deaths

## Imperial College, Report 12, The Global Impact of COVID-19 and Strategies for Mitigation and Suppression March 26, 2020

## Recreate Table 1

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
final.figures.path <- file.path(main.dir, "final figures")


### Define some functions to transform data --------------------------
to.billions <- function(x){x * 10 ^ -9}
to.millions <- function(x){x * 10 ^ -6}
thousands.to.absolute <- function(x){x * 10 ^ 3}

pretty.labels <- function(xstr, pretty.levels){
    xval <- recode(xstr, !!!pretty.levels)
    xval.out <- factor(xval,
                       levels = pretty.levels,
                       ordered = TRUE)
    return(xval.out)
}

### Get population data ----------------------------------------------

population.data.path <- file.path(shared.data.path,
                                  "regional_population_data.csv")

pop.data.in <- read.csv(population.data.path)

pop.data <- pop.data.in %>%
    select(region.code = Country.Code,
           pop2019 = X2018..YR2018.) %>%
    filter(region.code != "")

wb.region.codes.from.pop <- pop.data.in %>%
    select(region.name = Country.Name,
           region.code = Country.Code)

### Get deaths data --------------------------------------------------

deaths.path <- file.path(shared.data.path,
                         "annual-number-of-deaths-by-world-region.csv")
deaths.in <- read.csv(deaths.path)

wb.country.in <- file.path(shared.data.path,
                           "wb_country_codes.csv")

wb.countries <- read.csv(wb.country.in)

wb.country.to.regions <- wb.countries %>%
    select(country.code = code,
           region.name = region) %>%
    merge(wb.region.codes.from.pop,
          by = "region.name",
          all.x = TRUE) %>%
    unique()

get.wb.region <- function(ccode.in){
    filter(wb.country.to.regions,
           country.code == ccode.in) %>%
        pull(region.code)
}

OWID.aggregates <- c('OWID_CIS', 'OWID_MNS', 'OWID_PYA', 'OWID_WRL')

## Get deaths in 2018
deaths.df.with.regions <- deaths.in %>%
    filter(Code != "") %>%
    filter(!(Code %in% OWID.aggregates)) %>%
    merge(wb.country.to.regions,
          by.x = "Code",
          by.y = "country.code",
          all.x = TRUE)

## TODO! Fix missing countries
deaths.df.with.regions %>%
    filter(is.na(region.code)) %>%
    select(Entity, Code) %>%
    unique()

deaths <- deaths.df.with.regions %>%
    rename(ccode = Code,
           country = Entity,
           year = Year,
           deaths.thousands = 4) %>%
    select(-c(region.name, country)) %>%
    filter(year == 2015) %>%
    group_by(region.code) %>%
    summarize(deaths2015 = sum(deaths.thousands))
    ## Remove this once individual countries coded!
    ### filter(!is.na(region.code)) ##%>%
    ## Double check that OWD has the correct magnitudes
    ### mutate(deaths2015 = thousands.to.absolute(deaths.thousands))

### Input data (by hand) ---------------------------------------------
imperial.dt <- list(
    EAS = c(2117131000, 15303000, 92544000,442000,632619000,3315000),
    ECS = c(801770000,7276000,61578000,279000,257706000,1397000),
    LAC = c(566993000,3194000,45346000, 158000, 186595000,729000),
    MEA = c(419138000,1700000,30459000,113000,152262000,594000),
    NAC = c(326079000,2981000,17730000,92000,90529000,520000),
    SAS = c(1737766000,7687000,111703000,475000,629164000,2693000),
    SSF = c(1044858000,2483000,110164000,298000,454968000,1204000))

outcome.order <- c("infections", "deaths")
pretty.outcomes <- c("Infections, % of population",
                     "Deaths, % of population")
names(pretty.outcomes) <- outcome.order

scenario.order <- c("unmitigated", "soft", "hard", "dummy")

pretty.scenarios <- c("Unmitigated",
                      "Moderate suppression",
                      "Aggressive suppression",
                      "dummy")

names(pretty.scenarios) <- scenario.order

region.names <- c(EAS = "East Asia\n& Pacific",
                  ECS = "Europe\n& Central Asia",
                  LAC = "Latin America\n& Caribbean",
                  MEA = "Middle East\n& North Africa",
                  NAC = "North America",
                  SAS = "South Asia",
                  SSF = "Sub-Saharan Africa")

imperial.df.base <- do.call(
    rbind, imperial.dt) %>%
    data.frame()

imperial.names <- paste(
    rep(c("unmitigated",
          "hard", "soft"), each =  2),
    c("infections", "deaths"),
    sep = "_")

names(imperial.df.base) <- imperial.names

imperial.df <- imperial.df.base %>%
    mutate(region = recode(rownames(.), !!!region.names),
           region.code = rownames(.)) %>%
    merge(pop.data, by = "region.code") %>%
    merge(deaths, by = "region.code")%>%
    pivot_longer(cols = c(2:7),
                 names_to = c("scenario", "outcome"),
                 names_pattern = "(.*)_(.*)") %>%
    mutate(value_absolute = as.numeric(value),
           value_scaled = ifelse(
               outcome == "infections",
               to.billions(value),
                          ifelse(outcome == "deaths",
                                 to.millions(value),
                                 "mistakes have been made")),
           value_scaled = as.numeric(value_scaled),
           value_pc = 100 * value_absolute / pop2019,
           pc_deaths = 100 * deaths2015/pop2019,
           outcome = pretty.labels(outcome, pretty.outcomes),
           scenario = pretty.labels(scenario,  pretty.scenarios))

## Make a copy of the data with a dummy scenario
deaths2015.scenario <- "Unmitigated"

deaths.only.base <- imperial.df %>%
    select(region, scenario, pc_deaths) %>%
    filter(scenario == "Unmitigated") %>%
    unique() %>%
    mutate(scenario = deaths2015.scenario)
#           value_pc = 0)

deaths.only.to.add <- deaths.only.base %>%
    rbind(deaths.only.base) %>%
    mutate(outcome = rep(pretty.outcomes,
                         each = nrow(deaths.only.base)))

imperial.df.to.plot <- plyr::rbind.fill(
                                 imperial.df,
                                 deaths.only.to.add) %>%
    mutate(scenario = pretty.labels(scenario, rev(pretty.scenarios)),
           outcome = pretty.labels(outcome, pretty.outcomes),
           pc_deaths = ifelse(scenario != deaths2015.scenario,
                              0,
                              pc_deaths),
           pc_deaths_min = ifelse(scenario == deaths2015.scenario,
                                  value_pc,
                                  0),
           region.focus = ifelse(region == "Sub-Saharan Africa",
                                 1, 0))

deaths2015.to.plot <- imperial.df.to.plot %>%
    filter(outcome == pretty.outcomes[[2]])

scenarios.to.plot <- setdiff(pretty.scenarios, "dummy")

xdodge <- 0.8
point.nudge <- 0.275#0.1 + xdodge /4

lab.plot.df <- data.frame(
    x = "Sub-Saharan Africa",
    y = 1.5,
    lab = "Total number of deaths\nas % of population in 2015",
    outcome = pretty.outcomes[[2]],
    scenario = deaths.only.base$scenario) %>%
    unique()

deaths2015.col <- "darkred"
bar.cols <- c("#7fd1b9", "#de6e4b", "#2f8396", "gray")
alpha.range <- c(0.5, 0.9)

### Make the plot of projected deaths --------------------------------

cov.title <- "COVID19 could pose a burden on healthcare systems in terms of deaths and illnesses that would not be expected to occur\nin this type of timeframe in a regular year."


cov.caption <- paste("Estimates of deaths and infections from",
                     "Imperial College. Population from World Bank",
                     "Deaths from Our World in Data")

imperial.plot <- imperial.df.to.plot %>%
    ggplot(aes(x = region,
               y = value_pc,
               fill = scenario)) +
    coord_flip() +
    facet_wrap(~outcome, ncol = 2, scales = "free_x") +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0,0.05),
                       breaks = scales::pretty_breaks(4)) +
    geom_blank(data = data.frame(region = "Sub-Saharan Africa",
                                 scenario = "Unmitigated",
                                 y = c(1.6),
                                 outcome = pretty.outcomes),
               mapping = aes(y = y)) +
    expand_limits(y = y.expand.limit) +
    geom_linerange(data = deaths2015.to.plot,
                   aes(x = region,
                       ymin = pc_deaths_min,
                       ymax = pc_deaths,
                       group = interaction(region, scenario)),
                   size = 0.9,
                   colour = "gray30",
                   alpha = min(alpha.range),
                   position = position_dodge(width = xdodge)) +
    geom_col(position = position_dodge(xdodge),
             width = xdodge,
             aes(alpha = region.focus)) +
    scale_alpha_continuous(range = alpha.range) +
    scale_fill_manual(values = bar.cols,
                      breaks = scenarios.to.plot) +
    geom_point(data = deaths2015.to.plot %>%
                   filter(pc_deaths > 0),
               aes(x = region, y = pc_deaths),
               colour = deaths2015.col,
               size = 3,
               position =  position_nudge(x = point.nudge)) +
    geom_text(data = lab.plot.df,
              hjust = 0,
              nudge_x = 0.04,
              nudge_y = -0.55,
              aes(x = x, y = y, label = lab),
              colour = deaths2015.col) +
    labs(title = cov.title,
         caption = cov.caption) +
    guides(fill = guide_legend(reverse = FALSE,
                               override.aes = list(shape = NA)),
           alpha = FALSE) +
    theme_minimal() +
    theme(axis.title = element_blank(),
          axis.text.y = element_text(size = 11),
          axis.text.x = element_text(size = 12),
          legend.position  = "top",
          legend.direction = "horizontal",
          legend.justification = "centre",
          legend.title = element_blank(),
          legend.text = element_text(size = 11),
          strip.text = element_text(size = 14),
          plot.title.position = "plot",
          #plot.caption.position = "plot",
          plot.caption = element_text(
              hjust = 0,
              margin = margin(t = 1, unit = "cm")),
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank())


imperial.filename <- file.path(
    final.figures.path,
    "Wed8Apr imperial_projections_March2020.png")

ggsave(filename = imperial.filename,
       plot = imperial.plot,
       width = 28,
       height = 14,
       units = "cm")

write.csv(x = imperial.df,
          file = "imperial_modellling_projections.csv",
          row.names = FALSE)
deaths.only.base <- imperial.df %>%
    select(region, outcome, value_pc, pc_deaths, scenario) %>%
    unique()
