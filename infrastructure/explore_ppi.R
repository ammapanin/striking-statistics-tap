### Code to explore the PPI data
### Last updated: 26 August 2019
### amma.panin@gmail.com

## World Bank data sources
## https://ppi.worldbank.org/en/customquery
## https://www.doingbusiness.org/en/reports/thematic-reports/road-costs-knowledge-system
## http://web.worldbank.org/WBSITE/EXTERNAL/TOPICS/EXTTRANSPORT/EXTROADSHIGHWAYS/0,,contentMDK:20485235~menuPK:1097394~pagePK:148956~piPK:216618~theSitePK:338661,00.html#english

library(tidyverse)
library(readxl)
library(maptools) # to get a simple world map

### Load paths -------------------------------------------------------

infrastructure.folder <- file.path(normalizePath("~"),
                                   "Dropbox",
                                   "Work Documents",
                                   "World Bank",
                                   "amma_striking_stats_local",
                                   "infrastructure")

setwd(infrastructure.folder)


### Load data --------------------------------------------------------

wb.in <- readRDS("wb_indicators_clean.Rds")

country.codes <- read.csv("amma_country_codes.csv",
                          stringsAsFactors = FALSE)

rocks.dt.path <- "ROCKS-Update-June-2018.xlsx"
excel_sheets(rocks.dt.path)

rocks.in <- read_excel(rocks.dt.path,
                       sheet = "All",
                       skip = 2)

## ppi.dt.path <- "ppi_world_bank.xlsx"
## excel_sheets(ppi.dt.path) # View the sheets in the excel document
## ppi.in <- read_excel(ppi.dt.path)


## Figure out countries to manually recode
setdiff(rocks.in$Country, country.codes$wb)

ccodes.merge.df <- country.codes %>%
    select(country = wb,
           ccode = code,
           region,
           inc_group) %>%
    unique() %>%
    merge(rocks.in %>%
          select(rocks_country = Country) %>%
          unique(),
          by.x = "country",
          by.y = "rocks_country",
          all.x = TRUE) %>%
    mutate(country = ifelse(ccode == "COD", "Congo, Dem. Rep.", country),
           country = ifelse(ccode == "MKD", "Macedonia, FYR", country),
           country = ifelse(ccode == "YEM", "Yemen, Rep.",  country),
           country = ifelse(ccode == "CPV", "Cape Verde", country))  %>%
    rename(rocks_country = country)

## Check that names line up
setdiff(rocks.in$Country, ccodes.merge.df$rocks_country)

### Prepare ROCKS data and do preliminary exploration ----------------

rocks.names <- c("country",
                 "region",
                 "project_code_in",
                 "component_code_in",
                 "section_code_in",
                 "code_in",
                 "cost_type",
                 "year",
                 "work_type",
                 "duration",
                 "length_km",
                 "cost_M",
                 "currency",
                 "cost_M_USD",
                 "unit_cost_M_USD_per_km",
                 "check_length_under",
                 "check_cost_under",
                 "check_cost_over",
                 "check_cost_over_2",
                 "aggregate_check",
                 "outlier_categories",
                 "outlier_explanations")

names(rocks.in) <- rocks.names

rocks <- rocks.in %>%
    merge(ccodes.merge.df,
          by.x = "country",
          by.y = "rocks_country",
          all.x = TRUE) %>%
    filter(country != "Yugoslavia, FR (Serbia/Montenegro)") %>%
    mutate(proj.component.section = paste(project_code_in,
                                          component_code_in,
                                          section_code_in))  %>%
    arrange(component_code_in, section_code_in) %>%
    mutate(project_idx = 100 + group_indices(., project_code_in)) %>%
    ungroup() %>%
    mutate(section_idx = group_indices(.,proj.component.section),
           section_code = paste0(project_idx,
                                 str_pad(section_idx,
                                         width = 3,
                                         side = "left",
                                         pad = 0)))  %>%
    ungroup() %>%
    rename(wb_region = region.y)

rocks.estimate <- rocks %>%
    filter(cost_type == "Estimate")

### Filter useful country level data ---------------------------------

series.to.keep <- c("GDP (constant 2010 US$)")

wb.use <- wb.in %>%
    filter(series %in% series.to.keep)

### Rocks summary information ----------------------------------------

n.projects <- length(rocks$project_code_in %>% unique())
n.sections <- length(rocks.estimate$section_code %>% unique())

proj.section <- proj.section.tab[proj.section.tab[,1] != 0, ] %>%
    data.frame() %>%
    rename(section_name = section,
           section_freq = Freq)


### Explore ROCKS data -----------------------------------------------

rocks.country.tab <- table(rocks$country)

rocks.country <- rocks.estimate %>%
    group_by(country, project_idx) %>%
    summarise(n_sections_per_project = n()) %>%
    ungroup() %>%
    group_by(country) %>%
    summarise(n_sections = sum(n_sections_per_project),
              n_projects = n())

### Plot data coverage -----------------------------------------------

world.df <- map_data("world") %>%
    merge(rocks.country,
          by.x =  "region",
          by.y = "country",
          all.x  = TRUE) %>%
    filter(region != "Antarctica") %>%
    mutate(n_projects = ifelse(is.na(n_projects), 0, n_projects),
           n_projects_cats =
               ifelse(n_projects >= 5,
                      "5 and more",
                      n_projects)) %>%
    arrange(order)

coverage.plot <- ggplot(world.df,
                        aes(x = long,
                            y = lat,
                            group = group,
                            fill = n_projects_cats)) +
    geom_polygon()


ggsave(filename ="figures/coverage_map.pdf",
       plot = coverage.plot)

### Plot costs--------------------------------------------------------

##  Take out 'outliers' where unit costs where more than 10M per km!

units.df <- rocks %>%
    select(country,
           ccode,
           wb_region,
           cost_type,
           unit_cost_M_USD_per_km,
           length_km,
           project_idx) %>%
    filter(cost_type != "Ratio")  %>%
    group_by(wb_region, country, ccode, project_idx, cost_type) %>%
    summarise(unit_cost = mean(unit_cost_M_USD_per_km, na.rm  = TRUE),
              length_km = mean(length_km, na.rm  = TRUE)) %>%
    ungroup() %>%
    pivot_wider(names_from = cost_type,
                values_from = c(unit_cost, length_km))  %>%
    rename_at(.vars = vars(starts_with("unit_cost")),
              function(x){tolower(gsub("unit_", "", x))})

subunits.df <- units.df  %>%
    group_by(ccode) %>%
    mutate(n_cost_less_1 =
               sum(cost_actual <=1 & cost_estimate <=1),
           n_total = n(),
           p_cost_less_1 = n_cost_less_1 / n_total)  %>%
    filter(cost_actual <= 1 & cost_estimate <= 1)

units.scatterplot <- subunits.df %>%
    ggplot(aes(x =unit_cost_estimate,
               y  = unit_cost_actual,
               colour = wb_region)) + ## ,
               ## size = length_km_Estimate))+
    geom_point()+ #shape = "-") +
    geom_abline(slope =  1, intercept = 0) # +
    #scale_size_continuous(range = c(6, 25))

units.scatterplot

## Make boxplots by region

regional.df <- subunits.df %>%
    group_by(wb_region)  %>%
    mutate(p_cost_less_1 = sum(n_cost_less_1) / sum(n_total),
           region_lab =
               paste(wb_region, round(p_cost_less_1, 2)))

units.boxplots <- regional.df %>%
    ggplot(aes(x = region_lab, y = cost_actual)) +
    geom_boxplot()

units.boxplots





## Histogram

units.histogram <- units.df %>%
    ggplot(aes(x = Actual)) +
    geom_histogram()
units.histogram


### Create a tex file then a pdf from .Rnw file ----------------------
output.file.base <- "Road_costs_preliminary_note"
output.file.rnw <- paste0(output.file.base, ".Rnw")
output.file.tex <- paste0(output.file.base, ".tex")
output.file.pdf <- paste0(output.file.base, ".pdf")

Sweave(output.file.rnw)
tools::texi2pdf(output.file.tex, clean = TRUE, quiet = TRUE)

print(sprintf("End of script. pdf written out to %s.",
               output.file.pdf))


