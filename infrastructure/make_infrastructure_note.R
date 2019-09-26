### Code to explore the PPI data
### Last updated: 11 September 2019
### amma.panin@gmail.com

## World Bank data sources
## https://ppi.worldbank.org/en/customquery
## https://www.doingbusiness.org/en/reports/thematic-reports/road-costs-knowledge-system
## http://web.worldbank.org/WBSITE/EXTERNAL/TOPICS/EXTTRANSPORT/EXTROADSHIGHWAYS/0,,contentMDK:20485235~menuPK:1097394~pagePK:148956~piPK:216618~theSitePK:338661,00.html#english

library(tidyverse)
library(maptools) # to get a simple world map
library(xtable)
library(RColorBrewer)

### Load paths -------------------------------------------------------

infrastructure.folder <- file.path(normalizePath("~"),
                                   "Dropbox",
                                   "Work Documents",
                                   "World Bank",
                                   "amma_striking_stats_local",
                                   "infrastructure")

setwd(infrastructure.folder)


### Load data --------------------------------------------------------

wb.in <- readRDS("data/wb_indicators_clean.Rds")

rocks.path <- "data/rocks_combined_1994-2018.csv"
rocks.in <- read.csv(rocks.path, stringsAsFactors = FALSE)


### Prepare ROCKS data and do preliminary exploration ----------------

rocks <- rocks.in %>%
    filter(cost_type != "Ratio") %>%
    filter(!(work_type %in% c("Spot Regravelling",
                              "Patching")  &
             is.na(cost_M_USD_per_km)))

rocks.estimate <- rocks %>%
    filter(cost_type == "Estimate")

### Filter useful country level data ---------------------------------

series.to.keep <- c("GDP (constant 2010 US$)")

wb.use <- wb.in %>%
    filter(series %in% series.to.keep)

### Rocks summary information ----------------------------------------

n.projects <- length(rocks$project_idx %>% unique())
n.sections <- length(rocks$section_code %>% unique())
n.countries <- length(rocks$ccode %>% unique())

n.exclude <- rocks.in %>%
    filter(work_type %in% c("Spot Regravelling",
                            "Patching")  &
           is.na(cost_M_USD_per_km))


### Plot data coverage -----------------------------------------------
n.colours <- c("#F0F0F0",
               brewer.pal(n = 8, name  = "Blues")[3:7])

section.levels <- c("1 to 10",
                    "11 to 50",
                    "51 to 200",
                    "more than 200")

rocks.country <- rocks %>%
    select(cost_type, cost_M_USD_per_km, country, section_code) %>%
    pivot_wider(names_from = cost_type,
                values_from = cost_M_USD_per_km) %>%
    group_by(country) %>%
    summarise(n_sections = n()) %>%
    mutate(n_sections_cats = cut(n_sections,
                                 breaks = c(0, 10, 50, 200,
                                            max(n_sections))),
           n_sections_txt = factor(n_sections_cats,
                                   labels = section.levels,
                                   ordered = TRUE))

world.df <- map_data("world") %>%
    merge(rocks.country,
          by.x =  "region",
          by.y = "country",
          all.x  = TRUE) %>%
    filter(region != "Antarctica") %>%
    mutate(n_sections_txt = as.character(n_sections_txt),
           n_sections_txt = ifelse(is.na(n_sections_txt), 0,
                                   as.character(n_sections_txt)),
           n_sections_txt = factor(n_sections_txt,
                                   labels = c("0", section.levels),
                                   ordered = TRUE)) %>%
    arrange(order)

coverage.plot <- ggplot(world.df,
                        aes(x = long,
                            y = lat,
                            group = group,
                            fill = n_sections_txt)) +
    geom_polygon() +
    guides(fill = guide_legend(
               title =
                   "Number of work activities\nin database, 1984-2017")) +
    scale_fill_manual(values = n.colours) +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.title =  element_blank())

ggsave(filename ="figures/coverage_map.pdf",
       plot = coverage.plot,
       width = 25.5,
       height = 11.1,
       units = "cm")

### Understand costs--------------------------------------------------

## Subset data and widen it to have a country-project on each row ----
##  and different types of costs and lengths on each  column

units.df <- rocks %>%
    select(country,
           ccode,
           wb_region,
           cost_type,
           unit_cost = cost_M_USD_per_km,
           length_km,
           section_code) %>%
    mutate(cost_type = tolower(cost_type)) %>%
    pivot_wider(names_from = cost_type,
                values_from = c(unit_cost, length_km))  %>%
    rename_at(.vars = vars(starts_with("unit_cost")),
              function(x){gsub("unit_", "", x)})

cost_data_counts <- with(
    units.df,
    c(sum(!is.na(cost_estimate)
          & is.na(cost_actual) & is.na(cost_contract)),
      sum(is.na(cost_estimate)
          & is.na(cost_actual) & !is.na(cost_contract)),
      sum(is.na(cost_estimate)
          & !is.na(cost_actual) & is.na(cost_contract)),
      sum(!is.na(cost_estimate)
          & is.na(cost_actual) & !is.na(cost_contract)),
      sum(!is.na(cost_estimate)
          & !is.na(cost_actual) & is.na(cost_contract)),
      sum(is.na(cost_estimate)
          & !is.na(cost_actual) & !is.na(cost_contract)),
      sum(!is.na(cost_estimate)
          & !is.na(cost_actual) & !is.na(cost_contract))))

length_data_counts <- with(
    units.df,
    c(sum(!is.na(length_km_estimate)
          & is.na(length_km_actual) & is.na(length_km_contract)),
      sum(is.na(length_km_estimate)
          & is.na(length_km_actual) & !is.na(length_km_contract)),
      sum(is.na(length_km_estimate)
          & !is.na(length_km_actual) & is.na(length_km_contract)),
      sum(!is.na(length_km_estimate)
          & is.na(length_km_actual) & !is.na(length_km_contract)),
      sum(!is.na(length_km_estimate)
          & !is.na(length_km_actual) & is.na(length_km_contract)),
      sum(is.na(length_km_estimate)
          & !is.na(length_km_actual) & !is.na(length_km_contract)),
      sum(!is.na(length_km_estimate)
          & !is.na(length_km_actual) & !is.na(length_km_contract))))

cost.names <- c("estimated", "contract", "actual")

data.matrix <- data.frame(cost_data = cost_data_counts,
                          length_data = length_data_counts) %>%
    cbind(
        rbind(c("x", "", ""),
              c("", "x", ""),
              c("", "", "x"),
              c("x", "x", ""),
              c("x", "", "x"),
              c("", "x", "x"),
          c("x", "x", "x")) %>%
        data.frame()) %>%
    rename_all(seq_along)

header.row <- t(c("cost", "length of road", cost.names)) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    rename_all(seq_along)

mk.bf <- function(x.in){
    paste0("\\textbf{",
        x.in,
        "}")}

total.row <- c(mk.bf(sum(cost_data_counts)),
               mk.bf(sum(length_data_counts)),
               "",
               "",
               "")

n.costs.header <- paste0("\\\\multicolumn{3}{p{6cm}|}",
                         "{\\\\textbf{Number of ",
                         "activities with data}}")
cost.type.header <- paste0("\\\\multicolumn{3}{p{5cm}}",
                           "{\\\\textbf{Type of data recorded}}")

main.header <- c("", "", "col2.place", "", "")

tab.meat <- cbind(c("col1.place", rep("", 8), "\\textbf{Total}"),
                  rbind(main.header,
                        header.row,
                        data.matrix,
                        total.row,
                        stringsAsFactors = FALSE))

tab.caption <- paste("Different sources of data. ROCKS compiles data",
                     "from sources that include project propsals,",
                     "contracts, and evaluations. Projects appearing",
                     "in the database may have information based on",
                     "estimates, details of the signed contract,",
                     "actual costs evaluated after project completion",
                     "or some combination of the three.",
                     "\\vspace{1em}")

data.table_0 <- xtable(tab.meat,
                       align  = "lrrr|lll",
                       caption= tab.caption,
                       label = "tab_count_sources") %>%
    print(sanitize.text.function = identity,
          include.rownames = FALSE,
          include.colnames = FALSE,
          hline.after = c(0, 9, 10),
          caption.placement = "top")

data.table_1 <- gsub("col1.place &  &", n.costs.header, data.table_0)

data.table_2 <- gsub("col2.place &  &", cost.type.header, data.table_1)

write(data.table_2,  file = "tables/data_count_table.tex")


### Histogram of costs -----------------------------------------------

hist.col <- "darkgrey"
hist.blue1 <- "#2171B5"
hist.blue2 <- "#084594"

histogram.df <- units.df %>%
    select(starts_with("cost")) %>%
    pivot_longer(cols = starts_with("cost"),
                 names_to = "cost_type",
                 values_to =  "cost")

hist.dets <- hist(histogram.df$cost,
                  breaks = seq(0,
                               max(histogram.df$cost,
                                   na.rm = TRUE)+ 2,
                               0.5))

hist.height <- round(max(hist.dets$counts), digits = -3) - 500

hist.text.df <- data.frame(
    x = 3,
    y = c(hist.height, hist.height - 500),
    property = c("median", "mean"),
    value = c(median(histogram.df$cost, na.rm = TRUE),
              mean(histogram.df$cost, na.rm = TRUE))) %>%
    mutate(text = paste0(property, " = ",
                         round(value, 2),
                         " m USD"))

cost.histogram <- histogram.df %>%
    ggplot(aes(x = cost)) +
    geom_histogram(fill = hist.col, colour = hist.col,
                   binwidth  = 1) +
    scale_x_continuous(breaks = c(0,  5, 10, 15, 20, 25, 28))+
    geom_vline(xintercept = hist.text.df %>%
                   filter(property == "mean") %>%
                   pull(value),
               colour = hist.blue1) +
    geom_vline(xintercept = hist.text.df %>%
                   filter(property == "median") %>%
                   pull(value),
               colour = hist.blue2) +
    labs(x = "Cost per km of project in millions of USD",
         y = "Number of work activities")+
    theme_minimal() +
    geom_text(data = hist.text.df,
              aes(x = x, y = y, label = text, colour = property),
              hjust = 0,
              size = 5) +
    geom_segment(data = hist.text.df,
                 aes(x = x,
                     xend = value,
                     y = y,
                     yend = y - 10 * x ,
                     colour = property),
                 arrow = arrow(angle = 20,length = unit(0.25, "cm")))+
    scale_colour_manual(values = c(hist.blue1, hist.blue2)) +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 15),
          legend.position  = "none")


cost.histogram

ggsave(filename = "figures/cost_histogram.pdf",
       plot = cost.histogram,
       width = 27.2,
       height = 13.9,
       units = "cm")

### Estimated against actual costs -----------------------------------

region.colours <- rev(brewer.pal(11, "BrBG")[7:11])
africa.colour <- brewer.pal(6, "Spectral")[1]

estimate_actual.df <- units.df %>%
    filter(!is.na(cost_actual) & !is.na(cost_estimate)) %>%
    filter(!cost_actual < 0.001) %>%
    filter(!cost_estimate < 0.001) %>%
    mutate(cost_overrun_dummy = ifelse(
               cost_actual > cost_estimate,1, 0),
           cost_overrun_ratio = cost_actual / cost_estimate,
           length_overrun_dummy = ifelse(
               length_km_actual > length_km_estimate,  1, 0),
           length_overrun_ratio =
               length_km_actual / length_km_estimate)

prop.overun <- (sum(estimate_actual.df$cost_overrun_dummy) /
                length(estimate_actual.df$cost_overrun_dummy))

units.scatterplot <- estimate_actual.df %>%
    ggplot(aes(x = cost_estimate,
               y  = cost_actual,
               colour = wb_region)) +
    geom_point(alpha = 0.6, size = 3) +
    scale_x_log10(breaks = c(0.001, 0.01, 0.1, 1, 10),
                  labels = c(0, 0.01, 0.1, 1, 10),
                  limits = c(0.001, 10))+
    scale_y_log10(breaks = c(0.001, 0.01, 0.1, 1, 10),
                  labels = c(0, 0.01, 0.1, 1, 10),
                  limits = c(0.001, 10)) +
    scale_colour_manual(values= c(region.colours,
                                  africa.colour))+
    labs(x = paste("Estimated road activity cost per ",
                   "km, millions of USD (log-scale)"),
         y = paste("Actual road activity cost per km, ",
                   "millions of USD (log-scale)"))+
    theme(panel.grid.minor = element_blank(),
          legend.title = element_blank(),
          legend.position = "top",
          legend.text = element_text(size = 12),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 15)) +
    geom_segment(data = data.frame(),
                 aes(x = 0.0012, xend = 10,
                     y = 0.0012, yend = 10),
                 colour = "black",
                 alpha = 0.5)+
    geom_segment(data =  data.frame(),
                 aes(x = 0.001, xend = 0.0012,
                     y = 0.001, yend = 0.0012),
                 linetype = "dotted",
                 colour = "black",
                 alpha = 0.5)

units.scatterplot

ggsave(filename = "figures/estimate_actual.pdf",
       plot = units.scatterplot,
       width = 22.8,
       height = 16.4,
       units = "cm")

### Overuns by road length -------------------------------------------

overrun_by_length.df <- estimate_actual.df %>%
    mutate(short_and_expensive = cost_overrun_ratio > 1 &
               length_overrun_ratio < 1)

ratios.by.region <- overrun_by_length.df %>%
    group_by(wb_region) %>%
    summarise(prop = mean(short_and_expensive, na.rm =  TRUE))

overrun_bkground.cols <- brewer.pal(9, "BrBG")[1:4]

chart.headline.text <- paste(
    "%s%% of roads in sub-Saharan",
    "Africa were shorter and more expensive than",
    "planned")

chart.headline <- sprintf(chart.headline.text,
                          ratios.by.region %>%
                          filter(wb_region == "Sub-Saharan Africa") %>%
                          pull(prop) %>%
                          as.numeric() %>%
                          map(~ . * 100) %>%
                          pluck(1)%>%
                          round(0))

data.x.max <- max(overrun_by_length.df$cost_overrun_ratio)
data.y.max <- max(overrun_by_length.df$length_overrun_ratio)

plot.x.min <- -1.6
plot.y.min <- -0.3
plot.x.max <- round(data.x.max)
plot.y.max <- round(data.y.max)

overrun_rects.df <- data.frame(
    xmin.in = c(plot.x.min, 1),
    xmax.in = c(1, plot.x.max),
    ymin.in = rep(c(plot.y.min,1), each = 2),
    ymax.in = rep(c(1, plot.y.max),
                  each = 2),
    fill.cats = factor(seq(1, 4)))

overrun_text.df <- data.frame(
    x = rep(c(0.9,  5.5), each = 2),
    y = c(-0.08, 4.5),
    text = c("C: road was shorter and\ncheaper than planned",
             "A: road was longer and\ncheaper than planned",
             "D: road was shorter and more expensive than planned",
             "B: road was longer and more expensive than  planned"),
    adjustment = c(1, 1, 0.5, 0.5))

text.col <- "orangered4"#brewer.pal(11, "BrBG")[[1]]

overrun_by_length.plot <-  overrun_by_length.df%>%
    ggplot(aes(x = cost_overrun_ratio,
               y = length_overrun_ratio,
               colour = wb_region,
               size = cost_actual)) +
    scale_x_continuous(breaks = c(0, 1, 2, 4, 6, 8, 10),
                       limits = c(plot.x.min, plot.x.max),
                       expand =  c(0, 0)) +
    scale_y_continuous(limits = c(plot.y.min, plot.y.max),
                       expand  = c(0, 0)) +
    scale_colour_manual(values = c(region.colours,
                                   africa.colour)) +
    labs(title = chart.headline,
         x = "Ratio of actual to estimated road work cost",
         y = "Ratio of actual to estimated road length",
         caption = paste("Data from ROCKS database.",
                         "Each point represents a road work",
                         "project sponsored in part or wholly",
                         "by the World Bank and selected",
                         "international\ndonors between 1994",
                         "and 2018.")) +
    geom_rect(data = overrun_rects.df,
              inherit.aes = FALSE,
              mapping =  aes(xmin = xmin.in,
                  ymin = ymin.in,
                  xmax = xmax.in,
                  ymax = ymax.in,
                  fill = fill.cats),
              alpha  = 0.35) +
    geom_point() +
    scale_size(breaks = c(1, 5, 10), range=c(1, 9)) +
    geom_text(data = overrun_text.df,
              inherit.aes = FALSE,
              aes(x = x, y =  y, label =  text,
                  hjust = adjustment),
              colour = text.col,
              size = 4) +
    scale_fill_manual(values = overrun_bkground.cols)+
    theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          legend.text = element_text(size = 12),
          plot.title = element_text(
              size = 19,
              margin = margin(t = 10, r = 5, b = 15, l = 0)),
          plot.caption = element_text(
              hjust  = 0,
              margin = margin(t = 15, r = -5, b = 10, l = 0))) +
    guides(fill = FALSE,
           colour = guide_legend(title = "", order = 1,
                                 keyheight = 1.5),
           size = guide_legend(title = "Cost per km, M USD"))

overrun_by_length.plot

ggsave(filename = "figures/road_cost_length_overrun.pdf",
       plot = overrun_by_length.plot,
       width = 28.2,
       height = 17,
       units = "cm")

ggsave(filename = "figures/road_cost_length_overrun.png",
       plot = overrun_by_length.plot,
       width = 28.2,
       height = 17,
       units = "cm")


## Make boxplots by region





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


