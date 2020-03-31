library(tidyverse)

data.path <- file.path("poverty.csv")


dt.in <- read.csv(data.path, stringsAsFactors = FALSE)


pov.ppp <- "Poverty headcount ratio at $1.90 a day (2011 PPP) (% of population)"
pov.national <- "Poverty headcount ratio at national poverty lines (% of population)"

fix.wb.years <- function(yr.in){
    year <- substr(yr.in, start =  2, stop = 5)
    year.name <- paste0("y", year)
    return(year.name)
}


dt <- dt.in %>%
    mutate(poverty.series = ifelse(Series.Name == pov.ppp,
                                "poverty_ppp",
                                "poverty_national")) %>%
    rename_at(.vars = vars(starts_with("X")),
              .funs  = fix.wb.years) %>%
    mutate_at(.vars = vars(starts_with("y")),
              .funs = as.numeric) %>%
    select(country = Country.Name,
           ccode = Country.Code,
           poverty.series,
           starts_with("y")) %>%
    pivot_longer(cols = starts_with("y"),
                 names_to = "year",
                 names_prefix = "y",
                 values_to = "poverty.rate") %>%
    filter(ccode != "") %>%
    group_by(country, poverty.series) %>%
    group_modify(.f = function(df.in, group.var =  .y, ...){
        new.df.out = mutate(df.in,
                            poverty.diff = c(0, diff(poverty.rate)))
        return(new.df.out)
    }) %>%
    filter(poverty.series == "poverty_ppp")

order.in.2017 <- dt %>%
    filter(year == 2017) %>%
    arrange(poverty.rate) %>%
    pull(ccode) %>%
    unique()

dt.plot <- dt %>%
    mutate(ccode = factor(ccode,
                          levels = order.in.2017,
                          ordered = TRUE))

dt.time <-  dt.plot %>%
    group_by(ccode, year) %>%
    summarise(
        mortality.diff = mean(mortality.diff, na.rm =  TRUE),
        mortality = mean(mortality, na.rm = TRUE))


pre.recession <- seq(2003, 2007)
great.recession <- seq(2008, 2012)
post.recession <- seq(2013, 2017)

plot.years <- c(pre.recession,
                great.recession,
                post.recession)

make.recession.period <- function(year.in){
    year.out <- ifelse(year.in %in% pre.recession,
                       "pre",
                ifelse(year.in %in% great.recession,
                       "recession",
           ifelse(year.in %in% post.recession,
                  "post",
                  "other.year")))
    return(year.out)
}

dt.agg <- dt.plot%>%
    filter(year %in% plot.years) %>%
    mutate(recession = make.recession.period(year)) %>%
    group_by(country, recession) %>%
    mutate(poverty.mean = mean(poverty.rate, na.rm =  TRUE))


order.pre <- dt.agg %>%
    mutate(ccode = as.character(ccode)) %>%
    group_by(ccode) %>%
    summarize(mean.p = mean(poverty.mean, na.rm = TRUE)) %>%
    arrange(mean.p) %>%
    pull(ccode) %>%
    unique()

dt.agg.plot <- dt.agg %>%
    mutate(ccode = factor(ccode,
                          levels = order.pre,
                          ordered = TRUE),
           recession.group = factor(recession,
                                    levels  =
                                        c("pre", "recession", "post"),
                                    ordered =  TRUE))

quick.comparison.countries <- c("MUS","CPV",
                                "GHA", "SWZ",
                                "TZA", "BEN")

plot.max <- 610
recession.rect <- data.frame(
    xmin = factor(min(great.recession), levels = plot.years),
    xmax = factor(max(great.recession), levels = plot.years),
    ymin = 0,
    ymax = plot.max)

recession.rect <- data.frame(
    xmin = 5.5,
    xmax = 10.5,
    ymin = 0,
    ymax = plot.max)


poverty.mean <- dt.agg.plot %>%
    ggplot(aes(x = recession.group,
               y = poverty.mean)) +
    geom_point() +
    theme_minimal() +
    facet_grid(rows = "ccode") +
    theme(axis.text.x = element_text(size = 5))


mortality.over.time <- dt.time %>%
    filter(year %in% plot.years) %>%
    filter(ccode %in% quick.comparison.countries) %>%
    ggplot() +
    scale_y_continuous(limits = c(0, plot.max)) +
    geom_point(aes(x = year, y = mortality, colour = ccode))+
    geom_rect(inherit.aes = FALSE,
              data = recession.rect,
              mapping = aes(
                  xmin= xmin, xmax = xmax, ymin= ymin, ymax=ymax),
              alpha =  0.3)+
    theme_minimal() +
    theme(axis.text.x = element_text(size = 5))


mortality.changes.over.time <- dt.time %>%
    filter(year %in% plot.years) %>%
    filter(ccode %in% quick.comparison.countries) %>%
    ggplot() +
#    scale_y_continuous(limits = c(0, plot.max)) +
    geom_point(aes(x = year, y = mortality.diff, colour = ccode))+
#    geom_rect(inherit.aes = FALSE,
#              data = recession.rect,
#              mapping = aes(
#                  xmin= xmin, xmax = xmax, ymin= ymin, ymax=ymax),
#              alpha =  0.3)+
    theme_minimal() +
    theme(axis.text.x = element_text(size = 5))

