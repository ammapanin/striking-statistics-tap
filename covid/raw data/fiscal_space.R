

devtools::install_github("ricardo-bion/ggradar",
                          dependencies = TRUE)

library(ggradar)
library(dplyr)
library(scales)
library(tibble)

mtcars_radar <- mtcars %>%
  as_tibble(rownames = "group") %>%
  mutate_at(vars(-group), rescale) %>%
  tail(4) %>%
  select(1:10)

ggradar(mtcars_radar)


coord_radar <- function (theta = "x", start = 0, direction = 1) {
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") "y" else "x"
  ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start,
          direction = sign(direction),
          is_linear = function(coord) TRUE)
}

low <- 0.33
medium <- 0.67
high <- 1
fiscal.space.df <- data.frame(
    matrix(c(c("Angola", low, high, high, high),
             c("DRC", medium, high, high, medium),
             c("Mauritius", high, high, medium, medium),
             c("South Africa", medium, low, high, high)),
           ncol  = 5, byrow = TRUE))

names(fiscal.space.df) <- c("country",
                            "inflation",
                            "primary\nbalance",
                            "public\ndebt",
                            "current\naccount")

fsd <- fiscal.space.df %>% pivot_longer(cols = 2:5)%>%
    mutate(value = as.numeric(as.character(value)))

fsd.total <- fsd  %>%
    pivot_wider(names_from = name,
                values_from = value) %>%
    mutate(country = as.character(country)) %>%
    rbind(c("africa", colMeans(.[,2:5]))) %>%
    pivot_longer(cols = 2:5) %>%
    filter(country =="africa")%>%
    mutate(value = as.numeric(as.character(value)))


account.labels <- data.frame(x= "current\naccount",
                             y = c(0.33, 0.67,  1),
                             labels = c("low", "medium", "high"))

bobby <- ggplot(fsd,
                aes(x = name,
                    y = value,
                  group = country)) +
    scale_y_continuous(limits = c(0, 1),
                       breaks = c(0, 0.33, 0.67, 1))+
    geom_polygon(colour = "orange",  fill =  "NA") +
    facet_wrap(~country) +
                                        #geom_line()+
    coord_radar() +
    theme(panel.background=element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.line.x=element_blank(),
          panel.grid.major=element_line(size=0.3,
                                        linetype = 2,colour="grey"))



bobby2 <- ggplot(fsd.total,
                aes(x = name,
                  y = value,
                  group = country)) +
      scale_y_continuous(limits = c(0, 1),
                         breaks = c(0, 0.33, 0.67, 1))+
    geom_polygon(colour = "orange",  fill =  "NA") +
    geom_text(data = account.labels,
              inherit.aes = FALSE,
              aes(x = x, y = y, label = labels)) +
    labs(title = "Africa average") +
    #geom_line()+
    coord_radar() +
    theme(panel.background=element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.line.x=element_blank(),
          panel.grid.major=element_line(size=0.3,linetype = 2,colour="grey"))



title <- ggdraw() +
  draw_label(
    "How much fiscal space do countries have?",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

caption <- ggdraw() +
  draw_label(
    "Source: Africa Pulse 2020",
    size = 10,
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

fiscal.plot <- plot_grid(title,plot_grid(bobby2,bobby),caption,
                         ncol=1,
                         rel_heights = c(0.1, 1,0.1))


plot.final <- add.e4t.branding(fiscal.plot,
                               "SSA_fiscal_space")

