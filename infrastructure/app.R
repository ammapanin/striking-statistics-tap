## Create the Shiny App to present road costs data
## Lat updated: 11 September 2019

## Amma Panin, amma.panin@gmail.com
## www.africaceog.org

source("analyse_rocks.R")

library(shiny)

ui <- fluidPage(
    titlePanel(
        list(tags$img(src = "CEOGLogoFinal_Color.png",
                     height = '90px'),
             "CEoG Data Story - Costs of Road Infrastructure Projects"),

        windowTitle = "CEoG Data Story - Road costs"),

    em("Please allow about 10 seconds for loading"),

    HTML('<br><br>'),

    tabsetPanel(
        type = "tabs",

        tabPanel("Main",
                 fluidRow(
                     checkboxGroupInput(
                         "work_categories",
                         label = h3(paste("Choose which type of ",
                                          "construction project")),
                         choices = c("maintenance_and_upgrade",
                                     "new_construction"),
                         width = '100%'),
                     uiOutput("year_range"))
                 )
    )
)

server <- function(input, output){

    output$year_range <- renderUI({
        categories.selected <- input$work_categories
        countries.selected <- c("Ghana", "Liberia")

        years <- get_year_bounds(categories.selected,
                                 countries.selected)

        bob <- sliderInput("get_year_range",
                           label = h3("Slider Range"),
                           min = years$min,
                           max = years$max,
                           value = as.numeric(years),
                           sep = "")
        return(bob)
    })
}

shinyApp(ui = ui,
         server = server)
