library(dplyr)
library(ggplot2)
library(shiny)

###############
## Constants ##
###############

year <- 2023

classes <- read_csv(here::here("data/class-descriptions.csv")) |>
    mutate_at("class", as.factor)

## Zoning code summary from
## https://secondcityzoning.org/resources/import/zoning-code-summary-district-types.csv
zoning_code_summary <- read_csv(here::here("data/zoning-code-summary-district-types.csv"))

all_pins <- read_csv("data/zoning_pins.csv") |>
    mutate_at("class", as.factor)

#####################
## Shiny Functions ##
#####################

ui <- page_sidebar(
    title = "Cook County Real Estate Explorer",

    sidebar = sidebar(
        tags$a(href="https://charles-threlkeld-cook-county-real-estate-explorer.share.connect.posit.cloud/zoneApp.html", "See here for a motivating example."),

        selectInput(
            "location",
            label = "Neighborhood / Suburb",
            choices = all_pins |> distinct(location) |> arrange(location),
            selected = "Lincoln Square"),

        selectInput(
            "zone",
            label = "Property Zone",
            choices = all_pins |> distinct(zone),
            selected = "RS-3"),

        checkboxGroupInput(
            "checkGroup",
            "Select Property Classes for Comparison",
            choices = classes$class,
            selected = c("203", "211")
        )
    ),

    card(
        card_header("Class Comparison"),
        plotOutput("classComparison")
    ),

    card(
        card_header("Zones in Location"),
        tableOutput("zoneTable")
    ),

    card(
        card_header("Property Classes within Zone"),
        tableOutput("classTable")
    ),
)

server <- function(input, output) {
    loc_df <- reactive({
        all_pins |>
            filter(location == input$location)
    })

    observeEvent(input$location, {
                 updateSelectInput(
                     inputId = "zone",
                     choices = loc_df() |> distinct(zone) |> arrange(zone) |> pull(zone))
    })

    zone_df <- reactive({
        loc_df() |>
            filter(zone == input$zone)
    })

    observeEvent(input$zone, {
                 updateCheckboxGroupInput(
                     inputId = "checkGroup",
                     choices = zone_df() |> distinct(class) |> arrange(class) |> pull(class))
    })

    class_df <- reactive({
        zone_df() |>
            filter(class %in% input$checkGroup)
    })

    output$classComparison <- renderPlot({
        ggplot(class_df(), aes(x = cash_value, y = class)) +
            geom_violin(trim = FALSE) +
            stat_summary(fun=mean, geom="point", shape=0, size=2, color="red") +
            stat_summary(fun=median, geom="point", shape=0, size=2, color="blue") +
            scale_x_continuous("Fair Cash Value",
                                       labels = scales::label_currency(prefix="$")) +
            labs(
                title = stringr::str_glue("Cash Value of {input$zone} Properties in {input$location}"))
    })

    output$zoneTable <- renderTable({
        loc_df() |>
            group_by(zone) |>
            summarize(count = n()) |>
            arrange(desc(count)) |>
            left_join(zoning_code_summary, by=c("zone" = "district_type_code")) |>
            select(count, zone, juan_description) |>
            mutate(description = juan_description, .keep = "unused")
    })

    output$classTable <- renderTable({
        zone_df() |>
            group_by(class) |>
            summarize(count = n()) |>
            arrange(desc(count)) |>
            left_join(classes)
    })
}

shinyApp(ui, server, options = list(host = "0.0.0.0", port = 4444))
