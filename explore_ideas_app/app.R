library(shiny)
library(prodpad)
library(DT)
library(dplyr)

pcli <- prodpad()

ui <- fluidPage(

    class = "panel panel-heading",
    div(
        class = "panel-heading",
        h2("Interactive ProdPad API /GET ideas")
    ),


    fluidRow(

        class = "panel-body",
        column(
            width = 6,
            tags$div(
                class = "panel panel-default",
                tags$div(class = "panel-heading", "Product Filter"),
                tags$div(
                    class = "panel-body",
                    id = "sort1",
                    uiOutput("select_product")
                )
            ),
            tags$div(
                class = "panel panel-default",
                tags$div(class = "panel-heading", "Persona Filter"),
                tags$div(
                    class = "panel-body",
                    id = "sort1",
                    uiOutput("select_personas")
                )
            ),
            tags$div(
                class = "panel panel-default",
                tags$div(class = "panel-heading", "Tag Filter"),
                tags$div(
                    class = "panel-body",
                    id = "sort1",
                    uiOutput("select_tags")
                )
            )
        ),
        column(
            width = 6,
            actionButton("get", "GET /ideas"),
            DT::dataTableOutput("ideas")
        )
    )
)

server <- function(input, output) {

    query <- reactiveValues(data = NULL)

    observeEvent(input$get, {
        query$data <- prodpad::get_ideas(pcli, product = input$products, tags = input$tags)
    })

    output$ideas <- renderDT({
        if (is.null(query$data)) return()
        query$data %>%
            select(id, project_id, web_url, title, description)
    })

    all_products <- get_products(pcli)
    output$select_product <- renderUI({
        selectizeInput(
            "products",
            "Products",
            choices = rlang::set_names(all_products$name),
            multiple = TRUE
        )
    })

    all_personas <- get_personas(pcli)
    output$select_personas <- renderUI({
        selectizeInput(
            "personas",
            "Personas",
            choices = rlang::set_names(all_personas$name),
            multiple = TRUE
        )
    })

    all_tags <- get_tags(pcli)
    output$select_tags <- renderUI({
        selectizeInput(
            "tags",
            "Tags",
            choices = rlang::set_names(all_tags$tag),
            multiple = TRUE
        )
    })
}

shinyApp(ui = ui, server = server)
