library(shiny)
library(fs)
library(DT)
library(qs)
library(stringr)
library(dplyr)
library(purrr)

dat_file <- fs::dir_ls("data") %>% sort(., decreasing = TRUE) %>% purrr::chuck(1)
dat <- qs::qread(dat_file)
dat$repo = paste0("<a href='", dat$url, "'>", dat$repo, "</a>")
dat <- dplyr::select(dat, -url)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("JIMBRIG'S GITHUB STARRED REPOS"),
    h5("View all starred repositories in text format from: ", tags$a("HERE", href = "https://jimbrig.github.io/jimsghstars"), "."),
    h5("View script used to collect data from: ", tags$a("HERE", href = "https://gist.github.com/75fd952bad479737dc7b32b6ec203652#file-get_github_stars-r"), "."),
    hr(),

    # Sidebar with a slider input for number of bins

    # Show a plot of the generated distribution
    sidebarLayout(
        sidebarPanel(
            h3("Filters"),
            width = 2,
            shiny::dateRangeInput(
                "last_updated",
                "Last Updated",
                start = min(dat$last_updated, na.rm = TRUE),
                end = max(dat$last_updated, na.rm = TRUE),
                min = min(dat$last_updated, na.rm = TRUE),
                max = max(dat$last_updated, na.rm = TRUE)
            ),
            shiny::dateRangeInput(
                "created",
                "Creation Dates",
                start = min(dat$created, na.rm = TRUE),
                end = max(dat$created, na.rm = TRUE),
                min = min(dat$created, na.rm = TRUE),
                max = max(dat$created, na.rm = TRUE)
            ),
            shiny::sliderInput(
                "stars",
                label = "Minimum Number of Stars",
                min = 0,
                max = max(dat$stargazers),
                value = 0
            ),
            shiny::selectInput(
                "language",
                label = "Primary Language:",
                choices = unique(dat$language),
                selected = unique(dat$language),
                multiple = TRUE
            )
        ),
        mainPanel(
            DT::DTOutput("table")
        )
    )

)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    data <- reactive({

        # browser()

        dat %>%
            dplyr::filter(
                .data$last_updated >= input$last_updated[1],
                .data$last_updated <= input$last_updated[2],
                .data$created >= input$created[1],
                .data$created <= input$created[2],
                .data$stargazers >= input$stars,
                .data$language %in% input$language
            )

    })

    output$table <- DT::renderDT({

        out <- data()

        DT::datatable(
            out,
            escape = FALSE,
            rownames = FALSE,
            colnames = stringr::str_to_title(colnames(out)),
            caption = paste0(
                "Last updated on ",
                stringr::str_sub(basename(dat_file), 1, 10)
            ),
            extensions = c("Buttons"),
            options = list(
                dom = "Bflrtip",
                pageLength = 10,
                lengthMenu = c(10, 25, 50, 100),
                buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
            )
        )
    })
}

# Run the application
shinyApp(ui = ui, server = server)
