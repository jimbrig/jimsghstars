library(shiny)
library(fs)
library(DT)
library(qs)
library(stringr)
library(dplyr)
library(purrr)
library(shinycustomloader)
library(reactable)
library(formattable)
library(bslib)

dat_file <- fs::dir_ls("data") |> sort(.data, decreasing = TRUE) |> purrr::chuck(1L)
dat <- qs::qread(dat_file)

tbl_theme <- reactable::reactableTheme(
  color = "hsl(0, 0%, 90%)",
  backgroundColor = "hsl(0, 0%, 10%)",
  borderColor = "hsl(0, 0%, 18%)",
  stripedColor = "hsl(0, 0%, 13%)",
  headerStyle = list(
    `&:hover[aria-sort]` = list(
      backgroundColor = "hsl(0, 0%, 14%)"
    )
  ),
  tableBodyStyle = list(
    color = "hsl(0, 0%, 75%)"
  ),
  rowHighlightStyle = list(
    color = "hsl(0, 0%, 90%)",
    backgroundColor = "hsl(0, 0%, 14%)"
  ),
  selectStyle = list(
    backgroundColor = "hsl(0, 0%, 20%)"
  ),
  inputStyle = list(
    backgroundColor = "hsl(0, 0%, 10%)",
    borderColor = "hsl(0, 0%, 21%)",
    `&:hover, &:focus` = list(
      borderColor = "hsl(0, 0%, 30%)"
    )
  ),
  pageButtonHoverStyle = list(
    backgroundColor = "hsl(0, 0%, 20%)"
  ),
  pageButtonActiveStyle = list(
    backgroundColor = "hsl(0, 0%, 24%)"
  )
)

# dat$repo = paste0("<a href='", dat$url, "'>", dat$repo, "</a>")
# dat <- dplyr::select(dat, -url)

# Define UI for application that draws a histogram
ui <- shiny::fluidPage(

  theme = bslib::bs_theme(
    # bootswatch = "darkly"
    bootswatch = "cyborg"
  ),

  # Application title
  shiny::titlePanel("JIMBRIG'S GITHUB STARRED REPOS"),
  shiny::h5("View all starred repositories in text format from: ", shiny::tags$a("HERE", href = "https://jimbrig.github.io/jimsghstars"), "."),
  shiny::h5("View script used to collect data from: ", shiny::tags$a("HERE", href = "https://gist.github.com/75fd952bad479737dc7b32b6ec203652#file-get_github_stars-r"), "."),
  shiny::hr(),

  # Sidebar with a slider input for number of bins

  # Show a plot of the generated distribution
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::h3("Filters"),
      width = 3,
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
        multiple = TRUE,
        selectize = FALSE,
        size = 15
      )
    ),
    shiny::mainPanel(
      width = 9,
      reactable::reactableOutput("table") |>
        shinycustomloader::withLoader()
      # DT::DTOutput("table")
    )
  )

)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  data <- shiny::reactive({

    # browser()

    dat |>
      dplyr::filter(
        .data$last_updated >= input$last_updated[1],
        .data$last_updated <= input$last_updated[2],
        .data$created >= input$created[1],
        .data$created <= input$created[2],
        .data$stargazers >= input$stars,
        .data$language %in% input$language
      )

  })

  output$table <- reactable::renderReactable({ # DT::renderDT({

    out <- data()

    # browser()

    reactable::reactable(
      data = out,
      theme = tbl_theme,
      defaultColDef = reactable::colDef(
        align = "center"#,
        # minWidth = 100
      ),
      defaultSorted = c(
        "stargazers"
      ),
      columns = list(
        repo = reactable::colDef(
          name = "Repository",
          cell = function(value, index) {
            url <- out[index, "url"]
            if (nchar(url) < 3) return("")
            htmltools::tags$a(href = url, target = "_blank", as.character(value))
          }),
        description = reactable::colDef(
          name = "Description",
          cell = function(value) {
            stringr::str_to_title(value)
          }),
        last_updated = reactable::colDef(
          name = "Last Updated",
          defaultSortOrder = "desc"
        ),
        created = reactable::colDef(
          name = "Created At",
          defaultSortOrder = "desc"
        ),
        stargazers = reactable::colDef(
          name = "StarGazers",
          defaultSortOrder = "desc",
          cell = function(value) {
            formattable::comma(value, digits = 0)
          }
        ),
        language = reactable::colDef(
          name = "Language",
          cell = function(value) {
            stringr::str_to_title(value)
          }
        ),
        url = reactable::colDef(
          show = FALSE
        )
      ),
      filterable = TRUE,
      searchable = TRUE,
      resizable = TRUE,
      paginationType = "jump",
      showPageSizeOptions = TRUE,
      onClick = "expand",
      outlined = TRUE,
      bordered = TRUE,
      highlight = TRUE,
      striped = TRUE,
      wrap = TRUE,
      showSortable = TRUE,
      fullWidth = TRUE
    )

    # DT::datatable(
    #   out,
    #   escape = FALSE,
    #   rownames = FALSE,
    #   colnames = stringr::str_to_title(colnames(out)),
    #   caption = paste0(
    #     "Last updated on ",
    #     stringr::str_sub(basename(dat_file), 1, 10)
    #   ),
    #   extensions = c("Buttons"),
    #   options = list(
    #     dom = "Bflrtip",
    #     pageLength = 10,
    #     lengthMenu = c(10, 25, 50, 100),
    #     buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    #   )
    # )
  })

  shiny::observeEvent(list(
    input$last_updated,
    input$created,
    input$stars,
    input$language
  ), {
    reactable::updateReactable(
      "table"
    )
  })
}

# Run the application
shiny::shinyApp(ui = ui, server = server)
