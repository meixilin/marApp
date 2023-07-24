# ui for marApp

header <- dashboardHeader(
    title = "MAR explorer",
    titleWidth = sidewidth
)

sidebar <- dashboardSidebar(
    width = sidewidth,
    h4("Input validation"),
    radioButtons("mode", label = "Run custom dataset or demo?",
                 choices = c("Demo", "Custom"), selected = "Demo"),
    uiOutput("coordsSelect"),
    uiOutput("genomesSelect"),
    actionButton("go1", "Load data", width = 120),
    hr(),
    # start MAR calculation
    h4("Mutations-area relationship"),
    checkboxInput("log_mar", "Plot MAR on log scale", value = FALSE),
    actionButton("go2", "Calculate MAR", width = 120),
    hr()
)

body <- dashboardBody(
    # wrap the tabBox in fluidRow so the Body does not overflow
    fluidRow(
        tabBox(
            title = NULL, width = 12, id = "current_tab",
            tabPanel(
                "Input validation",
                fluidRow(
                    box(
                        width = 6, height = 600,
                        title = "Coordinate file preview",
                        DT::dataTableOutput("print_coords")
                    ),
                    box(
                        width = 6, height = 600,
                        title = "Genotype file preview",
                        DT::dataTableOutput("print_genomes")
                    )
                ),
                fluidRow(
                    box(
                        width = 12,
                        title = "Sample map",
                        leafletOutput("map_coords")
                    )
                )
            ),
            tabPanel(
                "Mutations-area relationship",
                fluidRow(
                    box(
                        width = 6, height = 600,
                        title = "MAR table",
                        DT::dataTableOutput("print_mardf")
                    ),
                    box(
                        width = 6, height = 600,
                        title = "Mutations-area plot",
                        plotlyOutput("plot_mardf", height = "auto")
                    )
                ),
                fluidRow(
                    box(
                        width = 12,
                        withMathJax(includeMarkdown("docs/mar_explanation.md")),
                        verbatimTextOutput("calc_mardf")
                    )
                )
            )
        )
    )
)


dashboardPage(
    header,
    sidebar,
    body,
    skin = 'green'
)

