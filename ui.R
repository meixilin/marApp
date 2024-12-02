# ui for marApp

header <- dashboardHeader(
    title = "MAR explorer",
    titleWidth = sidewidth
)

sidebar <- dashboardSidebar(
    width = sidewidth,
    sidebarMenu(
        menuItem("Upload data", tabName = "upload"),
        menuItem("Calculate MAR", tabName = "mar"),
        menuItem("Simulate extinction", tabName = "extsim")
    )
)

body <- dashboardBody(
    tags$head(tags$style(type='text/css', ".slider-animate-button { font-size: 20pt !important; }")),
    chooseSliderSkin("Flat", color = "green"),
    tabItems(
        # First tab content
        tabItem(
            tabName = "upload",
            fluidRow(
                box(
                    width = 12, collapsible = TRUE,
                    title = "Input validation", status = "info",
                    h5('Demo data takes ~3s to load. Please be patient while the sample data is loading.'),
                    radioButtons("mode", label = "Run custom dataset or demo?",
                                 choices = c("Demo", "Custom"), selected = "Demo"),
                    uiOutput("uploadNotes")
                )
            ),
            conditionalPanel(
                condition = "input.mode == 'Demo' | input.go1",
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
            )
        ),
        # Second tab content
        tabItem(
            tabName = "mar",
            fluidRow(
                box(
                    width = 12, collapsible = TRUE,
                    title = "Mutations-area relationship (MAR) options", status = "info",
                    sliderInput("nrep", label = "Number of replicates", value = 5, min = 5, max = 50, step = 5),
                    checkboxInput("log_mar", "Plot MAR on log scale", value = FALSE),
                    actionButton("go2", "Calculate MAR", width = 120)
                )
            ),
            fluidRow(
                box(title = "Calculating the mutations-area relationship",
                    width = 12,
                    withMathJax(includeMarkdown("docs/mar_explanation.md")),
                    verbatimTextOutput("calc_mardf")
                )
            ),
            fluidRow(
                box(
                    width = 6, height = '600px',
                    title = "Mutations-area example table",
                    DT::dataTableOutput("print_mardf")
                ),
                box(
                    width = 6, height = '600px',
                    title = "Mutations-area example plot",
                    plotlyOutput("plot_mardf", height = "auto")
                )
            )
        ),
        tabItem(
            tabName =  'extsim',
            conditionalPanel(
                condition = "input.mode == 'Demo'",
                fluidRow(
                    box(
                        width = 12, collapsible = TRUE,
                        title = "Extinction simulation options", status = "info",
                        sliderInput("a_ext", label = "(Apprx.) percent of area extincted",
                                    value = 0, min = 0, max = 100, step = extstep,
                                    animate = animationOptions(interval = 200, loop = FALSE))
                    )
                ),
                fluidRow(
                    box(
                        width = 12,
                        title = "Extinction map",
                        plotOutput("map_ext")
                    )
                ),
                fluidRow(
                    box(
                        width = 6, height = '500px',
                        title = "Extinction simulation table",
                        div(style='height:400px; overflow-y: scroll',
                            tableOutput('print_extdf'))

                    ),
                    box(
                        width = 6, height = '500px',
                        title = "Extinction simulation plot",
                        plotOutput("plot_extdf")
                    )
                )
            ),
            conditionalPanel(
                condition = "input.mode == 'Custom'",
                h5('Extinction simulation for custom dataset currently under development.
                   Please check back later.
                   Go to `Upload data` tab and select `Demo` to play the demo animation.')
            )
        )
    )
)

tagList(
    dashboardPage(
        header = header,
        sidebar = sidebar,
        body = body,
        skin = "green"),
    tags$footer(
        "© 2023 MOI LAB",
        style = "width:300px; padding:10px; background-color: #222D32; color: white"
    )
)

# align = "center", style = "
#               position:absolute;
#               bottom:0;
#               width:100%;
#               height:50px;   /* Height of the footer */
#               color: white;
#               padding: 10px;
#               background-color: black;
#               z-index: 1000;"


