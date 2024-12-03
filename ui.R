# ui for marApp

header <- dashboardHeader(
    title = "marApp",
    titleWidth = sidewidth
)

sidebar <- dashboardSidebar(
    width = sidewidth,
    sidebarMenu(
        menuItem("Upload data", tabName = "data"),
        menuItem("Site frequency spectrum", tabName = "sfs"),
        menuItem("Mutations-area relationship", tabName = "mar")
        # menuItem("Extinction simulation", tabName = "ext")
    )
)

body <- dashboardBody(
    tags$head(tags$style(type='text/css', ".slider-animate-button { font-size: 20pt !important; }")),
    chooseSliderSkin("Flat", color = "green"),
    tabItems(
        # First tab content
        tabItem(
            tabName = "data",
            fluidRow(
                box(
                    width = 12, collapsible = TRUE,
                    title = "Input validation", status = "info",
                    radioButtons("mode", label = "Run custom dataset or demo?",
                                 choices = c("Demo", "Custom"), selected = "Demo"),
                    uiOutput("uploadNotes"),
                    actionButton("go1", "Load data", width = 120)
                )
            ),
            conditionalPanel(
                condition = "input.go1",
                fluidRow(
                    box(
                        width = 12,
                        title = "Coordinate file preview",
                        verbatimTextOutput("print_mapsdata")
                    ),
                ),
                fluidRow(
                    box(
                        width = 12,
                        title = "Genotype file preview",
                        verbatimTextOutput("print_genodata")
                    )
                ),
                fluidRow(
                    box(
                        width = 12,
                        title = "Genomaps object",
                        leafletOutput("map_genomaps"),
                        h5("`Sample Raster` layer shows number of samples in grouped cells.\n
                           Given Leaflet's automatic reprojection, some cells might be displaced."),
                        h5("`Sample Points` layer shows inputted sample geo-locations. \n
                           Sample IDs are available when selected."),
                    )
                )
            )
        ),
        # Second tab content
        tabItem(
            tabName = "sfs",
            fluidRow(
                box(
                    width = 12, collapsible = TRUE,
                    title = "SFS and SAD options", status = "info",
                    # add space as separator
                    selectInput(inputId = "sad_model",
                                label = "Select the species-abundance distribution (SAD) models to fit:",
                                choices = sadchoices,
                                selected = c("lnorm", "ls"),
                                multiple = TRUE),
                    actionButton("go2", "Compute SFS and fit SAD", width = 180),
                )
            ),
            fluidRow(
                box(title = "The site-frequency spectrum and species abundance distributions",
                    width = 12,
                    withMathJax(includeMarkdown("docs/sfs_explanation.md")),
                )
            ),
            conditionalPanel(
                condition = "input.go2",
                fluidRow(
                    box(
                        width = 6,
                        title = "Statistics for SAD fit",
                        bt("SAD model fitting table:"),
                        DT::dataTableOutput("print_AICtabs"),
                        bt("SFS-based log-likelihoods:"),
                        DT::dataTableOutput("print_statdf"),

                    ),
                    box(
                        width = 6,
                        title = "Site frequency spectrum",
                        plotlyOutput("plot_sfsdf")
                    )
                )
            )
        ),
        # Third tab content
        tabItem(
            tabName = "mar",
            fluidRow(
                box(
                    width = 12, collapsible = TRUE,
                    title = "Mutations-area relationship (MAR) options", status = "info",
                    selectInput(inputId = "scheme",
                                label = "Select the MARsampling scheme:",
                                choices = mar:::.MARsampling_schemes,
                                selected = "random"),
                    selectInput(inputId = "Mtype",
                                label = "Select the genetic diversity metrics: ",
                                choices = Mchoices,
                                selected = c("M", "thetapi"),
                                multiple = TRUE),
                    selectInput(inputId = "Atype",
                                label = "Select the area metrics: ",
                                choices = Achoices,
                                selected = "A"),
                    numericInput(inputId = "nrep",
                                 label = "Number of replicates:",
                                 value = 5,
                                 min = 1,
                                 max = 20),
                    actionButton("go3", "Calculate MAR/GDAR", width = 150)
                )
            ),
            fluidRow(
                box(title = "The mutations (genetic diversity) area relationship",
                    width = 12,
                    withMathJax(includeMarkdown("docs/mar_explanation.md"))
                )
            ),
            conditionalPanel(
                condition = "input.go3",
                fluidRow(
                    box(title = "Summary of MAR/GDAR",
                        width = 12,
                        DT::dataTableOutput("print_marres"),

                    )
                ),
                fluidRow(
                    box(
                        width = 6,
                        title = "MAR sampling process",
                        uiOutput("slider_mar"),
                        plotOutput("anim_mardf")
                    ),
                    box(
                        width = 6,
                        title = "MAR/GDAR plots",
                        checkboxInput("log_mar", "Plot MAR/GDAR on log scale", value = FALSE),
                        selectInput("Mtype_plot", "Select the genetic diversity metrics to plot:", choices = Mchoices, selected = 'M'),
                        plotlyOutput("plot_mardf", height = "auto")
                    )
                )
            )
        )
        # tabItem(
        #     tabName =  "ext",
        #     conditionalPanel(
        #         condition = "input.mode == 'Demo'",
        #         fluidRow(
        #             box(
        #                 width = 12, collapsible = TRUE,
        #                 title = "Extinction simulation options", status = "info",
        #                 sliderInput("a_ext", label = "(Apprx.) percent of area extincted",
        #                             value = 0, min = 0, max = 100, step = extstep,
        #                             animate = animationOptions(interval = 200, loop = FALSE))
        #             )
        #         ),
        #         fluidRow(
        #             box(
        #                 width = 12,
        #                 title = "Extinction map",
        #                 plotOutput("map_ext")
        #             )
        #         ),
        #         fluidRow(
        #             box(
        #                 width = 6, height = '500px',
        #                 title = "Extinction simulation table",
        #                 div(style='height:400px; overflow-y: scroll',
        #                     tableOutput('print_extdf'))

        #             ),
        #             box(
        #                 width = 6, height = '500px',
        #                 title = "Extinction simulation plot",
        #                 plotOutput("plot_extdf")
        #             )
        #         )
        #     ),
        #     conditionalPanel(
        #         condition = "input.mode == 'Custom'",
        #         h5('Extinction simulation for custom dataset currently under development.
        #            Please check back later.
        #            Go to `Upload data` tab and select `Demo` to play the demo animation.')
        #     )
        # )
    )
)

tagList(
    dashboardPage(
        header = header,
        sidebar = sidebar,
        body = body,
        skin = "green"),
    tags$footer(
        "© 2024 MOI LAB",
        style = "width:300px; padding:10px; background-color: #222D32; color: white"
    )
)
