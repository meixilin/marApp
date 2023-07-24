function(input, output, session) {
    #######################################################################
    # renderUI for input selection
    output$coordsSelect <- renderUI({
        req(input$mode)
        if (input$mode == "Custom") {
            fileInput(inputId = "in_coords",
                      label = HTML("Three columns: ID, LON, LAT.<br/>(*.txt; *.tsv; *.gz)"),
                      placeholder = "Coordinates table.",
                      accept = c(".txt", ".tsv", ".gz"))
        }
    })
    output$genomesSelect <- renderUI({
        req(input$mode)
        if (input$mode == "Custom") {
            fileInput(inputId = "in_genomes",
                      label = HTML("Rows are samples, <br/>columns are SNPs (0/1/2 coded).<br/>(*.txt; *.tsv; *.gz)"),
                      placeholder = "Genotype table.",
                      accept = c(".txt", ".tsv", ".gz"))
        }
    })

    #######################################################################
    # Set up reactive values for coords and genomes to be passed to other events
    values <- reactiveValues(cc = NULL, gg = NULL)

    #######################################################################
    # Load and print table only when action button pressed
    observeEvent(input$go1, {
        #######################################################################
        # Load coords and genomes
        coords <- reactive({
            if (input$mode == "Custom") {
                mypath = input$in_coords$datapath
                if (grepl(".tsv$|.txt$",mypath)) {
                    read_table(mypath)
                } else {
                    stop('Unsupported coordinate file formats.')
                }
            } else {
                read_table('./data/demo_coords.tsv')
            }
        })

        genomes <- reactive({
            if (input$mode == "Custom") {
                mypath = input$in_genomes$datapath
                if (grepl(".tsv$|.txt$",mypath)) {
                    read_table(mypath)
                } else {
                    stop('Unsupported genotype file formats.')
                }
            } else {
                read_table('./data/demo_genomes.tsv.gz')
            }
        })
        # pass the function to values
        values$cc = coords; values$gg = genomes

        #######################################################################
        # Print data
        output$print_coords <- DT::renderDataTable({
            DT::datatable(coords()[1:20, ],
                          options = list(scrollX = TRUE))
        })
        output$print_genomes <- DT::renderDataTable({
            DT::datatable(genomes()[1:20,1:20],
                          options = list(scrollX = TRUE, fixedColumns = list(leftColumns = 2)),
                          extensions = "FixedColumns")
        })

        #######################################################################
        # Plot leaflet
        output$map_coords <- leaflet::renderLeaflet({
            leaflet() %>%
                addTiles() %>%
                addMarkers(data = coords(),
                           clusterOptions = markerClusterOptions(),
                           label = ~ID, lng = ~LON, lat = ~LAT)
        })
    })

    #######################################################################
    # Calculate MAR when the button is pressed
    observeEvent(input$go2, {
        incoord <- as.matrix(values$cc())
        ingeno <- as.matrix(values$gg()[,-1])
        # Generate MAR data
        mardf <- reactive({
            # load data in mar package
            withProgress(message = 'Calculating MAR ...', {
                mar::MARfastGT(coord = incoord,
                               geno = ingeno,
                               res = 0.01)
            })
        })
        output$print_mardf <- DT::renderDataTable({
            DT::datatable(mardf() %>% dplyr::arrange(A_sq),
                          options = list(scrollX = TRUE)) %>%
                DT::formatRound(., "A_sq", digits = 2)
        })
        # Plot MAR results
        output$plot_mardf <- renderPlotly({
            pp <- ggplot(data = mardf(), mapping = aes(x = A_sq, y = M)) +
                geom_point(size = 2, color = 'darkgreen') +
                labs(x = 'Area sampled', y = 'Number of mutations')
            if (input$log_mar) {
                pp <- pp +
                    scale_x_log10() + scale_y_log10()
            }
            ggplotly(pp)
        })

        # Calculate MAR results
        output$calc_mardf <- renderPrint({
            sar_power(mardf()[,c('A_sq','M')])
        })

        # Switch to the MAR tab when the button is clicked
        if (input$current_tab != "Mutations-area relationship") {
            updateTabItems(session, "current_tab", "Mutations-area relationship")
        }


    })
}
