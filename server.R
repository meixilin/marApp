

function(input, output, session) {
    #######################################################################
    # renderUI for input selection
    output$uploadNotes <- renderUI({
        req(input$mode)
        if (input$mode == "Custom") {
            tagList(
                includeMarkdown("docs/upload_req.md"),
                fileInput(inputId = "in_coords",
                          label = "Coordinate file",
                          accept = c(".txt", ".tsv", ".gz")),
                fileInput(inputId = "in_genomes",
                          label = "Genotype file",
                          accept = c(".txt", ".tsv", ".gz")),
                actionButton("go1", "Load data", width = 120)
            )
        }
    })

    #######################################################################
    # Set up reactive values for coords and genomes to be passed to other events
    values <- reactiveValues(cc = NULL, gg = NULL, rr = NULL, rrext = NULL, notna = NULL)

    #######################################################################
    # Load and print table demo by default. Load custom data when mode changed.
    observeEvent(input$mode, {
        #######################################################################
        # Load coords and genomes
        coords <- reactive({
            if (input$mode == "Custom") {
                req(input$go1)
                mypath = input$in_coords$datapath
                validater(mypath)
            } else {
                read_table('./data/demo_coords.tsv')
            }
        })

        genomes <- reactive({
            if (input$mode == "Custom") {
                req(input$go1)
                mypath = input$in_genomes$datapath
                validater(mypath)
            } else {
                read_table('./data/demo_genomes.tsv.gz')
            }
        })
        # pass the function to values
        values$cc = coords
        values$gg = genomes
        values$rr = coord2raster(as.matrix(coords()[,-1]))
        values$notna = cellStats(!is.na(values$rr), 'sum')
        # print(values$notna)

        #######################################################################
        # Print data
        output$print_coords <- DT::renderDataTable({
            DT::datatable(coords()[1:20, ],
                          options = list(scrollX = TRUE)) %>%
                formatRound(., columns = c('LON', 'LAT'))
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
        incoord <- as.matrix(values$cc()[,-1])
        ingeno <- as.matrix(values$gg()[,-1])
        # Generate MAR data
        mardf <- reactive({
            # load data in mar package
            withProgress(message = 'Calculating MAR ...', {
                mardflist <- lapply(1:input$nrep, function(ii){
                    # incProgress(1/input$nrep)
                    # use seed to set representative run 1
                    if (ii == 1) set.seed(123)
                    ## Randomly subset genome matrix to 1000 SNPs
                    s_genome = ingeno[, sample(1:ncol(ingeno), nsnps, F)]
                    mardf <- MARfastGT(coord = incoord,
                                       geno = s_genome,
                                       res = 1) # the resolution should be 1
                    return(mardf)
                })

            })
        })
        output$print_mardf <- DT::renderDataTable({
            DT::datatable(mardf()[[1]] %>% dplyr::arrange(A_sq),
                          options = list(scrollX = TRUE)) %>%
                DT::formatRound(., "A_sq", digits = 2)
        })
        # Plot MAR results
        output$plot_mardf <- renderPlotly({
            pp <- ggplot(data = mardf()[[1]], mapping = aes(x = A_sq, y = M)) +
                geom_point(size = 1, color = 'darkgreen') +
                labs(x = 'Area sampled', y = 'Number of mutations')
            if (input$log_mar) {
                pp <- pp +
                    scale_x_log10() +
                    scale_y_log10() +
                    stat_smooth(method = "lm", formula = y ~ x, geom = "smooth", color = "darkgray")
            }
            ggplotly(pp)
        })

        # Calculate MAR results
        output$calc_mardf <- renderPrint({
            # print the first result
            cat("Z_mar output for the plotted replicate:\n")
            print(sar_power(mardf()[[1]][,c('A_sq','M')]))
            # print the average result
            cat("Summary of Z_mar in", input$nrep, "replicates\n")
            splist <- sapply(1:input$nrep, function(ii) {
                sp <- sar_power(mardf()[[ii]][,c('A_sq','M')])$sigConf[2,1]
            })
            summary(splist)
        })

        # # Switch to the MAR tab when the button is clicked
        # if (input$current_tab != "mar") {
        #     updateTabItems(session, "current_tab", "mar")
        # }
    })

    #######################################################################
    # Run extinction simulations (only if MAR has been calculated)
    # Code inspiration: https://boazsobrado.com/blog/2018/02/08/leaflet-timeline-in-r/
    # Render the leaflet map
    output$map_ext <- leaflet::renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            addRasterImage(layerId = 'startcoord', values$rr, opacity = 0.1) %>%
            setView(lng = 30, lat = 49, zoom = 3)
    })

    observeEvent(input$a_ext, {
        print(input$a_ext)
        if (input$a_ext > 0) {
            toext = sampleRandom(values$rr, size = values$notna*extstep/100, cells = TRUE)[,'cell']
            print(toext)
            values$rr[toext] <- NA
        }
        leafletProxy("map_ext") %>%
            removeImage(layerId =  'endcoord') %>%
            addRasterImage(layerId = 'endcoord', values$rr, opacity = 0.8)
    })

}



