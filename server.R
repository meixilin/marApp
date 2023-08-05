

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
    values <- reactiveValues(cc = NULL, gg = NULL, ggsub = NULL, M_all = NULL, extinds = NULL,
                             rr = NULL, rrext = NULL, notna = NULL, r2i = NULL, aext = NULL, mext = 0)

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
        values$ggsub = samp_geno(as.matrix(genomes()[,-1]), nsnps)
        values$ggsubext = values$ggsub
        values$M_all = calc_M(values$ggsub)
        values$rr = coord2raster(as.matrix(coords()[,-1]))
        values$rrext = values$rr
        values$notna = raster::cellStats(!is.na(values$rr), 'sum')
        values$r2i = raster::cellFromXY(values$rr, coords()[,-1])

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
                    incProgress(1/input$nrep)
                    # use seed to set representative run 1
                    if (ii == 1) set.seed(myseed)
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
    observeEvent(input$a_ext, {
        values$aext = c(values$aext, input$a_ext)
        # reset at 0%
        if (input$a_ext == 0) values$rrext <- values$rr
        # toextsize = the current habitat - the expected habitat with a_ext
        toextsize = cellStats(!is.na(values$rrext), 'sum') - values$notna*(100-input$a_ext)/100
        if (toextsize > 0) {
            # this cannot be in a function, because rrext need to be passed across different a_ext
            toext = sampleRandom(values$rrext, size = toextsize, cells = TRUE)[,'cell']
            values$rrext[toext] <- NA
            # get the individuals that are extincted in this cell
            toextinds = cell2inds(values$r2i, toext)
            values$extinds = c(values$extinds, toextinds)
            # calculate proportion lost
            values$mext = c(values$mext, extinct_geno(values))
        }

        output$map_ext <- renderPlot({
            validate(need(toextsize >= 0, "Cannot reverse extinction"))
            par(mar = c(4, 4, 4, 6))
            plot(NULL,
                 xlim = c(xmin(values$rr), xmax(values$rr)),
                 ylim = c(ymin(values$rr), ymax(values$rr)),
                 xlab = 'Longitude', ylab = 'Latitude',
                 main = paste0(input$a_ext, '% area extinct'))
            maps::map("world", col = "lightgray", border = "lightgray", fill = TRUE, boundary = FALSE, bg = "white",
                      add = TRUE)
            plot(values$rrext, col = bpy.colors(255), add = TRUE, legend.args = list(text = '# of genomes', side = 2))
        })

        extdf = data.frame(a_ext = values$aext,
                           m_ext = values$mext)

        print(extdf)
        plot(extdf$a_ext, extdf$m_ext)
    })
}



