

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
                          accept = c(".csv", ".txt", ".tsv", ".gz")),
                fileInput(inputId = "in_genomes",
                          label = "Genotype file",
                          accept = c(".txt", ".vcf.gz", "txt.gz"))
            )
        }
    })

    #######################################################################
    # Load and generate genomaps object. Use gm1001g as default.

    #######################################################################
    # Load and build genomaps object
    mapsdata <- reactive({
        req(input$go1)
        if (input$mode == "Custom") {
            mypath = input$in_coords$datapath
            mydata = mar::lonlat_parser(mypath)
            obj = mar::marmaps(mydata, mapres = NULL, mapcrs = mycrs)
        } else {
            obj = mar::gm1001g$maps
            # TODO: raster_3.5-15 cannot handle output from raster_3.6-20
            if (is.na(raster::crs(obj$samplemap)@projargs)) {
                raster::crs(obj$samplemap) = mycrs
            }
        }
        return(obj)
    })

    genodata <- reactive({
        req(input$go1)
        if (input$mode == "Custom") {
            mypath = input$in_genomes$datapath
            if (grepl(".vcf", input$in_genomes$name)) {
                mydata = quiet(mar::vcf_parser(mypath, opengds = TRUE))
                obj = mar:::.seqarray2margeno(mydata)
                SeqArray::seqClose(mydata)
            } else {
                obj = mar::text_parser(mypath)
            }
        } else {
            obj = mar::gm1001g$geno
        }
        return(obj)
    })

    # Build genomaps
    gm <- reactive({
        mar::genomaps(geno = genodata(),
                      maps = mapsdata())
    })

    #######################################################################
    # Print data
    output$print_mapsdata <- renderPrint({
        mar:::print.marmaps(mapsdata())
    })
    output$print_genodata <- renderPrint({
        mar:::print.margeno(genodata())
    })

    #######################################################################
    # Plot leaflet
    # TODO: leaflet automatically reprojects, so the raster is not exactly what used
    output$map_genomaps <- leaflet::renderLeaflet({
        # convert gm() to a data frame
        mapdf = cbind(
            data.frame(ID = gm()$maps$sample.id),
            as.data.frame(gm()$maps$lonlat)
        )
        leaflet() %>%
            addTiles() %>%
            addRasterImage(x = gm()$maps$samplemap,
                           group = "Sample Raster") %>%
            addCircleMarkers(data = mapdf,
                             clusterOptions = markerClusterOptions(),
                             popup = ~ID, label = ~ID, lng = ~LON, lat = ~LAT,
                             group = "Sample Points") %>%
            addLayersControl(
                overlayGroups = c("Sample Raster", "Sample Points"),
                options = layersControlOptions(collapsed = FALSE)
            )
    })

    #########################################################################
    # Plot site frequency spectrum


    # #######################################################################
    # # Calculate MAR when the button is pressed
    # observeEvent(input$go2, {
    #     incoord <- as.matrix(coords()[,-1])
    #     ingeno <- as.matrix(genomes()[,-1])
    #     # Generate MAR data
    #     mardf <- reactive({
    #         # load data in mar package
    #         withProgress(message = 'Calculating MAR ...', {
    #             mardflist <- lapply(1:input$nrep, function(ii){
    #                 incProgress(1/input$nrep)
    #                 # use seed to set representative run 1
    #                 if (ii == 1) set.seed(myseed)
    #                 ## Randomly subset genome matrix to 1000 SNPs
    #                 s_genome = samp_geno(ingeno, nsnps)
    #                 mardf <- MARfastGT(coord = incoord,
    #                                    geno = s_genome,
    #                                    res = 1) # the resolution should be 1
    #                 return(mardf)
    #             })

    #         })
    #     })
    #     output$print_mardf <- DT::renderDataTable({
    #         DT::datatable(mardf()[[1]] %>% dplyr::arrange(A_sq),
    #                       options = list(scrollX = TRUE)) %>%
    #             DT::formatRound(., "A_sq", digits = 2)
    #     })
    #     # Plot MAR results
    #     output$plot_mardf <- renderPlotly({
    #         pp <- ggplot(data = mardf()[[1]], mapping = aes(x = A_sq, y = M)) +
    #             geom_point(size = 1, color = 'darkgreen') +
    #             labs(x = 'Area sampled', y = 'Number of mutations')
    #         if (input$log_mar) {
    #             pp <- pp +
    #                 scale_x_log10() +
    #                 scale_y_log10() +
    #                 stat_smooth(method = "lm", formula = y ~ x, geom = "smooth", color = "darkgray")
    #         }
    #         ggplotly(pp)
    #     })

    #     # Calculate MAR results
    #     output$calc_mardf <- renderPrint({
    #         # print the first result
    #         cat("Z_mar output for the plotted replicate:\n")
    #         print(sar_power(mardf()[[1]][,c('A_sq','M')]))
    #         # print the average result
    #         cat("Summary of Z_mar in", input$nrep, "replicates\n")
    #         splist <- sapply(1:input$nrep, function(ii) {
    #             sp <- sar_power(mardf()[[ii]][,c('A_sq','M')])$sigConf[2,1]
    #         })
    #         summary(splist)
    #     })
    # })

    # #######################################################################
    # # Run extinction simulations (only if MAR has been calculated)
    # # pre-calculate each step's raster cells and individuals that are being extincted.
    # EXTdata <- reactive({
    #     MARfastEXT(coord = as.matrix(coords()[,-1]),
    #                geno = samp_geno(as.matrix(genomes()[,-1]), nsnps, myseed),
    #                rasterco = rasterco(),
    #                extstep = extstep)
    # })
    # # set up referenece lines
    # EXTref <- ext_ref()

    # # if input$a_ext changes, load the pre-calculated results
    # observeEvent(input$a_ext, {
    #     rr = rasterco()
    #     extcells = unlist(unname(EXTdata()$extcells[1:which(names(EXTdata()$extcells) == as.character(input$a_ext))]))
    #     extdf = EXTdata()$extdf %>%
    #         dplyr::filter(a_ext*100 <= input$a_ext)
    #     if (!is.null(extcells)) {rr[extcells] <- NA} # modify extinction area
    #     output$map_ext <- renderPlot({
    #         par(mar = c(4, 4, 4, 6))
    #         plot(NULL,
    #              xlim = c(xmin(rasterco()), xmax(rasterco())),
    #              ylim = c(ymin(rasterco()), ymax(rasterco())),
    #              xlab = 'Longitude', ylab = 'Latitude',
    #              main = paste0(input$a_ext, '% area extinct'))
    #         maps::map("world", col = "lightgray", border = "lightgray", fill = TRUE, boundary = FALSE, bg = "white",
    #                   add = TRUE)
    #         plot(rr, col = bpy.colors(255), add = TRUE, legend.args = list(text = '# of genomes', side = 2))
    #     })

    #     output$plot_extdf <- renderPlot({
    #         x = EXTref[EXTref$id=='mar 0.3', 'aa']
    #         y1 = EXTref[EXTref$id=='mar 0.3', 'dm']
    #         y2 = EXTref[EXTref$id=='mar 0.5', 'dm']
    #         y3 = EXTref[EXTref$id=='nomar 10000', 'dm']
    #         y4 = EXTref[EXTref$id=='nomar 1e+09', 'dm']
    #         plot(NULL,xlim = c(0,1), ylim = c(0,1), xlab = '% of area lost', ylab = '% of mutations lost')
    #         polygon(c(x, rev(x)), c(y2, rev(y1)), col = "darkseagreen1", border = NA)
    #         polygon(c(x, rev(x)), c(y3, rev(y4)), col = "lightgray", border = NA)
    #         points(extdf$a_ext, extdf$m_ext, col = 'darkgreen', pch = 19)
    #         legend('topleft',
    #                fill = c('darkgreen','darkseagreen1', 'lightgray'),
    #                legend = c('Simulation','MAR prediction', 'Individual prediction'))
    #     })

    #     output$print_extdf <- renderTable({
    #         extdf %>%
    #             dplyr::select(a_ext, m_ext) %>%
    #             dplyr::mutate_all(~sprintf("%.2f%%", . * 100))
    #     }, width = '200%')
    # })
}



