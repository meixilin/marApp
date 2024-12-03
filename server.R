

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
    observeEvent(input$go2, {
        # Generate SFS and expected SFS
        sfslist <- reactive({
            AC <- mar:::.get_AC(gm()$geno)
            N <- length(gm()$maps$sample.id)
            genosfs <- mar::sfs(AC = AC, N = N, ploidy = 2, folded = TRUE)
            neutralsfs <- mar::expsfs(lenAC = length(AC), N = N, ploidy = 2, folded = TRUE)
            allsfs <- list(genosfs, neutralsfs)
            names(allsfs) <- c("data", "neutral")
            return(allsfs)
        })

        sadlist <- reactive({
            if (!is.null(input$sad_model)) {
                marsad <- mar:::MARsad(gm = gm(), sad_models = input$sad_model, folded = TRUE)
            } else {
                marsad <- list(AICtabs = NULL, sadsfss = NULL)
            }
            return(marsad)
        })

        # logLik comparisons
        statdf <- reactive({
            allsfs <- c(sfslist(), sadlist()$sadsfss)
            # compare by logLik
            ll_list <- sapply(allsfs, function(model) ll_sfs (model = model, data = genosfs))
            statdf <- data.frame(model = names(allsfs),
                                 logLik = unname(ll_list),
                                 stringsAsFactors = FALSE)
            return(statdf)
        })

        # outputs
        output$print_AICtabs <- DT::renderDataTable({
            if (!is.null(sadlist()$AICtabs)) {
                DT::datatable(as.data.frame(sadlist()$AICtabs)) %>%
                    DT::formatRound(., c("logLik","AIC","dLogLik","dAIC"))
            }
        })

        output$print_statdf <- DT::renderDataTable({
            DT::datatable(statdf() %>% dplyr::arrange(desc(logLik))) %>%
                DT::formatRound(., "logLik")
        })

        # plot sfs
        output$plot_sfsdf <- renderPlotly({
            forplot <- mar:::.sfsl2df(c(sfslist(), sadlist()$sadsfss)) %>%
                reshape2::melt(id.vars = 'AC',
                               variable.name = 'model')
            pp <- ggplot(data = forplot, mapping = aes(x = AC, y = value, fill = model)) +
                geom_col(position = 'dodge') +
                labs(x = 'Minor Allele Count', y = 'Number of Alleles')
            ggplotly(pp)
        })
    })

    #######################################################################
    # Calculate MAR when the button is pressed
    observeEvent(input$go3, {
        # Generate MAR data
        mardf <- reactive({
            # load data in mar package
            withProgress(message = 'Calculating MAR ...', {
                MARsampling(gm = gm(),
                            scheme = input$scheme,
                            nrep = input$nrep,
                            xfrac = 0.01)
            })
        })

        # Build MAR
        marres <- reactive({
            mars <- lapply(input$Mtype, function(x) mar::MARcalc(mardf(), Mtype = x, Atype = input$Atype))
            names(mars) <- input$Mtype
            marsuml <- lapply(mars, .marsummary)
            obj <- do.call(rbind, lapply(marsuml, as.data.frame, stringsAsFactors = FALSE))
            return(obj)
        })

        # Print MAR results
        output$print_marres <- DT::renderDataTable({
            DT::datatable(marres()) %>%
                DT::formatRound(., c("c", "z", "R2_adj")) %>%
                DT::formatSignif(., c("c_p", "z_p"))
        })

        # Plot MAR results
        output$plot_mardf <- renderPlotly({
            forplot = mardf()[,c(input$Atype, input$Mtype_plot)] %>% na.omit()
            c = marres()[rownames(marres()) == input$Mtype_plot,'c']
            z = marres()[rownames(marres()) == input$Mtype_plot,'z']
            # make predictions table (since stat_function does not work)
            preddf <- data.frame(x = sort(unique(forplot[,1]))) %>%
                dplyr::mutate(y = c * x^z)
            colnames(preddf) <- colnames(forplot)
            pp <- ggplot(data = forplot,
                         mapping = aes_string(x = input$Atype, y = input$Mtype_plot)) +
                geom_point(size = 1, color = 'darkgreen') +
                geom_line(data = preddf, color = 'darkgray') +
                labs(x = get_name(Achoices, input$Atype), y = get_name(Mchoices, input$Mtype_plot))

            if (input$log_mar) {
                pp <- pp +
                    scale_x_log10() +
                    scale_y_log10()
            }
            ggplotly(pp)
        })

        # generate an output slider
        output$slider_mar <- renderUI({
            sliderInput(inputId = "a_mar",
                        label = "Sampling box size:",
                        min = 1,
                        max = nrow(mardf())/input$nrep,
                        step = 1,
                        value = 1,
                        animate = animationOptions(interval = 1000, loop = FALSE))
        })

        output$anim_mardf <- renderPlot({
            req(input$a_mar)
            # get the extdf
            bboxlist <- lapply(strsplit(mardf()$extent, ';'), as.integer)
            idx <- (input$a_mar - 1) * input$nrep + 1
            par(mar = c(5.1, 4.1, 4.1, 4.1))
            raster::plot(raster::extent(gm()$maps$samplemap), xlab = 'lon', ylab = 'lat')
            raster::plot(gm()$maps$samplemap, add = T, legend.mar = 3, legend.args = list(text = '# of genomes', side = 2))
            for (ii in idx:(idx+input$nrep-1)) {
                plot(mar:::.rowcol_extent(gm()$maps, bbox = bboxlist[[ii]]), col = 'black', add = T)
            }
        })
    })

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



