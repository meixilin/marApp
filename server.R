

function(input, output, session) {
    #######################################################################
    # renderUI for inputting conservation decisions
    output$conInput <- renderUI({
        req(input$mode0)
        if (input$mode0 == "estimate loss") {
            numericInput("habitat_loss", "Proportion of habitat lost (0 to 1):",
                         value = 0.3, min = 0, max = 1, step = 0.01)
        } else{
            numericInput("gd_target", "Proportion of genetic diversity to protect (0 to 1):",
                         value = 0.9, min = 0, max = 1, step = 0.01)
        }
    })

    #######################################################################
    # Generate a report for genetic diversity conservation for download

    observeEvent(input$go0, {
        req(input$structure_slider, input$mode0)

        params <- list(
            mode = isolate(input$mode0),
            structure = isolate(input$structure_slider),
            aloss = isolate(input$habitat_loss),
            gtarg = isolate(input$gd_target)
        )

        out_html <- "report.html"
        rmarkdown::render(
            input = con_reports[input$mode0],
            output_file = out_html,
            params = params,
            envir = new.env(parent = globalenv())
        )

        output$reportUI <- renderUI({
            withMathJax(includeHTML(out_html))

        })

        output$downloadReport <- downloadHandler(
            filename = function() {
                paste0('Report_', Sys.Date(), '.html')
            },
            content = function(file) {
                file.copy(out_html, file)
            }
        )
    })

    #######################################################################
    # renderUI for Upload data input selection
    output$uploadNotes <- renderUI({
        req(input$mode)
        if (input$mode == "Custom") {
            tagList(
                includeMarkdown("docs/upload_req.md"),
                fileInput(inputId = "in_coords",
                          label = "Coordinate file",
                          accept = c(".txt", ".txt.gz", ".csv", ".csv.gz", ".tsv", ".tsv.gz")),
                fileInput(inputId = "in_genomes",
                          label = "Genotype file",
                          accept = c(".txt", ".txt.gz", ".tsv", ".tsv.gz", ".vcf", ".vcf.gz"))
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
        }
        return(obj)
    })

    genodata <- reactive({
        req(input$go1)
        if (input$mode == "Custom") {
            mypath = input$in_genomes$datapath
            if (grepl(".vcf", input$in_genomes$name)) {
                mydata = quiet(mar::vcf_parser(mypath, opengds = TRUE))
                gdsname = mydata$filename
                obj = mar:::.seqarray2margeno(mydata)
                SeqArray::seqClose(mydata)
                # remove the file after closing the gds file
                file.remove(gdsname)
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
            genosfs <- sfslist()$data
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
                            xfrac = 0.01,
                            quorum = TRUE)
            })
        })

        # Build MAR
        marres <- reactive({
            mars <- lapply(input$Mtype, function(x) mar::MARcalc(mardf(), Mtype = x, Atype = input$Atype))
            names(mars) <- input$Mtype
            marsuml <- lapply(mars, mar:::.marsummary)
            obj <- do.call(rbind, lapply(marsuml, as.data.frame, stringsAsFactors = FALSE))
            return(obj)
        })

        # Print MAR results
        output$print_marres <- DT::renderDataTable({
            DT::datatable(marres()) %>%
                DT::formatRound(., c("c", "z", "R2_adj")) %>%
                DT::formatSignif(., c("c_p", "z_p"))
        })

        # Option to download mardf()
        output$download_mardf <- downloadHandler(
              filename = function() {
                paste0('mardf_', Sys.Date(), '.csv')
              },
              content = function(file) {
                write.csv(mardf(), file)
              }
        )

        # Plot MAR results
        output$plot_mardf <- renderPlotly({
            forplot = mardf()[,c(input$Atype, input$Mtype_plot)]
            forplot = forplot[(forplot[,2] > 0 & !is.na(forplot[,2])), ]
            c = marres()[rownames(marres()) == input$Mtype_plot,'c']
            z = marres()[rownames(marres()) == input$Mtype_plot,'z']
            # make predictions table (since stat_function does not work)
            preddf <- data.frame(x = sort(unique(forplot[,1]))) %>%
                dplyr::mutate(y = c * x^z)
            colnames(preddf) <- colnames(forplot)
            pp <- ggplot(data = forplot,
                         mapping = aes(x = .data[[input$Atype]], y = .data[[input$Mtype_plot]])) +
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

    #######################################################################
    # Run extinction simulations (only if MAR has been calculated)
    observeEvent(input$go4, {
        # Generate EXT data
        extdf <- reactive({
            # load data in mar package
            withProgress(message = 'Simulating extinction ...', {
                MARextinction(gm = gm(),
                            scheme = input$scheme,
                            nrep = input$nrep,
                            xfrac = 0.01)
            })
        })

        # Build MAR based on EXT data
        extres <- reactive({
            mars <- lapply(input$Mtype, function(x) mar::MARcalc(extdf(), Mtype = x, Atype = input$Atype))
            names(mars) <- input$Mtype
            marsuml <- lapply(mars, mar:::.marsummary)
            obj <- do.call(rbind, lapply(marsuml, as.data.frame, stringsAsFactors = FALSE))
            return(obj)
        })

        # Print MAR results based on EXT data
        output$print_extres <- DT::renderDataTable({
            DT::datatable(extres()) %>%
                DT::formatRound(., c("c", "z", "R2_adj")) %>%
                DT::formatSignif(., c("c_p", "z_p"))
        })

        # Option to download extdf()
        output$download_extdf <- downloadHandler(
            filename = function() {
                paste0('extdf_', Sys.Date(), '.csv')
            },
            content = function(file) {
                write.csv(extdf(), file)
            }
        )

        # Plot EXT results (% lost vs % lost)
        output$plot_extdf <- renderPlotly({
            forplot = extdf()[,c(input$Atype, input$Mtype_plot, 'repid')] %>% na.omit()
            # generate percentages data
            forplot[,1] <- 1 - forplot[,1]/max(forplot[,1])
            forplot[,2] <- forplot[,2]/max(forplot[,2])
            # get c and z again from EXT output
            z = extres()[rownames(extres()) == input$Mtype_plot,'z']
            # make predictions table (since stat_function does not work)
            preddf <- data.frame(x = sort(unique(forplot[,1]))) %>%
                dplyr::mutate(y = (1-x)^z)
            colnames(preddf) <- colnames(forplot)[1:2]
            pp <- ggplot(data = forplot,
                         mapping = aes(x = .data[[input$Atype]], y = .data[[input$Mtype_plot]], color = .data[['repid']])) +
                geom_point(size = 1) +
                geom_line(data = preddf, color = 'darkgray') +
                scale_color_gradient(low = "lightgreen", high = "darkgreen") +
                scale_x_continuous(labels = scales::percent) +
                scale_y_continuous(labels = scales::percent) +
                labs(x = paste0("% of ", get_name(Achoices, input$Atype), " lost"),
                     y = paste0("% of ", get_name(Mchoices, input$Mtype_plot), " remained")) +
                theme(legend.position = 'none')
            ggplotly(pp)
        })

        # generate an output slider for extinction visualizations
        output$select_ext <- renderUI({
            numericInput(inputId = 'repid_ext', label = 'Select which simulations to animate:',
                         value = 1, min = 1, max = input$nrep, step = 1)
        })
        output$slider_ext <- renderUI({
            sliderInput(inputId = "a_ext",
                        label = "Extinction step:",
                        min = 1,
                        max = nrow(extdf())/input$nrep,
                        step = 1,
                        value = 1,
                        animate = animationOptions(interval = 1000, loop = FALSE))
        })

        output$anim_extdf <- renderPlot({
            req(input$a_ext)
            # get the given extdf()
            extdf0 <- extdf()[extdf()$repid == input$repid_ext, ]
            extl <- lapply(strsplit(extdf0$extl, ';'), as.integer)
            rr <- gm()$maps$samplemap; values(rr) <- NA
            par(mar = c(5.1, 4.1, 4.1, 4.1))
            raster::plot(raster::extent(gm()$maps$samplemap), xlab = 'lon', ylab = 'lat')
            raster::plot(gm()$maps$samplemap, add = T, legend.mar = 3, legend.args = list(text = '# of genomes', side = 2))
            rr[setdiff(gm()$maps$cellid, extl[[input$a_ext]])] <- 1
            raster::plot(rr, add = T, col = 'black', legend = FALSE)
        })
    })

    session$onSessionEnded(function() {
        tempfiles = c(list.files(pattern = "^mardf"), list.files(pattern = "^extdf"), list.files(pattern = 'report.html'),
                      list.files(pattern = '^Report.+html$'))
        if (length(tempfiles) > 0) {
            sapply(tempfiles, unlink)
        }
    })
}



