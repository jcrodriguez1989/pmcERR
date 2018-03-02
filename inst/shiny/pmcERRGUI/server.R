library(shiny);

# shiny max file upload size to 30MB
options(shiny.maxRequestSize=30*1024^2);

shinyServer(function(input, output, session) {
    pprDldrs <- list(); # global variable for PaperDownloader objects
    relations <- list();

    prepareUI();

    output$downloadResLink <- downloadHandler(
        filename=function() { paste('pmcERRRes-', Sys.Date(), '.RData', sep='') },
        content=function(file) { save(pprDldrs=pprDldrs, relations=relations, file=file) }
    )

    observeEvent(input$entitiesInputFile, {
        print('Input file loaded.')
        filePath <- input$entitiesInputFile$datapath;
        fileText <- paste(readLines(filePath), collapse="\n");

        # update text area with file content
        updateTextAreaInput(session, "entitiesInput", value=fileText)
    })

    observeEvent(input$searchbutton, {
        disablebuttons();
        print('Search button clicked.');
        # change tab and download paper ids
        entitiesInput <- input$entitiesInput;
        sepInput <- input$sepInput;

        if (entitiesInput == '' | length(entitiesInput) == 0) {
            showNotification('Invalid entities.', type='error');
        } else if (sepInput == '' | length(sepInput) == 0) {
            showNotification('Invalid separator character.', type='error');
        } else {
            entitiesInput <- strsplit(entitiesInput, '\n', fixed=TRUE)[[1]];
            entitiesList <- strsplit(entitiesInput, sepInput, fixed=TRUE);
            entitiesList <- lapply(entitiesList, trimws);

            progress <- Progress$new(session, min=1, max=length(entitiesList));
            on.exit(progress$close());
            progress$set(message='Downloading paper IDs',
                         detail='This may take a while...');

            pprDldrs <<- lapply(seq_along(entitiesList), function(i) {
                entities <- entitiesList[[i]];
                progress$set(value=i);
                # pprDldr <- PaperDownloader(entities=entities, papersDir='./papers');
                pprDldr <- PaperDownloader(entities=entities, papersDir=input$papersDir);
                # pprDldr <- PaperDownloader(entities=entities, papersDir='/home/jcrodriguez/mytmp/papers/'); # todo: delete
                pprDldr <- getPapersIds(pprDldr, input$exactMatchInput);
            })

            outtable <- cbind(Entities=entitiesInput,
                '#Papers'=unlist(lapply(pprDldrs, function(pprDldr) length(pprDldr@paperIds))));
            updateDownloadPage(outtable, output);
            updateTabsetPanel(session, 'maintabset', selected='Download')
        }
        enablebuttons();
    })

    observeEvent(input$downloadbutton, {
        disablebuttons();
        print('Download button clicked.');
        maxPapers <- as.numeric(input$maxPapersInput);
        if (is.na(maxPapers)) {
            maxPapers <- -1;
        }

        if (maxPapers >= 0) message('Max papers: ', maxPapers);

        ignoreEnts <- as.numeric(input$ignoreEntsInput);
        if (is.na(ignoreEnts)) {
            ignoreEnts <- Inf;
        }

        progress <- Progress$new(session, min=1, max=length(pprDldrs));
        on.exit(progress$close());
        progress$set(message='Downloading papers',
                     detail='This may take a while...');

        rels <- lapply(seq_along(pprDldrs), function(i) {
            pprDldr <- pprDldrs[[i]];
            progress$set(value=i);

            if (length(pprDldr@paperIds) > ignoreEnts ||
                length(pprDldr@paperIds) == 0) {
                return(pmcERR:::Relations(entities=pprDldr@entities));
            }

            pprDldr <- downloadPapers(pprDldr, n=maxPapers);
            actPapers <- getPapers(pprDldr, n=maxPapers);
            getRelations(actPapers, pprDldr@entities);
        })

        relsRelevance <- unlist(lapply(rels, pmcERR:::getRelevance));
        rels <- rels[order(relsRelevance, decreasing=TRUE)];

        relations <<- rels;

        updateRelationsPage(relations, session);
        enablebuttons();
    })

    observeEvent(input$plotNetwbtn, {
        disablebuttons();
        print('Plot network button clicked.');

        entChoices <- input$entitySelInput;
        entChoices <- as.numeric(entChoices);

        actRelations <- relations[entChoices];

        ### create network nodes
        ## entities nodes
        entNodes <- do.call(rbind, lapply(actRelations, function(actRels) {
            actId <- paste(actRels@entities, collapse=paste('', input$sepInput, ''));
            actGroup <- paste(actRels@entities, collapse=', ');
            actRelevance <- actRels@relevance;
            c(id=actId, label=actId, title='', group=actGroup, value=actRelevance, shape='dot');
        }))
        # normalize values
        values <- as.numeric(entNodes[,'value']);
        values <- (values - min(values)) / (max(values)-min(values))
        entNodes[,'value'] <- as.character(values);

        ## paper nodes
        paperNodes <- do.call(rbind, lapply(actRelations, function(actRels) {
            # actRels <- actRelations[[1]];
            do.call(rbind, lapply(actRels@relatedPapers, function(actRel) {
                # actRel <- actRels@relatedPapers[[1]];
                actPaper <- actRel@paper;
                actId <- actPaper@id;
                actLink <- paste('https://www.ncbi.nlm.nih.gov/pmc/articles/PMC', actPaper@id, sep='');
                acttitle <- paste0(actPaper@title, '<br><a href="', actLink, '" target="_blank">Link</a>');

                actRelevance <- (actRel@relevance)$relevance;

                c(id=actId, label='', title=acttitle, group='', value=actRelevance, shape='square');
            }))
        }))

        # merge repeated papers
        paperIdstable <- table(paperNodes[,'id']);
        finalPaperNodes <- paperNodes[paperNodes[,'id'] %in% names(paperIdstable)[paperIdstable == 1], ]; # get unique papers
        repeatedPapersIds <- names(paperIdstable)[paperIdstable > 1];
        mergedPapers <- do.call(rbind, lapply(repeatedPapersIds, function(actPaperId) {
            actPaperNodes <- paperNodes[paperNodes[,'id'] == actPaperId,];
            actRes <- actPaperNodes[1,];
            actRes['value'] <- sum(as.numeric(actPaperNodes[,'value'])); # sum relevances of repeated papers
            return(actRes);
        }));
        finalPaperNodes <- rbind(finalPaperNodes, mergedPapers);
        # normalize values
        values <- as.numeric(finalPaperNodes[,'value']);
        values <- (values - min(values)) / (max(values)-min(values))
        finalPaperNodes[,'value'] <- as.character(values);

        stopifnot(all.equal(colnames(entNodes), colnames(finalPaperNodes)));
        nodes <- rbind(entNodes, finalPaperNodes);
        nodes <- data.frame(nodes);
        nodes$value <- as.numeric(as.character(nodes$value));

        ### create network edges
        ## entities <-> papers edges
        entPaperEdges <- do.call(rbind, lapply(actRelations, function(actRels) {
            # actRels <- actRelations[[1]];
            do.call(rbind, lapply(actRels@relatedPapers, function(actRel) {
                # actRel <- actRels@relatedPapers[[1]];
                actPaper <- actRel@paper;
                actPaperId <- actPaper@id;
                actEntId <- paste(actRel@entities, collapse=paste('', input$sepInput, ''));

                actRelevance <- (actRel@relevance)$relevance;

                c(from=actEntId, to=actPaperId, value=actRelevance, color='black');
            }))
        }))
        # normalize values
        values <- as.numeric(entPaperEdges[,'value']);
        values <- (values - min(values)) / (max(values)-min(values))
        entPaperEdges[,'value'] <- as.character(values);

        ## entities <-> entities edges
        # combns <- combn(seq_along(actRelations), 2);
        # entEntEdges <- do.call(rbind, lapply(seq_len(ncol(combns)), function(i) {
        #     ents1 <- actRelations[[ combns[1,i] ]]@entities;
        #     ents2 <- actRelations[[ combns[2,i] ]]@entities;
        #
        #     entsInters <- intersect(ents1, ents2);
        #     if (length(entsInters) == 0) return(NA);
        #
        #     entsUnion <- union(ents1, ents2);
        #
        #     actFrom <- paste(ents1, collapse=paste('', input$sepInput, ''));
        #     actto   <- paste(ents2, collapse=paste('', input$sepInput, ''));
        #     actRelevance <- length(entsInters) / length(entsUnion);
        #
        #     c(from=actFrom, to=actto, value=actRelevance, color='black');
        # }));
        # entEntEdges <- entEntEdges[rowSums(is.na(entEntEdges)) == 0,,drop=FALSE];

        # stopifnot(all.equal(colnames(entPaperEdges), colnames(entEntEdges)));
        # edges <- rbind(entPaperEdges, entEntEdges);
        # edges <- data.frame(edges);
        edges <- data.frame(entPaperEdges)
        edges$value <- as.numeric(as.character(edges$value));

        output$networkPlot <- renderVisNetwork({
            visNetwork(nodes, edges, width="100%") %>%
            #     visIgraphLayout() %>%
                visOptions(selectedBy=list(variable="group", multiple=TRUE))
            # %>%
            #     visLegend()
        })

        updateTabsetPanel(session, 'maintabset', selected='Network');

        enablebuttons();
    })

    observeEvent(input$entitySelInput, {
    # observeEvent(input$relsFilterbutton, {
        # print('Filter button clicked.');
        entChoices <- input$entitySelInput;

        updatePapersSelector(relations, entChoices, session);
    })

    observeEvent(input$paperSelInput, {
        # print('PaperSelInput event.');
        selectedPaper <- input$paperSelInput;
        showRelation(relations, selectedPaper, output, session);
    })

    observeEvent(input$loadResInputFile, {
        print('Input results file loaded.')
        filePath <- input$loadResInputFile$datapath;
        env <- new.env();
        load(filePath, env);

        if (!all(c('pprDldrs', 'relations') %in% names(env))) {
            showNotification('Could not load file.', type='error');
        } else {
            pprDldrs <<- env$pprDldrs;
            relations <<- env$relations;
        }

        outtable <- do.call(rbind, lapply(pprDldrs, function(x) {
            c(Entities=paste(x@entities, collapse=' | '),
              '#Papers'=length(x@paperIds));
        }))

        updateNewSearchPage(outtable[,'Entities'], session);
        updateDownloadPage(outtable, output);
        updateRelationsPage(relations, session);
    })
})

updatePapersSelector <- function(relations, entChoices, session) {
    # paperChoices selections will be 'ENTINDEX-PAPERINDEX'
    entChoices <- as.numeric(entChoices);

    paperChoices <- do.call(rbind, lapply(entChoices, function(i) {
        actRel <- relations[[i]];
        if (length(actRel@relatedPapers) == 0) return(NA);

        actRelPapers <- actRel@relatedPapers;
        relevances <- unlist(lapply(actRelPapers, function(x) {
            pmcERR:::getRelevance(x)$relevance
        }));

        res <- cbind(
            intId=paste(i, seq_along(actRelPapers), sep='-'),
            relevances);
        rownames(res) <- unlist(lapply(actRelPapers, function(x) x@paper@id));
        return(res);
    }))

    paperChoices <- paperChoices[rowSums(is.na(paperChoices)) == 0,,drop=FALSE];

    if (nrow(paperChoices) > 0) {
        paperChoices <- paperChoices[order(as.numeric(paperChoices[,2]), decreasing=TRUE),,drop=FALSE];
        choices <- paperChoices[,1];
        names(choices) <- rownames(paperChoices);
    } else {
        choices <- NULL;
    }

    if (!is.null(choices)) {
        choices <- choices[!is.na(choices)];
    } else {
        choices <- list();
    }

    updateSelectInput(session, 'paperSelInput', choices=choices);
}

showRelation <- function(relations, selectedPaper, output, session) {
    selectedPaper <- strsplit(selectedPaper, '-')[[1]];
    actRel <- relations[[as.numeric(selectedPaper[[1]])]];
    actPaperRel <- actRel@relatedPapers[[as.numeric(selectedPaper[[2]])]];
    actPaper <- actPaperRel@paper;
    actRelSents <- actPaperRel@relatedSents;

    updateTextInput(session, 'papertitle', value=actPaper@title);
    updateTextInput(session, 'paperDate', value=actPaper@date);

    # todo: function getLink(paper)
    actLink <- paste('https://www.ncbi.nlm.nih.gov/pmc/articles/PMC', actPaper@id, sep='');
    updateTextInput(session, 'paperLink', value=actLink);

    for (entity in actRel@entities) {
        actRelSents <- gsub(entity, paste('<B><i>', entity, '</i></B>', sep=''), actRelSents, ignore.case=TRUE);
    }

    actRelSents <- paste(actRelSents, collapse='<br><br><B>------------------------------</B><br><br>');
    output$paperOutput <- renderText({ actRelSents });
}

updateNewSearchPage <- function(entities, session) {
    updateTextAreaInput(session, 'entitiesInput', value=paste(entities, collapse='\n'));
}

updateDownloadPage <- function(outtable, output) {
    outtable <- as.data.frame(outtable);
    outtable[,2] <- as.numeric(as.character(outtable[,2]));
    output$outputdtable <- DT::renderDataTable(
        DT::datatable(outtable, extensions='Buttons',
                      options=list(
                          dom='Bfrtip',
                          buttons=c('copy', 'csv', 'excel'))
        ));
}

updateRelationsPage <- function(relations, session) {
    # entChoices selections will be the index
    entChoices <- seq_along(relations);
    names(entChoices) <- unlist(lapply(relations, function(x) paste(x@entities, collapse=' | ')));

    updatePapersSelector(relations, entChoices, session);

    updateSelectInput(session, 'entitySelInput', choices=entChoices, selected=entChoices);

    updateTabsetPanel(session, 'maintabset', selected='Relations');
}

prepareUI <- function() {
    disable('papertitle');
    disable('paperDate');
    disable('paperLink');
}

enablebuttons <- function() {
    enable('searchbutton');
    enable('downloadbutton');
    enable('downloadResLink');
    enable('plotNetwbtn');
    enable('loadResInputFile');
}

disablebuttons <- function() {
    disable('searchbutton');
    disable('downloadbutton');
    disable('downloadResLink');
    disable('plotNetwbtn');
    disable('loadResInputFile');
}
