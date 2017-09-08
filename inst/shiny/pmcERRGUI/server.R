
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny);

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
                pprDldr <- PaperDownloader(entities=entities, papersDir='./papers');
                pprDldr <- getPapersIds(pprDldr);
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
        message('Max papers: ', maxPapers);
        
        progress <- Progress$new(session, min=1, max=length(pprDldrs));
        on.exit(progress$close());
        progress$set(message='Downloading papers',
                     detail='This may take a while...');
        
        relations <<- lapply(seq_along(pprDldrs), function(i) {
            pprDldr <- pprDldrs[[i]];
            progress$set(value=i);
            pprDldr <- downloadPapers(pprDldr, n=maxPapers);
            actPapers <- getPapers(pprDldr, n=maxPapers); length(actPapers)
            getRelations(actPapers, pprDldr@entities);
        })
        
        updateRelationsPage(relations, session);
        enablebuttons();
    })
    
    observeEvent(input$entitySelInput, {
    # observeEvent(input$relsFilterbutton, {
        # print('Filter button clicked.');
        entChoices <- input$entitySelInput;

        updatePapersSelector(relations, entChoices, session);
    })
    
    observeEvent(input$paperSelInput, {
        print('PaperSelInput event.');
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

    paperChoices <- unlist(lapply(entChoices, function(i) {
        actRel <- relations[[i]];
        if (length(actRel@relatedPapers) == 0) return(NA);
        
        actRelPapers <- actRel@relatedPapers;
        res <- paste(i, seq_along(actRelPapers), sep='-');
        names(res) <- unlist(lapply(actRelPapers, function(x) x@paper@id));
        return(res);
    }))
    
    if (!is.null(paperChoices)) {
        paperChoices <- paperChoices[!is.na(paperChoices)];
    } else {
        paperChoices <- list();
    }
    
    updateSelectInput(session, 'paperSelInput', choices=paperChoices);
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
    output$outputdtable <- renderDataTable(outtable);
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
    enable('loadResInputFile');
}

disablebuttons <- function() {
    disable('searchbutton');
    disable('downloadbutton');
    disable('downloadResLink');
    disable('loadResInputFile');
}
