#'Relations methods
#'
#'Methods to work with Relations.
#'\code{getRelations} gets a Relations. Contains the papers that relate the
#'given entities with the relating sentences.
#'
#'@param papers a list of Paper objects to search for relations.
#'@param entities a character vector of entities to query for.
#'
#'@return Relations objects.
#'
#'@docType methods
#'@name methods-Relations
#'@rdname methods-Relations
#'
#'@examples
#'## Create a PaperDownloader to query for two entities relations
#'entities <- c('terminal differentiation', 'basal-like');
#'pprDldr <- PaperDownloader(entities=entities, papersDir='/tmp/pmcERR/');
#'
#'## Get the number of papers possible to download, updating the pprDldr
#'pprDldr <- getPapersIds(pprDldr);
#'
#'## Download the first 3 papers
#'pprDldr <- downloadPapers(pprDldr, n=3);
#'
#'## Get the first 3 papers
#'papers <- getPapers(pprDldr, n=3);
#'
#'relations <- getRelations(papers, entities);
#'
setGeneric(name='methods-Relations', def=function(papers, entities) {
    standardGeneric('methods-Relations')
})

#'@name getRelations
#'@inheritParams methods-Relations
#'@rdname methods-Relations
#'@aliases getRelations,list,character
#'@exportMethod getRelations
#'
setGeneric(name='getRelations', def=function(papers, entities) {
    standardGeneric('getRelations')
})

#'@inheritParams methods-Relations
#'@rdname methods-Relations
#'@aliases getRelations,list,character
#'
#'@include AllClasses.R
#'@include methods-relevance.R
#'
setMethod(
    f='getRelations',
    signature=c('list', 'character'),
    definition=function(papers, entities) {
        stopifnot(all(unlist(lapply(papers, is, class='Paper'))));
        # todo: check that entities is list of strings

        lowEnts <- tolower(entities);
        # Relations(entities='list', relatedPapers='list');
        relatedSents <- lapply(papers, function(actPaper) {
            actSents <- getSentences(actPaper);
            lowActSents <- tolower(actSents);
            related <- unlist(lapply(lowActSents, function(actSent) {
                isRelated <- all(unlist(lapply(lowEnts, grepl, actSent,
                                               fixed=TRUE)));
                return(isRelated);
            }))
            actSents[related];
        })

        relatedOnes <- lapply(relatedSents, length) > 0;
        relatedPapers <- papers[relatedOnes];
        relatedSents <- relatedSents[relatedOnes];

        relations <- lapply(seq_along(relatedPapers), function(i) {
            actRel <- Relation(
                paper=relatedPapers[[i]],
                relatedSents=relatedSents[[i]],
                entities=entities)
            actRel <- getRelevance(actRel);
            stopifnot(is(actRel, 'Relation'));
            return(actRel);
        })

        res <- Relations(
            entities=entities,
            relatedPapers=relations);
        res <- getRelevance(res);
        stopifnot(is(res, 'Relations'));

        return(res);
    }
)
