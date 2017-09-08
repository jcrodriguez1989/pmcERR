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
#'
setMethod(
    f='getRelations',
    signature=c('list', 'character'),
    definition=function(papers, entities) {
        stopifnot(all(unlist(lapply(papers, is, class='Paper'))));
        # todo: check that entities is list of strings

        # Relations(entities='list', relatedPapers='list');
        relatedSents <- lapply(papers, function(actPaper) {
            actSents <- getSentences(actPaper);
            related <- unlist(lapply(actSents, function(actSent) {
                isRelated <- all(unlist(lapply(entities, grepl, actSent,
                                               ignore.case=TRUE)));
                return(isRelated);
            }))
            actSents[related];
        })

        relatedOnes <- lapply(relatedSents, length) > 0;
        relatedPapers <- papers[relatedOnes];
        relatedSents <- relatedSents[relatedOnes];

        relations <- lapply(seq_along(relatedPapers), function(i) {
            Relation(
                paper=relatedPapers[[i]],
                relatedSents=relatedSents[[i]])
        })

        res <- Relations(
            entities=entities,
            relatedPapers=relations);

        return(res);
    }
)
