#'@exportMethod getRelations
#'
setGeneric(name='getRelations', def=function(papers, entities) {
    standardGeneric('getRelations')
})

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
                isRelated <- all(unlist(lapply(entities, grepl, actSent, ignore.case=TRUE)));
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
