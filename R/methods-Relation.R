setGeneric(name='getRelevance', def=function(relation) {
    standardGeneric('getRelevance')
})

#'@include AllClasses.R
#'
setMethod(
    f='getRelevance',
    signature=c('Relation'),
    definition=function(relation) {
        relation@relevance <- length(relation@relatedPapers);

        return(relation);
    }
)

## todo:
# relative distance between entities

