library(pmcERR);
rm(list=ls());
options('verbose'=TRUE);

recalcRelevance <- function(relations) {
    newRelations <- lapply(relations, function(actRelations) {
#         actRelations <- relations[[1]];
        message('*************************');
        message(actRelations@relevance);
        actRelations@relevance <- numeric();
        
        actRelations@relatedPapers <- lapply(actRelations@relatedPapers, function(actRelation) {
#             actRelation <- actRelations@relatedPapers[[1]];
            message(actRelation@relevance);
            actRelation@relevance <- list();
            actRelation <- pmcERR:::getRelevance(actRelation);
            message('---------');
            message(actRelation@relevance);
            return(actRelation);
        })
        
        actRelations <- pmcERR:::getRelevance(actRelations);
        message(actRelations@relevance);
        return(actRelations);
    })
    return(newRelations);
}

load('~/Downloads/pmcERRRes-2017-10-19(2)_2.RData');
message(relations[[1]]@relevance);
relations <- suppressMessages(recalcRelevance(relations));
message(relations[[1]]@relevance);
# save(pprDldrs=pprDldrs, relations=relations, file='~/Downloads/pmcERRRes-2017-10-19(2)_2.RData')
