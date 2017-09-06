setGeneric(name='.addPaper', def=function(paper) {
    standardGeneric('.addPaper')
})

#'@include AllClasses.R
#'
setMethod(
    f='.addPaper',
    signature=c('Paper'),
    definition=function(paper) {
        # papers <- papers[!is.na(papers)];
        # stopifnot(all(unlist(lapply(papers, is, class2='Paper'))));

        papersManager <- .pkgGlobalEnv$papersManager;
        actPapers <- papersManager@papers;
        actPapers[[ paper@id ]] <- paper;

        newPapersManages <- PapersManager(papers=actPapers);
        assign('papersManager', newPapersManages, envir=.pkgGlobalEnv);

        return(NA);
    }
)

setGeneric(name='.getPapers', def=function(paperIds) {
    standardGeneric('.getPapers')
})

setMethod(
    f='.getPapers',
    signature=c('character'),
    definition=function(paperIds) {
        papersManager <- .pkgGlobalEnv$papersManager;
        papers <- papersManager@papers;
        return(papers[ paperIds ]);
    }
)
