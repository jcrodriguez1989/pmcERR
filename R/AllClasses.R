# All class definitions in R/AllClasses.R
#'pmcERR S4 classes implementation in R
#'
#'PaperDownloader class is used to download papers given some entities.
#'
#'@slot entities character vector of entities.
#'@slot papers list of papers, will be filled when downloaded.
#'@slot papersDir character indicating the path to locally store papers (can be
#'character() in order to dont store them).
#'
#'
#'@docType methods
#'@name AllClasses
#'@rdname AllClasses
#'@seealso \code{\link{methods-PaperDownloader}}
#'
#'@examples
#'## Create a PaperDownloader to query for two entities relations
#'entities <- c('terminal differentiation', 'basal-like');
#'pprDldr <- PaperDownloader(entities=entities, papersDir='/tmp/pmcERR/');
#'
#'@export PaperDownloader
#'
PaperDownloader <- setClass(
    Class='PaperDownloader',
    slots=c(
        entities='character',
        paperIds='character',
        papersDir='character'
    ),
    prototype=list(
    ),
    validity=function(object) {
        has_entities <- length(object@entities) > 0;
        entities_are_string <- all(unlist(
            lapply(object@entities, is.character)));
        all_ok <- has_entities && entities_are_string;
        return(all_ok);
    }
)

setClass(
    Class='Paper',
    slots=c(
        id='character',
        title='character',
        abstract='character',
        body='character',
        date='Date',
        sentences='character'
    ),
    prototype=list(
        date=as.Date('1918-08-18')
    ),
    validity=function(object) {
        has_id <- object@id != '' && is.character(object@id);
        all_ok <- has_id;
        return(all_ok);
    }
)
Paper <- function(...) {
    paper <- new('Paper', ...);
    stopifnot(validObject(paper));
    paper@sentences <- splitSentences(paper);
    return(paper);
}

Relation <- setClass(
    Class='Relation',
    slots=c(
        paper='Paper',
        relatedSents='character'
    ),
    prototype=list(
    ),
    validity=function(object) {
        all_ok <- TRUE;
        return(all_ok);
    }
)

Relations <- setClass(
    Class='Relations',
    slots=c(
        entities='character',
        relatedPapers='list' # list of Relation objects
    ),
    prototype=list(
    ),
    validity=function(object) {
        has_entities <- length(object@entities) > 0;
        entities_are_string <- all(unlist(
            lapply(object@entities, is.character)));
        all_ok <- has_entities && entities_are_string;
        return(all_ok);
    }
)

PapersManager <- setClass(
    Class='PapersManager',
    slots=c(
        papers='list' # list of Paper objects
    ),
    prototype=list(
    ),
    validity=function(object) {
        all_ok <- TRUE;
        return(all_ok);
    }
)
