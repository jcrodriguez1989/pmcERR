# All class definitions in R/AllClasses.R

#'@export Paper
#'
Paper <- setClass(
    Class='Paper',
    slots=c(
        id='character',
        database='character',
        title='character',
        abstract='character',
        body='character',
        date='Date'
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

#'@export PaperDownloader
#'
PaperDownloader <- setClass(
    Class='PaperDownloader',
    slots=c(
        entities='character',
        papers='list',
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

#'@export Relations
#'
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
