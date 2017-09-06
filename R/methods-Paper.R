#'Paper methods
#'
#'Methods to work with a Paper.
#'\code{getFullPaper} returns the full paper as a string.
#'\code{getSentences} returns the full paper in a vector separating each
#'sentence.
#'
#'@param paper a Paper object.
#'
#'@return string, or character vector.
#'
#'@docType methods
#'@name methods-Paper
#'@rdname methods-Paper
#'
#'@examples
#'## Create a PaperDownloader to query for two entities relations
#'entities <- c('terminal differentiation', 'basal-like');
#'pprDldr <- PaperDownloader(entities=entities, papersDir='/tmp/pmcERR/');
#'
#'## Get the first paper
#'papers <- getPapers(pprDldr, n=1);
#'paper <- papers[[1]];
#'
#'getSentences(paper);
#'
setGeneric(name='methods-Paper', def=function(paper) {
    standardGeneric('methods-Paper')
})

#'@name getFullPaper
#'@inheritParams methods-Paper
#'@rdname methods-Paper
#'@aliases getFullPaper,Paper-method
#'@exportMethod getFullPaper
#'
setGeneric(name='getFullPaper', def=function(paper) {
    standardGeneric('getFullPaper')
})

#'@inheritParams methods-Paper
#'@rdname methods-Paper
#'@aliases getFullPaper,Paper-method
#'
#'@include AllClasses.R
#'
setMethod(
    f='getFullPaper',
    signature=c('Paper'),
    definition=function(paper) {
        stopifnot(validObject(paper));

        fullPaper <- paste(paper@title, paper@abstract, paper@body, sep='. ');
        return(fullPaper);
    }
)

#'@name getSentences
#'@inheritParams methods-Paper
#'@rdname methods-Paper
#'@aliases getSentences,Paper-method
#'@exportMethod getSentences
#'
setGeneric(name='getSentences', def=function(paper) {
    standardGeneric('getSentences')
})

#'@inheritParams methods-Paper
#'@rdname methods-Paper
#'@aliases getSentences,Paper-method
#'
#'@include AllClasses.R
#'
setMethod(
    f='getSentences',
    signature=c('Paper'),
    definition=function(paper) {
        stopifnot(validObject(paper));

        return(paper@sentences);
    }
)

setGeneric(name='splitSentences', def=function(paper) {
    standardGeneric('splitSentences')
})

#'@include AllClasses.R
#'@importFrom tokenizers tokenize_sentences
#'
setMethod(
    f='splitSentences',
    signature=c('Paper'),
    definition=function(paper) {
        stopifnot(validObject(paper));

        sentences <- tokenize_sentences(pmcERR:::.replaceAbbreviations(
            paste(paper@title, paper@abstract, paper@body, sep='. ')), simplify=TRUE);

        if (is.list(sentences)) {
            sentences <- as.character(sentences);
        }
        return(sentences);
    }
)

.replaceAbbreviations <- function(sentence) {
    abbreviations <- c('Fig.', 'al.', 'vs.', 'Vs.', 'min.', 'Min.', 'max.', 'Max.', 'e.g.', 'E.g.', 'Dr.', 'St.', 'i.e.', 'Inc.', 'Figs.', 'Ref.', 'sp.');
    for (abb in abbreviations) {
        abbRepl <- gsub('.', '', abb, fixed=TRUE);
        sentence <- gsub(abb, abbRepl, sentence, fixed=TRUE);
    }
    return(sentence);
}
# replaceAbbreviations <- .replaceAbbreviations # todo: delete
