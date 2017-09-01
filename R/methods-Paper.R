#'@exportMethod getFullPaper
#'
setGeneric(name='getFullPaper', def=function(paper) {
    standardGeneric('getFullPaper')
})

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

#'@exportMethod getSentences
#'
setGeneric(name='getSentences', def=function(paper) {
    standardGeneric('getSentences')
})

#'@include AllClasses.R
#'@importFrom tokenizers tokenize_sentences
#'
setMethod(
    f='getSentences',
    signature=c('Paper'),
    definition=function(paper) {
        stopifnot(validObject(paper));

        sentences <- tokenize_sentences(.replaceAbbreviations(
            paste(paper@title, paper@abstract, paper@body, sep='. ')), simplify=TRUE);
        # sentences <- tokenize_sentences(.replaceAbbreviations(paper@title), simplify=TRUE);
        # sentences <- c(sentences, tokenize_sentences(.replaceAbbreviations(paper@abstract), simplify=TRUE));
        # sentences <- c(sentences, tokenize_sentences(.replaceAbbreviations(paper@body), simplify=TRUE));
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
