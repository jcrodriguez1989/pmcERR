# install.packages('pmcQuery_0.1.0.tar.gz', type='source', repos=NULL);
# rm(list=ls());
# library(pmcQuery); library(tokenizers);
# entities <- c('terminal differentiation', 'basal-like');
# paperDownloader <- PaperDownloader(entities=entities, papersDir='/media/jcrodriguez/Data11/mytmp/noExiste/');
# paperDownloader <- PaperDownloader(entities=entities, papersDir='/home/jcrodriguez/relatedpapers/papers');
# paperDownloader <- getPapersIds(paperDownloader);
# paperDownloader <- downloadPapers(paperDownloader);
# papers <- getPapers(paperDownloader); length(papers);
# relations <- getRelations(papers, entities); # relations
# length(relations@relatedPapers);
# lapply(relations@relatedPapers, function(x) x@relatedSents);

#'@exportMethod getPapersIds
#'
setGeneric(name='getPapersIds', def=function(paperDownloader) {
    standardGeneric('getPapersIds')
})

#'@importFrom XML xmlToList xmlTreeParse
#'@include AllClasses.R
#'
setMethod(
    f='getPapersIds',
    signature=c('PaperDownloader'),
    definition=function(paperDownloader) {
        stopifnot(validObject(paperDownloader));

        query <- .getQuery(paperDownloader);
        queryUrl <- 'https://eutils.ncbi.nlm.nih.gov/entrez/eutils/'; # ncbi api
        database <- 'pmc'; # PubMed Central papers

        completeUrl <- paste(queryUrl, 'esearch.fcgi?db=', database, '&term=',
            query, '&retmax=999999', sep='');

        IDconnect <- url(completeUrl, open='rb'); # todo: poner try
        idXML <- readLines(IDconnect, warn=FALSE, encoding='UTF-8');
        idXML <- xmlTreeParse(idXML);
        close.connection(IDconnect);
        myIDlist <- xmlToList(idXML);

        papersIds <- unlist(myIDlist$IdList);
        papersIds <- sort(papersIds, decreasing=TRUE);
        message(length(papersIds), ' papers to download')

        papers <- as.list(rep(NA, length(papersIds)));
        names(papers) <- papersIds;
        paperDownloader@papers <- papers;

        return(invisible(paperDownloader));
    }
)

.getQuery <- function(paperDownloader) {
    stopifnot(is(paperDownloader, 'PaperDownloader'));
    stopifnot(validObject(paperDownloader));
    entities <- paperDownloader@entities;
    query <- paste('("', entities, '")', sep='', collapse='AND');
    query <- URLencode(query, reserved=TRUE);
    query <- gsub('%20', '+', query);
    return(query);
}
# getQuery <- .getQuery; # todo: delete


#'@exportMethod downloadPapers
#'
setGeneric(name='downloadPapers', def=function(paperDownloader, ...) {
    standardGeneric('downloadPapers')
})

#'@include AllClasses.R
#'
setMethod(
    f='downloadPapers',
    signature=c('PaperDownloader'),
    definition=function(paperDownloader, n=-1) {
        stopifnot(validObject(paperDownloader));

        if (length(paperDownloader@papers) == 0) {
            paperDownloader <- getPapersIds(paperDownloader);
        }

        papersDir <- paperDownloader@papersDir;
        if (length(papersDir) > 0 && !dir.exists(papersDir)) {
            dir.create(papersDir, recursive=TRUE);
        }

        actPapers <- paperDownloader@papers;
        ids <- names(actPapers);
        n <- ifelse(n < 0, length(ids), min(length(ids), n));

        actPapers[seq_len(n)] <- lapply(seq_len(n), function(i) {
            actPaper <- actPapers[[ ids[[i]] ]];
            if (!is(actPaper, 'Paper')) {
                actPaper <- downloadPaper(ids[[i]], database='pmc', papersDir=papersDir);
            }
            return(actPaper);
        })

        paperDownloader@papers <- actPapers;
        return(invisible(paperDownloader));
    }
)

#'@importFrom XML xmlToList xmlParse
.downloadPaper <- function(paperId, database='pmc', papersDir=character()) {
    downloadUrl <- 'https://eutils.ncbi.nlm.nih.gov/entrez/eutils/';
    downloadUrl <- paste(downloadUrl, 'efetch.fcgi?db=', database, '&id=', paperId, sep='');

    paperFile <- paste0(paste0(papersDir, '/'), paperId, '.xml');

    if (length(papersDir) == 0 || !file.exists(paperFile)) {
        # if I dont want to save it. Or if I have not downloaded it
        tmpConnect <- url(downloadUrl, open='rb');
        outData <- readLines(tmpConnect, warn=FALSE, encoding='UTF-8');
        close.connection(tmpConnect);
#         message('Downloaded paper with ID: ', paperId);
        if (length(papersDir) > 0) {
            # I want to download it
            writeLines(outData, paperFile);
        }
    } else {
        # if I already have it
        outData <- readLines(paperFile, warn=FALSE, encoding='UTF-8');
#         message('Loaded paper with ID: ', paperId);
    }
    outData <- xmlParse(outData, encoding='UTF-8');
    myPaperlist <- xmlToList(outData);

    title <- myPaperlist$article$front$`article-meta`$`title-group`$`article-title`;
    title <- ifelse(is.list(title), paste(rapply(title, paste), collapse=' '), title);
    date <- myPaperlist$article$front$`article-meta`$`pub-date`;
    date <- try({ as.Date(paste(date$year, date$month, date$day, sep='-')); }, silent=TRUE);
    if (inherits(date, 'try-error')) date <- as.Date('1918-08-18');
    abstract <- myPaperlist$article$front$`article-meta`$abstract;
    abstract <- abstract[!grepl('.attrs', names(abstract))];
    abstract <- ifelse(is.list(abstract), paste(rapply(abstract, paste), collapse=' '), '');
    body <- myPaperlist$article$body;
    body <- ifelse(is.list(body), paste(rapply(body, paste), collapse=' '), '');

    paper <- Paper(id=paperId, database=database, title=title, abstract=abstract, body=body, date=date);

    return(paper);
}
downloadPaper <- .downloadPaper; # todo: delete

#'@exportMethod getPapers
#'
setGeneric(name='getPapers', def=function(paperDownloader, ...) {
    standardGeneric('getPapers')
})

#'@include AllClasses.R
#'
setMethod(
    f='getPapers',
    signature=c('PaperDownloader'),
    definition=function(paperDownloader, n=-1) {
        stopifnot(validObject(paperDownloader));

        paperDownloader <- downloadPapers(paperDownloader, n);

        papers <- paperDownloader@papers;

        if (n > 0 && length(papers) > 0) {
            papers <- papers[1:min(n, length(papers))];
        } else if (n == 0) {
            papers <- list();
        }

        return(papers);
    }
)
