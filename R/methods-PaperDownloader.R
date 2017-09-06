#'PaperDownloader methods
#'
#'Methods to work with a PaperDownloader.
#'\code{getPapersIds} gets the number of papers possible to download that
#'relate the entities. Updates the PaperDownloader.
#'\code{downloadPapers} downloads the papers (n is the max desired to download).
#'\code{getPapers} gets the list of downloaded Paper objects (n is the max
#'desired to download).
#'
#'@param paperDownloader a PaperDownloader object.
#'@param n (optional) numeric parameter.
#'@param ... not in use.
#'
#'@return PaperDownloader object with updated values. Or a list of Paper
#'objects.
#'
#'@docType methods
#'@name methods-PaperDownloader
#'@rdname methods-PaperDownloader
#'@seealso \code{\link{AllClasses}}
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
#'length(papers);
#'
setGeneric(name='methods-PaperDownloader', def=function(paperDownloader) {
    standardGeneric('methods-PaperDownloader')
})

#'@name getPapersIds
#'@inheritParams methods-PaperDownloader
#'@rdname methods-PaperDownloader
#'@aliases getPapersIds,PaperDownloader-method
#'@exportMethod getPapersIds
#'
setGeneric(name='getPapersIds', def=function(paperDownloader) {
    standardGeneric('getPapersIds')
})


#'@inheritParams methods-PaperDownloader
#'@rdname methods-PaperDownloader
#'@aliases getPapersIds,PaperDownloader-method
#'
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

        paperDownloader@paperIds <- papersIds;

        return(invisible(paperDownloader));
    }
)

#'@importFrom utils URLencode
#'
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


#'@name downloadPapers
#'@inheritParams methods-PaperDownloader
#'@rdname methods-PaperDownloader
#'@aliases downloadPapers,PaperDownloader-method
#'@exportMethod downloadPapers
#'
setGeneric(name='downloadPapers', def=function(paperDownloader, ...) {
    standardGeneric('downloadPapers')
})

#'@inheritParams methods-PaperDownloader
#'@rdname methods-PaperDownloader
#'@aliases downloadPapers,PaperDownloader-method,numeric
#'
#'@include AllClasses.R
#'
setMethod(
    f='downloadPapers',
    signature=c('PaperDownloader'),
    definition=function(paperDownloader, n=-1) {
        stopifnot(validObject(paperDownloader));

        if (length(paperDownloader@paperIds) == 0) {
            paperDownloader <- getPapersIds(paperDownloader);
        }

        papersDir <- paperDownloader@papersDir;
        if (length(papersDir) > 0 && !dir.exists(papersDir)) {
            dir.create(papersDir, recursive=TRUE);
        }

        paperIds <- paperDownloader@paperIds;
        n <- ifelse(n < 0, length(paperIds), min(length(paperIds), n));

        invisible(lapply(seq_len(n), function(i) {
            actPaperId <- paperIds[[i]];
            .downloadPaper(actPaperId, papersDir=papersDir);
        }))

        return(paperDownloader);
    }
)

#'@importFrom XML xmlToList xmlParse
#'
#'@include methods-PapersManager.R
#'
.downloadPaper <- function(paperId, papersDir=character()) {
    loadedPaper <- .getPapers(paperId);
    if (!is.null(loadedPaper) && is(loadedPaper, 'Paper')) {
        # Paper was already loaded
        message('Was already loaded paper with ID: ', paperId);
        return(loadedPaper);
    }

    database <- 'pmc';
    downloadUrl <- 'https://eutils.ncbi.nlm.nih.gov/entrez/eutils/';
    downloadUrl <- paste(downloadUrl, 'efetch.fcgi?db=', database, '&id=', paperId, sep='');

    paperFile <- paste0(paste0(papersDir, '/'), paperId, '.xml');
    if (length(papersDir) == 0 || !file.exists(paperFile)) {
        # if I dont want to save it. Or if I have not downloaded it
        tmpConnect <- url(downloadUrl, open='rb');
        outData <- readLines(tmpConnect, warn=FALSE, encoding='UTF-8');
        close.connection(tmpConnect);
        message('Downloaded paper with ID: ', paperId);
        if (length(papersDir) > 0) {
            # I want to download it
            writeLines(outData, paperFile);
        }
    } else {
        # if I already have it
        outData <- readLines(paperFile, warn=FALSE, encoding='UTF-8');
        message('Loaded paper with ID: ', paperId);
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

    paper <- Paper(id=paperId, title=title, abstract=abstract, body=body, date=date);
    .addPaper(paper);

    return(paper);
}
# downloadPaper <- .downloadPaper; # todo: delete

#'@name getPapers
#'@inheritParams methods-PaperDownloader
#'@rdname methods-PaperDownloader
#'@aliases getPapers,PaperDownloader-method
#'@exportMethod getPapers
#'
setGeneric(name='getPapers', def=function(paperDownloader, ...) {
    standardGeneric('getPapers')
})

#'@inheritParams methods-PaperDownloader
#'@rdname methods-PaperDownloader
#'@aliases getPapers,PaperDownloader-method,numeric
#'
#'@include AllClasses.R
#'@include methods-PapersManager.R
#'
setMethod(
    f='getPapers',
    signature=c('PaperDownloader'),
    definition=function(paperDownloader, n=-1) {
        stopifnot(validObject(paperDownloader));

        paperDownloader <- downloadPapers(paperDownloader, n);

        paperIds <- paperDownloader@paperIds;

        if (n > 0 && length(paperIds) > 0) {
            paperIds <- paperIds[1:min(n, length(paperIds))];
        } else if (n == 0) {
            paperIds <- NULL;
        }
        papers <- .getPapers(paperIds)

        return(papers);
    }
)
