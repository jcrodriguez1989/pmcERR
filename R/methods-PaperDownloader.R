#'PaperDownloader methods
#'
#'Methods to work with a PaperDownloader.
#'\code{getPapersIds} gets the number of papers possible to download that
#'relate the entities. And updates the PaperDownloader.
#'\code{downloadPapers} downloads the papers (n is the max desired to download).
#'\code{getPapers} gets the list of downloaded Paper objects (n is the max
#'desired to download).
#'
#'@param paperDownloader a PaperDownloader object.
#'@param n (optional) numeric parameter.
#'@param exactMatch (optional) logical indicating if entities must be matched
#'exactly. Ie each entity between quotation marks when querying. Default: TRUE.
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
setGeneric(name='getPapersIds', def=function(paperDownloader, ...) {
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
    definition=function(paperDownloader, exactMatch=TRUE) {
        stopifnot(validObject(paperDownloader));

        query <- .getQuery(paperDownloader, exactMatch);
        queryUrl <- 'https://eutils.ncbi.nlm.nih.gov/entrez/eutils/'; # ncbi api
        database <- 'pmc'; # PubMed Central papers

        completeUrl <- paste(queryUrl, 'esearch.fcgi?db=', database, '&term=',
            query, '&retmax=999999', sep='');

        idXML <- .downloadXml(completeUrl, tries=5); # try to download the xml
        papersIds <- character();
        if (is.character(idXML)) {
            idXML <- xmlTreeParse(idXML);
            myIDlist <- xmlToList(idXML);

            papersIds <- unlist(myIDlist$IdList);
            if (length(papersIds) > 0)
                papersIds <- sort(papersIds, decreasing=TRUE);
        } else {
            warning(paste('Error downloading', completeUrl));
        }
        message(length(papersIds), ' papers to download')
        paperDownloader@paperIds <- as.character(papersIds);

        return(invisible(paperDownloader));
    }
)

#'@importFrom utils URLencode
#'
.getQuery <- function(paperDownloader, exactMatch) {
    stopifnot(is(paperDownloader, 'PaperDownloader'));
    stopifnot(validObject(paperDownloader));
    entities <- paperDownloader@entities;

    if (exactMatch) {
        query <- paste('("', entities, '")', sep='', collapse='AND');
    } else {
        query <- paste('(', entities, ')', sep='', collapse='AND');
    }

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
    loadedPaper <- .getPapers(paperId)[[1]];
    if (!is.null(loadedPaper) && is(loadedPaper, 'Paper')) {
        # Paper was already loaded
        message('Was already loaded paper with ID: ', paperId);
        return(loadedPaper);
    }

    database <- 'pmc';
    downloadUrl <- 'https://eutils.ncbi.nlm.nih.gov/entrez/eutils/';
    downloadUrl <- paste(downloadUrl, 'efetch.fcgi?db=', database, '&id=',
                         paperId, sep='');

    paperFile <- paste0(paste0(papersDir, '/'), paperId, '.xml');
    if (length(papersDir) == 0 || !file.exists(paperFile)) {
        # if I dont want to save it. Or if I have not downloaded it
        outData <- .downloadXml(downloadUrl, tries=5);
        message('Downloaded paper with ID: ', paperId);
        if (length(papersDir) > 0 && !is.na(outData[[1]])) {
            # I want to download it
            writeLines(outData, paperFile);
        }
    } else {
        # if I already have it
        outData <- readLines(paperFile, warn=FALSE, encoding='UTF-8');
        message('Loaded paper with ID: ', paperId);
    }

    if (is.na(outData[[1]])) {
        return(NA);
    }
    outData <- xmlParse(outData, encoding='UTF-8');
    myPaperlist <- xmlToList(outData);

    if (names(myPaperlist) == 'error') {
        # server side error
        return(NA);
    }

    title <- myPaperlist$article$front$`article-meta`$`title-group`$`article-title`;
    title <- ifelse(is.list(title), paste(rapply(title, paste), collapse=' '),
                    title);
    date <- myPaperlist$article$front$`article-meta`$`pub-date`;
    date <- try({ as.Date(paste(date$year, date$month, date$day, sep='-')); },
                silent=TRUE);
    if (inherits(date, 'try-error')) date <- as.Date('1918-08-18');
    abstract <- myPaperlist$article$front$`article-meta`$abstract;
    abstract <- abstract[!grepl('.attrs', names(abstract))];
    abstract <- ifelse(is.list(abstract), paste(rapply(abstract, paste),
                                                collapse=' '), '');
    body <- myPaperlist$article$body;
    body <- ifelse(is.list(body), paste(rapply(body, paste),
                                        collapse=' '), '');

    paper <- Paper(id=paperId, title=title, abstract=abstract, body=body,
                   date=date);
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
        papers <- .getPapers(paperIds);
        papers <- papers[!is.na(names(papers))];

        return(papers);
    }
)

.downloadXml <- function(downloadUrl, tries=1L) {
    tries <- as.integer(tries);
    stopifnot(length(tries) == 1L, !is.na(tries));

    while (tries > 0L) {
        hasError <- try({
            tmpConnect <- url(downloadUrl, open='rb');
            outData <- readLines(tmpConnect, warn=FALSE, encoding='UTF-8');
            close.connection(tmpConnect);
        }, silent=TRUE);

        if (inherits(hasError, 'try-error')) {
            warning(paste('Retrying to download:', downloadUrl));
            tries <- tries - 1L;
        } else {
            break;
        }

    }

    if (tries == 0L) {
        outData <- NA;
    }

    return(outData);
}
