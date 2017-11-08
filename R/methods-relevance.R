setGeneric(name='getRelevance', def=function(object, entities) {
    standardGeneric('getRelevance')
})


################### For Relations

#'@include AllClasses.R
#'
# For a relation returns its relevance based of each sentence relevance
# Note, if relevance was not calculated then it returns the object (filled)
setMethod(
    f='getRelevance',
    signature=c('Relations', 'missing'),
    definition=function(object) {
        if (length(object@relevance) == 1) return(object@relevance);

        relations <- object@relatedPapers;
        relRelevances <- lapply(relations, getRelevance);
        relRelevances <- unlist(lapply(relRelevances, function(x) x$relevance));

        if (is.null(relRelevances)) {
            # no related papers
            object@relevance <- -Inf;
            return(object);
        }

        newOrder <- order(relRelevances, decreasing=!F);
        relations <- relations[newOrder];

        # Just sum each paper relevance
        relsRelevance <- sum(relRelevances);

        # And give some points if has many related papers
        nPapers <- length(relations);
        # if 1 nPapers then no extra point; if 2 or 3 -> 0.5 point; if more -> 1
        addPoints <- ifelse(nPapers == 1, 0, ifelse(nPapers > 3, 1, 0.5));

        relsRelevance <- relsRelevance + addPoints;

        object@relevance <- relsRelevance;
        object@relatedPapers <- relations;

        return(object);
    }
)

################### For Relation

#'@include AllClasses.R
#'
# For a relation returns its relevance based of each sentence relevance
# Value: [-1;1] * nSents + [0;1]
# Note, if relevance was not calculated then it returns the object (filled)
setMethod(
    f='getRelevance',
    signature=c('Relation', 'missing'),
    definition=function(object) {
        if (length(object@relevance) == 2) return(object@relevance);

        entities <- object@entities;
        sentences <- object@relatedSents;

        sentsRelevance <- unlist(lapply(sentences, getRelevance, entities));
        newOrder <- order(sentsRelevance, decreasing=!F);
        sentences <- sentences[newOrder];
        sentsRelevance <- sentsRelevance[newOrder];

        # Just sum each sents relevance
        relRelevance <- sum(sentsRelevance);

        # And give some points if has many related sents
        nSents <- length(sentences);
        # if 1 relSent then no extra point; if 2 or 3 -> 0.5 point; if more -> 1
        addPoints <- ifelse(nSents == 1, 0, ifelse(nSents > 3, 1, 0.5));

        relRelevance <- relRelevance + addPoints;

        result <- list(relevance=relRelevance, sentsRelevance=sentsRelevance);
        object@relevance <- result;
        object@relatedSents <- sentences;

        return(object);
    }
)

################### For Sentence (string)

# Combine different relevance metrics for each sentence.
# Value: from -1 to 1
setMethod(
    f='getRelevance',
    signature=c('character', 'character'),
    definition=function(object, entities) {
        stopifnot(length(object) == 1);
        stopifnot(length(entities) > 0);

        bow <- .bagOfWords(object); bow
        sentLength <- .sentenceLength(object); sentLength
        entPres <- .entPresence(object, entities); entPres
        distEnts <- .distanceBetweenEntities(object, entities); distEnts

        result <- c(bow, sentLength, entPres, distEnts);
        # result <- result[result != 0];
        result <- mean(result);

        # combine results
        result <- 35*bow + 9*entPres + 28*sentLength + 28*distEnts;
        result <- result/100;

        if (getOption('verbose')) {
            print('*****************************************')
            print(paste('Entities:', entities, collapse=', '));
            print(object);
            print('--------------------')
            print(result);
            print(c(bow=bow, entPres=entPres, sentLength=sentLength, distEnts=distEnts));
        }

        stopifnot(-1 <= result && result <= 1);
        return(result);
    }
)

# Gives relevance depending on desirable (or not) words
# Value: from -1 to 0
.bagOfWords <- function(object) {
    negWords <- c('Table', 'Figure');

    object <- tolower(object);
    negWords <- tolower(negWords);

    # just the mean of words that are in the
    negScore <- mean(unlist(lapply(negWords, function(actWord) {
        grepl(actWord, object, fixed=TRUE)
    })))

    # todo: put some positiveWords

    # do a combination between positive and negative words
    result <- -negScore;

    stopifnot(-1 <= result && result <= 0);
    return(result);
}

# Depending on the length of the sentence it gives the relevance.
# the average sentence length 15-20 words, as said in:
# https://strainindex.wordpress.com/2008/07/28/the-average-sentence-length/
# Value: from -1 to 1
.sentenceLength <- function(object) {
    sentLength <- sum(unlist(strsplit(object, ' ')) != '');
    result <- 0;
    if (sentLength < 15) {
        result <- (14-sentLength+1) / 14; # positive
    } else if (sentLength < 21) {
        result <- 0;
    } else {
        result <- 20 / sentLength - 1; # negative
    }

    stopifnot(-1 <= result && result <= 1);
    return(result);
}

# More relevance if entities appear several times.
# Value: from 0 to 1
.entPresence <- function(object, entities) {
    object <- tolower(object);
    entities <- tolower(entities);

    entApp <- sum(unlist(lapply(entities, function(actWord) {
        unlist(gregexpr(actWord, object, fixed=TRUE)) >= 0
    })))

    result <- 1-length(entities)/entApp;

    stopifnot(0 <= result && result <= 1);
    return(result);
}

# Relevance depending on the distance between the entities
# Value: from -1 to 1
.distanceBetweenEntities <- function(object, entities) {
    if (length(entities) == 1) return(0);

    object <- tolower(object);
    entities <- tolower(entities);

    entsPos <- lapply(entities, function(actWord) {
        actRes <- unlist(gregexpr(actWord, object, fixed=TRUE));
        return(c(actRes, actRes+nchar(actWord)));
    })

    combs <- do.call(expand.grid, entsPos);
    # min distance that contains all entities. From first to last char
    entsDist <- min(apply(combs, 1, function(x) max(x) - min(x)));

    halfSent <- nchar(object)/2;
    result <- -(entsDist - halfSent) / (halfSent);

    stopifnot(-1 <= result && result <= 1);
    return(result);
}
