# PubMed Central Entity Relation Recognition.

The pmcERR package allows downloading the scientific articles of PubMed Central given a list of entities. Then given the downloaded articles looks for relations between these entities within them. In this way, giving a list of articles that validate the relationship between entities.

## Installation
In R console type:

    R > install.packages('devtools');
    R > library(devtools);
    R > install_github('jcrodriguez1989/pmcERR');
