
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

newSearchPage <- function() {
    wellPanel(
        fluidRow(
            # column(6, selectInput(inputId='databaseInput', label='Database', list(PMC='pmc'), selected=NULL)),
            column(6, textInput(inputId='sepInput', label='Separator character', value='|')),
            column(3, checkboxInput(inputId="exactMatchInput", label="Exact match", value=!F))),
        wellPanel(
            fluidRow(
                column(5, fileInput(inputId='entitiesInputFile', label=NULL, buttonLabel='Load entities file')),
                column(7, actionButton(inputId='searchbutton', label='Search', width='100%'))),
            textAreaInput(inputId='entitiesInput', label='Entities', value='', height='600px', width='100%', resize='both', placeholder="In each line place a query, each entity must be | separated.\nEg:\nentity 1 | entity 2\nentity 3 | entity 4 | entity 5")
        )
    )
}

downloadPage <- function() {
    wellPanel(
        DT::dataTableOutput(outputId='outputdtable'),
        fluidRow(
            # column(4, numericInput(inputId='maxPapersInput', label=NULL, value=NA)),
            column(3, textInput(inputId='maxPapersInput', label=NULL, placeholder='Max papers to download per entity')),
            column(3, textInput(inputId='ignoreEntsInput', label=NULL, placeholder='Exclude entities with more than #papers')),
            column(6, actionButton(inputId='downloadbutton', label='Download', width='100%')))
    )
}

relationsPage <- function() {
    wellPanel(
        fluidRow(
            column(8, selectInput(inputId='entitySelInput', label='Entities', list(), selected=NULL, multiple=TRUE, size=10, selectize=FALSE)),
            # column(1, actionButton(inputId='relsFilterbutton', label=NULL, icon=icon('filter'))),
            column(4, selectInput(inputId='paperSelInput', label='Related papers IDs', list(), selected=NULL, size=10, selectize=FALSE))),
        fluidRow(
            column(2, downloadButton('downloadResLink', 'Download results')),
            column(2, actionButton(inputId='plotNetwbtn', label='Plot as network'))),
        br(),
        br(),
        wellPanel(
            fluidRow(
                column(9, textInput(inputId='papertitle', label='Title')),
                column(3, textInput(inputId='paperDate', label='Date'))
            ),
            textInput(inputId='paperLink', label=NULL),
            htmlOutput(outputId='paperOutput')
        )
    )
}

networkPage <- function() {
    wellPanel(
        visNetworkOutput("networkPlot", height='700px')
    )
}

loadResPage <- function() {
    fileInput(inputId='loadResInputFile', label=NULL, buttonLabel='Load results file', accept=c('.RData, .Rds, .rda'))
}

shinyUI(
    fluidPage(
        useShinyjs(),
        navbarPage(title='pmcERR', id='maintabset',
                   tabPanel('New search', newSearchPage(), selected=TRUE),
                   tabPanel('Download', downloadPage()),
                   tabPanel('Relations', relationsPage()),
                   tabPanel('Network', networkPage()),
                   tabPanel('Load results', loadResPage())
        )
    )
)
