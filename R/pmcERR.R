#'pmcERR GUI
#'
#'pmcERR package shiny-based graphical user interface (GUI)
#'\code{pmcERR} starts the GUI.
#'
#'@docType methods
#'@name pmcERR-GUI
#'@rdname pmcERR-GUI
#'
#'@examples
#'## Start the GUI
#'\dontrun{
#'pmcERR();
#'}
#'
#'@importFrom shiny runApp
#'@export pmcERR
#'
pmcERR <- function() {
    appDir <- system.file('shiny', 'pmcERRGUI', package='pmcERR');
    if (appDir == '') {
        stop("Could not find GUI directory. Try re-installing `pmcERR`.", call.=FALSE);
    }

    runApp(appDir);
}
