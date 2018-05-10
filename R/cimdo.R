#'@title CIMDO Dashboard
#'@description Launch shiny app UI for CIMDO from R console
#'@param obj object of cimdo class, Default: NULL
#'@param path character, path to CIMDO output directory, Default: NULL
#'@param plotHeight numeric, that sets the height of the plot output, Default 800
#'@param viewerType character, viewer to be used to launch the app to
#' c('paneViewer','dialogViewer','browserViewer'), Default: 'paneViewer'
#' @details pass to the function either a precompiled cimdo class object using
#' \code{\link{cimdo_read}} or pass a path to a CIMDO output directory to 
#' simultaneously read and launch.
#'@examples
#' \dontrun{
#'if(interactive()){
#'
#' PATH <- PATH_TO_CIMDO_OUTPUT
#'
#' obj <- cimdo_read(PATH)
#' 
#' cimdo(obj)
#'
#'}
#'}
#'@export
#'@rdname cimdo
cimdo <- function(obj = NULL,
                  path = NULL,
                  plotHeight = 800,
                  viewerType = 'paneViewer'){
  
  if(is.null(obj))
    obj <- cimdo_read(path = path)
  
  if(!inherits(obj,'cimdo'))
    stop('input object must inherit a cimdo class (read data through CIMDO::cimdo_read)')
  
  obj%>%
    cimdo_gadget(
      plotHeight,
      viewerType
      )
  
}