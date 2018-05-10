#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param p PARAM_DESCRIPTION
#' @param geom PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @rdname remove_geom
#' @export 

remove_geom <- function(p, geom) {
  
  layers <- lapply(p$layers, function(x) if(any(grepl(paste0('(?i)',geom),class(x$geom)))) NULL else x)
  layers <- layers[!sapply(layers, is.null)]
  
  p$layers <- layers
  p
  
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param prefix PARAM_DESCRIPTION, Default: 'Y'
#' @param combine PARAM_DESCRIPTION, Default: 'Q'
#' @return OUTPUT_DESCRIPTION
#' @rdname yq
#' @export 

yq <- function (x,prefix="%Y",combine="Q"){
  
  prefix <- ifelse(is.null(prefix),"",format(x,prefix))
  
  paste(
    prefix,
    
    floor(as.numeric(format(x,"%m"))/3-1e-3)+1,
    
    sep=combine)
  
}
