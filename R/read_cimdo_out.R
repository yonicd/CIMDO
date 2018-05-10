#' @import dplyr
#' @importFrom reshape2 melt
#' @importFrom utils read.csv
#' @importFrom rlang .data
read_cimdo_out <- function(data){
  
  p1 <- utils::read.csv(data$file,header = TRUE,
                 skip = data$startrow,
                 row.names = NULL,
                 stringsAsFactors = FALSE)
  
  if(data$shift){
    
    nc <- ncol(p1)
    names(p1)[-nc] <- names(p1)[-1]
    p1 <- p1[,-nc]
    
  }
  
  p1 <- p1%>%
      dplyr::mutate(Date = as.Date(.data[['Date']],format="%d/%m/%Y"))
    
  p1 <- p1%>%
    reshape2::melt(id = "Date")%>%
    dplyr::mutate(variable = gsub("[.|P.]"," ",.data[['variable']]))
  
  return(p1)
  
}
