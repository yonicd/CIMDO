#' @importFrom reshape2 melt
#' @importFrom utils read.csv
#' @importFrom rlang .data
read_DiDe <- function(data){
  
  jointmat <- utils::read.csv(data$file,header = TRUE,
                       skip = data$startrow,
                       row.names = NULL)
  
  nc <- ncol(jointmat)
  
  names(jointmat)[-nc] <- names(jointmat)[-c(1)]
  
  jointmat <- jointmat[,-nc]
  
  names(jointmat)[2] <- "to.default"
  
  jointmat$Date[jointmat$Date==""] <- NA
  
  jointmat$ID_ <- rep(1:(nrow(jointmat)/length(unique(jointmat$to.default))),
                      each=length(unique(jointmat$to.default)))
  
  jointmat <- jointmat%>%
    dplyr::select(-.data[['Date']])%>%
    dplyr::left_join(
      jointmat%>%
        dplyr::select(.data[['Date']],.data[['ID_']])%>%
        dplyr::filter(!is.na(.data[['Date']])),
      by='ID_')%>%
    dplyr::select(-.data[['ID_']])%>%
    dplyr::select(.data[['Date']],dplyr::everything())%>%
    dplyr::mutate(as.Date(.data[['Date']],format="%d/%m/%Y"))
  
  jointmat <- jointmat%>%
    reshape2::melt(id=c("to.default","Date"))
  
  names(jointmat)[3] <- "in.default"
  
  jointmat$in.default <- factor(jointmat$in.default,
                                labels = gsub("[a-z._ ]","",levels(jointmat$in.default))
                                )
  
  jointmat$to.default <- factor(jointmat$to.default,
                                labels = gsub("[a-z._ ]","",levels(jointmat$to.default))
                                )
  
  jointmat <- jointmat[as.character(jointmat$in.default)!=as.character(jointmat$to.default),]
  
  return(jointmat)
}
