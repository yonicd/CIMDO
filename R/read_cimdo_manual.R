#' @import dplyr
#' @importFrom utils read.csv
#' @importFrom rlang .data
read_cimdo_manual <- function(files,file_id){
  
  cimdo_manual <- utils::read.csv(files$file[file_id],
           header = TRUE,
           skip = files$startrow[file_id],
           row.names = NULL,
           stringsAsFactors = FALSE)%>%
    dplyr::mutate(Date = as.Date(.data[['Date']],format="%d/%m/%Y"),
                  variable = "Manual",
                  file = files$file[file_id],
                  measure = as.character(files$measure)[file_id])%>%
    dplyr::rename(value = .data[['P.idxProb.going.default.idxGiven.default.']])
  
  cimdo_manual_names <- utils::read.csv(files$file[file_id],
                                 header = FALSE,
                                 skip = 3,
                                 nrows = 2,
                                 row.names = NULL,
                                 stringsAsFactors = FALSE)
  
  cimdo_manual_names <- gsub("[()]|idxProb=|idxGiven=",
                             "",
                             gsub('^\\s+|\\s+$','',cimdo_manual_names$V1)
                             )
  
  cimdo_manual$variable <- paste0(cimdo_manual_names,collapse = "|")
  
  cimdo_manual
}
