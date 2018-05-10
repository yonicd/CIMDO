#' @title Create CIMDO object
#' @description Read in CIMDO output directory files into a single data.frame
#' @param path character, path to CIMDO output directory
#' @return data.frame
#' @rdname cimdo_read
#' @export 
#' @import dplyr
#' @importFrom plyr ddply
#' @importFrom rlang .data
cimdo_read <- function(path){
  
cimdo_files <- dplyr::data_frame(
    file = list.files( path, pattern = "csv", full.names = TRUE)
  )%>%
  dplyr::mutate(
    name = basename(file)    
  )%>%
  dplyr::left_join(cimdo_key,
            by="name")

cimdo_files <- cimdo_files[which(sapply(cimdo_files$file,file.size)!=0),]

file_id <- which(cimdo_files$measure=="user definded groups")

cimdo_manual <- read_cimdo_manual(cimdo_files,file_id)

# CIMDO ----

cimdo_out <- cimdo_files%>%
  dplyr::filter(.data[['startrow']]%in%c(0,1,2,3,11)&!matrix)%>%
  plyr::ddply(.variables = c("file","measure"),
              .fun = read_cimdo_out
              )

cimdo_out <- dplyr::bind_rows(
  cimdo_out%>%
    dplyr::mutate(measure  = as.character(.data[['measure']])),
  cimdo_manual)%>%
    dplyr::mutate(variable = gsub('^\\s+|\\s+$',
                                  '',
                                  gsub("rate_adj","",.data[['variable']])
                                  )
                  )%>%
    dplyr::mutate_at(dplyr::vars(.data[['file']],.data[['measure']]),
                     dplyr::funs(factor))

cimdo_out <- cimdo_out%>%
  dplyr::mutate(
    Date = as.Date(.data[['Date']],'%d/%m/%Y'),
    Y = format(.data[['Date']],'%Y'),
    m = format(.data[['Date']],'%m'),
    Q = yq(.data[['Date']],prefix = NULL)
  )

cimdo_shiny <- cimdo_out[,c("Date","Y","Q","m","variable","measure","value")]

names(cimdo_shiny) <- toupper(names(cimdo_shiny))

# DiDe ----

DiDe <- cimdo_files%>%
  dplyr::filter(matrix)%>%
  plyr::ddply(
    .variables = c('file','measure'),
    .fun = read_DiDe)

DiDe <- DiDe%>%
  dplyr::mutate(
    Date = as.Date(.data[['Date']],'%d/%m/%Y'),
    Y = format(.data[['Date']],'%Y'),
    m = format(.data[['Date']],'%m'),
    Q = yq(.data[['Date']],prefix = NULL)
  )

DiDe_shiny <- DiDe[,c("Date","Y","Q","m","measure","in.default","to.default","value")]

names(DiDe_shiny) <- toupper(names(DiDe_shiny))

DiDe_shiny <- DiDe_shiny%>%
  dplyr::mutate(VARIABLE=paste(.data[['TO.DEFAULT']],.data[['IN.DEFAULT']],sep="|"))%>%
  dplyr::select(-c(.data[['TO.DEFAULT']],.data[['IN.DEFAULT']]))

DiDe_shiny <- DiDe_shiny%>%
  dplyr::mutate_at(dplyr::vars(.data[['Y']],.data[['M']],.data[['VALUE']]),
                   dplyr::funs(as.numeric))%>%
  dplyr::mutate_at(dplyr::vars(-c(.data[['Y']],.data[['M']],.data[['DATE']],.data[['VALUE']])),
                   dplyr::funs(factor))

# to shiny ----

to_shiny <- rbind(cimdo_shiny,
                  DiDe_shiny)

to_shiny <- to_shiny%>%
  dplyr::mutate_at(
    dplyr::vars(.data[['Y']],.data[['M']]),
    dplyr::funs(as.numeric)
    )%>%
  dplyr::mutate_at(
    dplyr::vars(-c(.data[['Y']],.data[['M']],.data[['DATE']],.data[['VALUE']])),
    dplyr::funs(factor)
    )

to_shiny$MEASURE <- factor(to_shiny$MEASURE,
                           levels = as.character(cimdo_key$measure)[as.character(cimdo_key$measure)%in%levels(to_shiny$MEASURE)]
                           )

attr(to_shiny,'path') <- path
attr(to_shiny,'class') <- c(class(to_shiny),'cimdo')

return(to_shiny)
}
