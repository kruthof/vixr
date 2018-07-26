#' Helper Function that allows the conversion of date variables, often find in
#' quote tables from data providers. i.e.when  'quote_datetime' the form of
#' 'YYYY-MM-DD HH:MM:SS'
#'
#' @param data Dataframe incl. variables 'expiration' and 'quote_datetime'.
#'   'dmY==TRUE' should be used if data is int the format of d-m-Y.
#'
#' @return Dataframe with new variables 'QuoteTime' (indicating minutes from
#'   midnight until quote) and 'QuoteDate'
#' @seealso \code{\link{letVixShiny}},\code{\link{vic_cal}}
#' @importFrom reshape2 "colsplit"
#' @examples convert_data(data,dmY=TRUE) 
convert_data = function(Data, dmY=FALSE){    

  #check if date format equals d.m.y (as provided by user)
  if (dmY == TRUE) {
    as.Date(x=Data$expiration, format, tryFormats = c("%d-%m-%Y", "%d/%m/%Y","%d.%m.%Y"))
    tmp_DateTime_split = colsplit(gsub("([^ ])([ ])", "\\1\\ \\2", Data$quote_datetime),
                                "\\s", c("QuoteDate", "QuoteTime"))
  
    Data$QuoteTime = strtoi(as.difftime(tmp_DateTime_split$QuoteTime, format = "%H:%M", units = "mins"))
    Data$QuoteDate = as.Date(tmp_DateTime_split$QuoteDate, format="%d.%m.%Y")
  } else { Data$expiration  = as.Date(Data$expiration)


  #Split variable "quote_datetime into two seperate variables
  tmp_DateTime_split = colsplit(gsub("([^ ])([ ])", "\\1\\.\\2", Data$quote_datetime),
                              "\\.", c("QuoteDate", "QuoteTime"))



  #converting QuoteTime into minutes from midnight
  Data$QuoteTime = strtoi(as.difftime(tmp_DateTime_split$QuoteTime, format = "%H:%M:%S", units = "mins"))
  #converting QuoteData to type Date
  Data$QuoteDate = as.Date(tmp_DateTime_split$QuoteDate)
  }
  return(data)
}