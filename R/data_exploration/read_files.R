#' Read various filetypes to convert to dataframe.
#'
#' @param df A dataframe if filetype is dataframe or file path.
#' @param filetype_in Type of file read. Provide extension of file if not dataframe. Permissible filetypes are Rds, rds, csv, xls and xlsx.
#' @return A dataframe.
#' @examples
#' data = filetype((data="/stats/projects/all/R_Tools_Development/data/salaries_data.Rds",filetype_in="Rds"))
#' 


filetype <- function(df,filetype_in="dataframe") {
  
  if (tolower(filetype_in) == "dataframe") {data = df}  else if (tolower(filetype_in) == "rds") {
    data = readRDS(df)}  else if (tolower(filetype_in) == "xls") {
      data = read.xlsx(df, sheetName = 1, header = TRUE)}  else if (tolower(filetype_in) == "xlsx") {
        data =  read.xlsx(df, sheetName = 1, header = TRUE)}  else if (tolower(filetype_in) == "csv") {
          data = read.csv(df,header = TRUE, sep = ",")}  else stop("ERROR: File type is not compatible")
  
 
  return(data)
  
}