#' Read various filetypes to convert to dataframe.
#'
#' @param data A dataframe if filetype is dataframe or file path.
#' @param filetype Type of file read. Provide extension of file if not dataframe. Permissible filetypes are Rds, rds, csv, xls and xlsx.
#' @return A dataframe.
#' @examples
#' data = filetype((data="/stats/projects/all/R_Tools_Development/data/salaries_data.Rds",filetype="Rds"))
#' 


filetype <- function(data,filetype="dataframe") {
  
  if (tolower(filetype) == "dataframe") {data = data}  else if (tolower(filetype) == "rds") {
    data = readRDS(data)}  else if (tolower(filetype) == "xls") {
      data = read.xlsx(data, sheetName = 1, header = TRUE)}  else if (tolower(filetype) == "xlsx") {
        data =  read.xlsx(data, sheetName = 1, header = TRUE)}  else if (tolower(filetype) == "csv") {
          data = read.csv(data,header = TRUE, sep = ",")}  else stop("ERROR: File type is not compatible")
  
 
  return(data)
  
}