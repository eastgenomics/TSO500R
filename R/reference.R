#' Read GEL reference Excel spreadsheet into a list of data frames, per sheet
#'
#' @param reference_path Path to reference .xlsx
#'
#' @return list of data.frame
#' @export
read_reference <- function(reference_path){
  sheets <- c("Hotspots", "OPA", "Census", "Colon", "Histiocytosis", "Glioblastoma", "GIST")
  reference <- map(sheets, ~ readxl::read_excel(reference_path, sheet = .x) %>% janitor::clean_names())
  names(reference) <- sheets
  return(reference)
}
