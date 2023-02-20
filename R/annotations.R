#' Read GEL annotation Excel spreadsheet into a list of data frames, per sheet
#'
#' @param annotation_data_path Path to annotation .xlsx
#'
#' @return list of data.frame
#' @export
read_annotation_data <- function(annotation_data_path){
  sheets <- c("Hotspots", "OPA", "Census", "Colon", "Histiocytosis", "Glioblastoma", "GIST")
  annotations <- map(sheets, ~ readxl::read_excel(annotation_data_path, sheet = .x) %>% janitor::clean_names())
  names(annotations) <- sheets
  return(annotations)
}
