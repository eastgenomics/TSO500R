#' Save data frames to Microsoft Excel workbook
#'
#' @description Store data frames to Microsoft Excel workbook. Submission of
#'   more than one data frame will result in a workbook with that many sheets.
#'   The number of worksheet names _must_ match the number of data frames
#'   submitted.
#'
#' @param workbook_name Filename of the workbook
#' @param data_frames list of data frames
#' @param sheet_names character vector of worksheet names
#'
#' @export
write_workbook <- function(workbook_name, data_frames, sheet_names){
  sheet_indices <- seq(length(data_frames))
  workbook <- openxlsx::createWorkbook()
  purrr::pwalk(
    .l = list(data_frames, sheet_names, sheet_indices),
    .f = function(x, y, z) write_worksheet(x, y, z, workbook)
  )
  openxlsx::saveWorkbook(wb = workbook,
                         file = paste0(workbook_name, "_", Sys.Date(), ".xlsx"), overwrite = TRUE)
}

#' Write worksheet to Excel workbook.
#'
#' @param data
#' @param sheet_name
#' @param sheet_index
#' @param workbook
#'
#' @return
#'
#' @examples
write_worksheet <- function(data, sheet_name, sheet_index, workbook){
  openxlsx::addWorksheet(wb = workbook, sheetName = sheet_name, gridLines = TRUE)
  openxlsx::writeDataTable(wb = workbook, sheet = sheet_index, x = data)
}
