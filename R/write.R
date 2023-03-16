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

#' Save data frames to RData object.
#'
#' @param data_frames list of data frames
#' @param file_path path to output file
#'
#' @return
#'
#' @examples
write_rdata_file <- function(data_frames, file_path){
  save(data_frames, file=file_path)
}

#' Write files needed for reporting in MultiQC.
#' @param analysis_details_frame data frame with analysis details (get_analysis_details_df)
#' @param sequencing_run_details_frame data frame with sequencing run details (get_sequencing_run_details_df)
#' @param variant_stats data frame with variant type statistics (get_count_df)
#' @param tmb_msi_data_frame data frame with tmb and msi statistics (get_metrics_df)
#' @param file_path path to output file
#'
#' @return
#'
#' @examples
write_multiqc_data <- function(analysis_details_frame, sequencing_run_details_frame, variant_stats, tmb_msi_data_frame, folder_path){
  write.table(analysis_details_frame, paste0(folder_path, "analysis_details.txt"), row.names = FALSE)
  write.table(sequencing_run_details_frame, paste0(folder_path, "sequencing_run_details.txt"), row.names = FALSE)
  write.table(variant_stats, paste0(folder_path, "variant_stats.txt"), row.name = FALSE)
  write.table(tmb_msi_data_frame, paste0(folder_path, "tmb_msi_statistics.txt"), row.names = FALSE)
}
