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

#' Generate sample sheet for DRAGEN TSO500 Anaylsis Pipeline input based on TSO500 sample sheet.
#'
#' @param samplesheet path to TSO500 sample sheet
#' @param file_format_version the version of the file format (default: 2)
#' @param run_name the run name (default: RunName)
#' @param instrument_type path to output file
#' @param software_version The software version (default: 3.10.9)
#' @param adapter_read1 index read1 sequence
#' @param adapter_read2 index read2 sequence
#' @param adapter_behavior this indicates that the BCL Convert software trims the specified adapter sequences from each read (default: trim)
#' @param minimum_trimmed_read_length reads with a length trimmed below this point are masked (default: 35)
#' @param mask_short_reads reads with a length trimmed below this point are masked (default: 35)
#' @param outfile path to output CSV file
#'
#' @return
#'
#' @examples
generate_dragen_samplesheet <- function(samplesheet, file_format_version="2", run_name="RunName", instrument_type="NovaSeq", software_version="3.10.9", adapter_read1, adapter_read2, adapter_behavior="trim", minimum_trimmed_read_length=35, mask_short_reads=35, outfile){
  # dragen sample sheet templates
  dragen_samplesheet_header = "[Header],,,,,,,
FileFormatVersion,${file_format_version},,,,,,
RunName,${run_name},,,,,,
InstrumentType,${instrument_type},,,,,,
,,,,,,,"
  
  dragen_samplesheet_reads = "[Reads],,,,,,,
Read1Cycles,101,,,,,,
Read2Cycles,101,,,,,,
Index1Cycles,10,,,,,,
Index2Cycles,10,,,,,,
,,,,,,,"
  
  dragen_samplesheet_bclconvert = "[BCLConvert_Settings],,,,,,,
SoftwareVersion,${software_version},,,,,,
AdapterRead1,${adapter_read1},,,,,,
AdapterRead2,${adapter_read2},,,,,,
AdapterBehavior,${adapter_behavior},,,,,,
MinimumTrimmedReadLength,${minimum_trimmed_read_length},,,,,,
MaskShortReads,${mask_short_reads},,,,,,
,,,,,,,"
  
  dragen_samplesheet_bclconvert_data = "[BCLConvert_Data],,,,,,,
${bclconvert_data}
,,,,,,,
[TSO500S_Settings],,,,,,,
,,,,,,,"
  
  dragen_samplesheet_tso500_data = "[TSO500S_Data],,,,,,,
${tso500_data}"
  
  # Read text file into string
  cvo_file <- readr::read_file(samplesheet)
  split_cvo_string <- stringr::str_split(string = cvo_file, pattern = "\r\n") %>% unlist()
  
  # parse header part of provided sample sheet
  start_header <- match("[Header],,,,,,,,",split_cvo_string) + 1
  end_header <- which(grepl('Chemistry', split_cvo_string))
  header <- read.csv(text=split_cvo_string[start_header:end_header],header=FALSE) %>%
    purrr::discard(~all(is.na(.))) %>%
    pivot_wider(names_from=V1, values_from=V2)
  
  # parse settings part of provided sample sheet
  start_settings <- match("[Settings],,,,,,,,",split_cvo_string) + 1
  end_settings <- which(grepl('OverrideCycles', split_cvo_string))
  settings <- read.csv(text=split_cvo_string[start_settings:end_settings],header=FALSE) %>%
    purrr::discard(~all(is.na(.))) %>%
    pivot_wider(names_from=V1, values_from=V2)
  
  # prase data part of provided sample sheet
  start_data <- match("[Data],,,,,,,,",split_cvo_string) + 1
  data <- read.csv(text=split_cvo_string[start_data:length(split_cvo_string)])
  
  # transform bclconvert data
  bclconvert_data <- data %>%
    mutate(Sample_ID = paste(Sample_ID,Index_ID,sep="_")) %>%
    select(c(Sample_ID,index,index2)) %>%
    add_column(col_name1 = "",
               col_name2 = "",
               col_name3 = "",
               col_name4 = "",
               col_name5 = "") %>%
    format_csv()
  
  # transform tso500 data
  tso500_data <- data %>%
    select(c(Sample_ID,Index_ID,Sample_Plate,Sample_Well,Sample_Type,Pair_ID)) %>%
    mutate(Sample_ID = paste(Sample_ID,Index_ID,sep="_")) %>%
    mutate(Pair_ID = Sample_ID) %>%
    add_column(Sample_Feature = "") %>%
    add_column(Sample_Description = "") %>%
    replace(is.na(.), "") %>%
    format_csv()
  
  # replace values accordingly in template strings
  # content for the sample sheet header
  dragen_header_string <- stringr::str_interp(dragen_samplesheet_header, list(file_format_version=file_format_version, run_name=run_name, instrument_type=instrument_type))
  
  # content for the bcl convert settings section
  bclconvert_string <- stringr::str_interp(dragen_samplesheet_bclconvert, list(software_version=software_version,adapter_read1=adapter_read1,adapter_read2=adapter_read2,adapter_behavior=adapter_behavior,minimum_trimmed_read_length=minimum_trimmed_read_length,mask_short_reads=mask_short_reads))
  
  # content for the bcl convert data section
  bclconvert_data_string <- stringr::str_interp(dragen_samplesheet_bclconvert_data, list(bclconvert_data=str_sub(bclconvert_data, end = -2)))
  bclconvert_data_string <- str_replace_all(bclconvert_data_string, "col_name\\w+", "")
  
  # content for the TSO500 data section
  tso500_data_string <- stringr::str_interp(dragen_samplesheet_tso500_data, list(tso500_data=str_sub(tso500_data, end = -2)))
  
  # write csv file
  cat(paste(dragen_header_string,dragen_samplesheet_reads,bclconvert_string,bclconvert_data_string,tso500_data_string, sep="\n"), file = outfile)
}
