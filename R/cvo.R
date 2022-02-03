#' Read in a CombinedVariantOutput.tsv file and store as an object
#'
#' @description Read in a CombinedVariantOutput.tsv file
#'
#' @param cvo_file_path a file path to a CombinedVariantOutput.tsv file
#'
#' @return A combined.variant.output object
#' @export
#'
#' @examples
cvo <- function(cvo_file_path){
  new_combined_variant_output(cvo_file_path)
}

#' Constructor function for combined.variant.output objects
#' Not to be called directly
#'
#' @param cvo_file_path a file path to a CombinedVariantOutput.tsv file
#'
#' @return A combined.variant.output object
new_combined_variant_output <- function(cvo_file_path) {

  cvo_file <- readr::read_file(cvo_file_path)
  split_cvo_string <- stringr::str_split(string = cvo_file, pattern = "\\[") %>% unlist()

  # handle the parts of the file that are structured as key-value pairs
  # i.e. metadata and TMB/MSI sections
  records <- purrr::map(split_cvo_string[2:5], parse_record)
  names(records) <- c("analysis_details", "sequencing_run_details", "tmb", "msi")

  # handle the parts of the file that are structured as tabular data
  # i.e. gene amplifications, splice variants, fusions, and small variants
  tables <- purrr::map(split_cvo_string[6:9], parse_table)
  names(tables) <- c("gene_amplifications", "splice_variants", "fusions", "small_variants")

  return(structure(c(records, tables), class = "combined.variant.output"))
}

#' Validator function for combined.variant.output constructor
#' Not to be called directly
#' NOT IMPLEMENTED
#'
#' @return
validate_tso500 <- function() {}

#' Read in a batch of CombinedVariantOutput.tsv files into a list
#'
#' @param cvo_directory a file path to a directory containing one of more
#' CombinedVariantOutput.tsv files
#'
#' @return A named list of combined.variant.output objects
#' @export
read_cvo_data <- function(cvo_directory){
  cvo_files <- list.files(
    path = cvo_directory,
    pattern = "*CombinedVariantOutput.tsv",
    full.names = TRUE
  )
  cvo_data <- map(cvo_files, cvo)
  names(cvo_data) <- map(cvo_data, ~ .x$analysis_details$pair_id)
  cvo_data
}

#' Extract small variants from combined.variant.output object and
#' return in data frame format
#'
#' @param cvo_obj cvo_obj
#'
#' @return A data frame of small variants
#' @export
get_small_variants <- function(cvo_obj, ...){
  UseMethod("get_small_variants", cvo_obj)
}

#' Get small variants from combined.variant.output object
#'
#' @param cvo_obj cvo_obj
#' @return A data frame
#' @method get_small_variants combined.variant.output
#' @export
get_small_variants.combined.variant.output <- function(cvo_obj){
  if(is.na(cvo_obj$small_variants)){
    small_variant_df <- data.frame()
  } else {
    small_variant_df <- cvo_obj$small_variants %>%
      dplyr::mutate(sample_id = cvo_obj$analysis_details$pair_id) %>%
      dplyr::select(sample_id, tidyr::everything())
  }
  return(small_variant_df)
}

#' Helper function to parse key-value lines in CombinedVariantOutput.tsv
#'
#' @param record_string
#'
#' @return char vector
parse_record <- function(record_string){

  intermediate <- record_string %>%
    trim_header_and_footer() %>%
    stringr::str_split("\n") %>%
    unlist() %>%
    stringr::str_remove("\\t$") %>%
    stringr::str_split("\\t")

  if(stringr::str_detect(record_string, "TMB|MSI")){
    record <- purrr::map(intermediate, ~ as.numeric(.x[2]))
  } else {
    record <- purrr::map(intermediate, ~ .x[2])
  }

  record_names <- purrr::map_chr(intermediate, ~ .x[1])
  names(record) <- janitor::make_clean_names(record_names)
  return(record)
}

#' Helper function to parse tabular data in CombinedVariantOutput.tsv
#'
#' @param table_string
#'
#' @return data.frame
parse_table <- function(table_string){

  intermediate <- table_string %>% trim_header_and_footer()
  header_line <- stringr::str_extract(intermediate, ".+\n")

  if(stringr::str_detect(header_line, "\\t\\n")){
    intermediate <- stringr::str_replace_all(
      string = intermediate,
      pattern = "\\t\\n",
      replacement = "\n")
  }

  table_data <- intermediate %>% handle_empty_table_values()
  return(table_data)
}

#' Helper function to handle empty rows
#' in CombinedVariantOutput.tsv tabular data
#'
#' @param intermediate_tbl
#'
#' @return data.frame
handle_empty_table_values <- function(intermediate_tbl){
  if(stringr::str_detect(string = intermediate_tbl, pattern = "\\nNA$")){
    return(NA)
  } else {
    to_clean <- utils::read.table(text = intermediate_tbl, sep = "\t", header = TRUE)
    clean_name_df <- janitor::clean_names(to_clean)
    return(clean_name_df)
  }
}

#' Helper function to remove header and footer from CombinedVariantOutput.tsv
#'
#' @param string
#'
#' @return char vector
trim_header_and_footer <- function(string){
  s %>%
    stringr::str_remove(".+\\t\\t\\n") %>%
    stringr::str_remove("[\\n\\t]+$")
}
