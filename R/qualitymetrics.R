#' Read in a MetricsOutput.tsv file and store as an object
#'
#' @description Read in a MetricsOutput.tsv file
#'
#' @param metrics_file_path a file path to a MetricsOutput.tsv file
#' @param local_app specifies whether quality metrics are coming from local app
#'
#' @return A quality.metrics.output object
#' @export
#'
#' @examples
qualitymetrics <- function(metrics_file_path, local_app=FALSE){
  new_combined_quality_metrics_output(metrics_file_path, local_app)
}

#' Constructor function for quality.metrics.output objects
#' Not to be called directly
#'
#' @param metrics_file_path a file path to a MetricsOutput.tsv file
#' @param local_app specifies whether quality metrics are coming from local app
#'
#' @return A quality.metrics.output object
new_combined_quality_metrics_output <- function(metrics_file_path, local_app=FALSE) {

  qm_file <- readr::read_file(metrics_file_path)
  split_qmo_string <- stringr::str_split(string = qm_file, pattern = "\\[") %>% unlist()

  # handle the parts of the file that are structured as key-value pairs
  # i.e. header and notes sections
  records <- purrr::map(split_qmo_string[c(2,12)], parse_record)
  names(records) <- c("header", "notes")

  # handle the parts of the file that are structured as tabular data
  # i.e. run qc metrics, analysis status etc. 
  tables <- purrr::map(split_qmo_string[3:11], parse_table)

  # the order of tables is different between local app and DRAGEN analysis pipeline
  names(tables) <- c("run_qc_metrics", "analysis_status", "dna_qc_metrics", "dna_qc_metrics_snvtmb", "dna_qc_metrics_msi", "dna_qc_metrics_cnv", "rna_qc_metrics", "dna_expanded_metrics", "rna_expanded_metrics")
  
  if (local_app) {
    names(tables) <- c("run_qc_metrics", "analysis_status", "dna_qc_metrics", "dna_qc_metrics_snvtmb", "dna_qc_metrics_msi", "dna_qc_metrics_cnv", "dna_expanded_metrics", "rna_qc_metrics", "rna_expanded_metrics")
  }

  return(structure(c(records, tables), class = "combined.quality.metrics.output"))
}

#' Validator function for quality.metrics.output constructor
#' Not to be called directly
#' NOT IMPLEMENTED
#'
#' @return
validate_tso500_qc <- function() {}

#' Read in a batch of MetricsOutput.tsv files into a list
#'
#' @param qmo_directory a file path to a directory containing one of more
#' MetricsOutput.tsv files
#' @param local_app specifies whether quality metrics are coming from local app (default: FALSE)
#'
#' @return A named list of combined.quality.metrics.output objects
#' @export
read_qmo_data <- function(qmo_directory, local_app=FALSE){
  qmo_files <- list.files(
    path = qmo_directory,
    pattern = "*MetricsOutput.tsv",
    full.names = TRUE
  )
  qmo_data <- map(qmo_files, qualitymetrics, local_app)
  names(qmo_data) <- map(qmo_data, ~ .x$header$output_time)
  qmo_data
}

#' Extract run qc metrics from combined.quality.metrics.output object and
#' return in data frame format
#'
#' @param qmo_obj qmo_obj
#'
#' @return A data frame with run qc metrics
#' @export
get_run_qc_metrics <- function(qmo_obj, ...){
  UseMethod("get_run_qc_metrics", qmo_obj)
}

#' Extract the analysis status from combined.quality.metrics.output object and
#' return in data frame format
#'
#' @param qmo_obj qmo_obj
#'
#' @return A data frame with the analysis status
#' @export
get_analysis_status <- function(qmo_obj, ...){
  UseMethod("get_analysis_status", qmo_obj)
}

#' Extract dna qc metrics from combined.quality.metrics.output object and
#' return in data frame format
#'
#' @param qmo_obj qmo_obj
#'
#' @return A data frame with the dna qc metrics
#' @export
get_dna_qc_metrics <- function(qmo_obj, ...){
  UseMethod("get_dna_qc_metrics", qmo_obj)
}

#' Extract dna qc metrics for small variants and tmb from combined.quality.metrics.output object and
#' return in data frame format
#'
#' @param qmo_obj qmo_obj
#'
#' @return A data frame with dna qc metrics (small variants/TMB)
#' @export
get_dna_qc_metrics_snvtmb <- function(qmo_obj, ...){
  UseMethod("get_dna_qc_metrics_snvtmb", qmo_obj)
}

#' Extract dna qc metrics for msi from combined.quality.metrics.output object and
#' return in data frame format
#'
#' @param qmo_obj qmo_obj
#'
#' @return A data frame with dna qc metrics (MSI)
#' @export
get_dna_qc_metrics_msi <- function(qmo_obj, ...){
  UseMethod("get_dna_qc_metrics_msi", qmo_obj)
}

#' Extract dna qc metrics for CNV from combined.quality.metrics.output object and
#' return in data frame format
#'
#' @param qmo_obj qmo_obj
#'
#' @return A data frame with dna qc metrics (CNV)
#' @export
get_dna_qc_metrics_cnv <- function(qmo_obj, ...){
  UseMethod("get_dna_qc_metrics_cnv", qmo_obj)
}

#' Extract expanded dna qc metrics from combined.quality.metrics.output object and
#' return in data frame format
#'
#' @param qmo_obj qmo_obj
#'
#' @return A data frame with extended dna qc metrics
#' @export
get_dna_expanded_metrics <- function(qmo_obj, ...){
  UseMethod("get_dna_expanded_metrics", qmo_obj)
}

#' Extract rna qc metrics from combined.quality.metrics.output object and
#' return in data frame format
#'
#' @param qmo_obj qmo_obj
#'
#' @return A data frame with rna qc metrics
#' @export
get_rna_qc_metrics <- function(qmo_obj, ...){
  UseMethod("get_rna_qc_metrics", qmo_obj)
}

#' Extract expanded rna qc metrics combined.quality.metrics.output object and
#' return in data frame format
#'
#' @param qmo_obj qmo_obj
#'
#' @return A data frame with expanded rna qc metrics
#' @export
get_rna_expanded_metrics <- function(qmo_obj, ...){
  UseMethod("get_rna_expanded_metrics", qmo_obj)
}

#' Get run qc metrics from combined.quality.metrics.output object
#'
#' @param qmo_obj qmo_obj
#' @return A data frame
#' @method get_run_qc_metrics combined.quality.metrics.output
#' @export
get_run_qc_metrics.combined.quality.metrics.output <- function(qmo_obj){
  suppressWarnings(
    if(all(is.na(qmo_obj$run_qc_metrics))){
      run_qc_metrics_df <- data.frame()
    } else {
      run_qc_metrics_df <- qmo_obj$run_qc_metrics %>% 
        select(metric_uom, lsl_guideline, usl_guideline, value)
    }
  )
  return(run_qc_metrics_df)
}

#' Get analysis status from combined.quality.metrics.output object
#'
#' @param qmo_obj qmo_obj
#' @return A data frame
#' @method get_analysis_status combined.quality.metrics.output
#' @export
get_analysis_status.combined.quality.metrics.output <- function(qmo_obj){
  suppressWarnings(
    if(all(is.na(qmo_obj$analysis_status))){
      analysis_status_df <- data.frame()
    } else {
      analysis_status_df <- qmo_obj$analysis_status %>%
        rename(metric = x) %>%
        mutate(across(is.logical, ~as.character(.x))) %>% #otherwise pivot_longer will fail due to logical + character
        pivot_longer(!metric, names_to = "sample_id") %>%
        pivot_wider(names_from = metric, values_from = value)
    }
  )
  return(analysis_status_df)
}

#' Get dna qc metrics from combined.quality.metrics.output object
#'
#' @param qmo_obj qmo_obj
#' @return A data frame
#' @method get_dna_qc_metrics combined.quality.metrics.output
#' @export
get_dna_qc_metrics.combined.quality.metrics.output <- function(qmo_obj){
    suppressWarnings(
        if(all(is.na(qmo_obj$dna_qc_metrics))){
            dna_qc_metrics_df <- data.frame()
        } else {
            dna_qc_metrics_df <- qmo_obj$dna_qc_metrics %>% 
                pivot_longer(!metric_uom, names_to = "sample_id") %>%
                pivot_wider(names_from = metric_uom)
        }
    )
    return(dna_qc_metrics_df)
}

#' Get dna qc metrics for small variants and tmb from combined.quality.metrics.output object
#'
#' @param qmo_obj qmo_obj
#' @return A data frame
#' @method get_dna_qc_metrics_snvtmb combined.quality.metrics.output
#' @export
get_dna_qc_metrics_snvtmb.combined.quality.metrics.output <- function(qmo_obj){
    suppressWarnings(
        if(all(is.na(qmo_obj$dna_qc_metrics_snvtmb))){
            dna_qc_metrics_snvtmb_df <- data.frame()
        } else {
            dna_qc_metrics_snvtmb_df <- qmo_obj$dna_qc_metrics_snvtmb %>%
                pivot_longer(!metric_uom, names_to = "sample_id") %>%
                pivot_wider(names_from = metric_uom)
        }
    )
    return(dna_qc_metrics_snvtmb_df)
}

#' Get dna qc metrics for msi from combined.quality.metrics.output object
#'
#' @param qmo_obj qmo_obj
#' @return A data frame
#' @method get_dna_qc_metrics_msi combined.quality.metrics.output
#' @export
get_dna_qc_metrics_msi.combined.quality.metrics.output <- function(qmo_obj){
    suppressWarnings(
        if(all(is.na(qmo_obj$dna_qc_metrics_msi))){
            dna_qc_metrics_msi_df <- data.frame()
        } else {
            dna_qc_metrics_msi_df <- qmo_obj$dna_qc_metrics_msi %>%
                pivot_longer(!metric_uom, names_to = "sample_id") %>%
                pivot_wider(names_from = metric_uom)
        }
    )
    return(dna_qc_metrics_msi_df)
}

#' Get dna qc metrics for cnv from combined.quality.metrics.output object
#'
#' @param qmo_obj qmo_obj
#' @return A data frame
#' @method get_dna_qc_metrics_cnv combined.quality.metrics.output
#' @export
get_dna_qc_metrics_cnv.combined.quality.metrics.output <- function(qmo_obj){
    suppressWarnings(
        if(all(is.na(qmo_obj$dna_qc_metrics_cnv))){
            dna_qc_metrics_cnv_df <- data.frame()
        } else {
            dna_qc_metrics_cnv_df <- qmo_obj$dna_qc_metrics_cnv %>%
                pivot_longer(!metric_uom, names_to = "sample_id") %>%
                pivot_wider(names_from = metric_uom)
        }
    )
    return(dna_qc_metrics_cnv_df)
}

#' Get expanded dna qc metrics from combined.quality.metrics.output object
#'
#' @param qmo_obj qmo_obj
#' @return A data frame
#' @method get_dna_expanded_metrics combined.quality.metrics.output
#' @export
get_dna_expanded_metrics.combined.quality.metrics.output <- function(qmo_obj){
    suppressWarnings(
        if(all(is.na(qmo_obj$dna_expanded_metrics))){
            dna_expanded_metrics_df <- data.frame()
        } else {
            dna_expanded_metrics_df <- qmo_obj$dna_expanded_metrics %>%
                pivot_longer(!metric_uom, names_to = "sample_id") %>%
                pivot_wider(names_from = metric_uom)
        }
    )
    return(dna_expanded_metrics_df)
}

get_rna_qc_metrics

#' Get rna qc metrics from combined.quality.metrics.output object
#'
#' @param qmo_obj qmo_obj
#' @return A data frame
#' @method get_rna_qc_metrics combined.quality.metrics.output
#' @export
get_rna_qc_metrics.combined.quality.metrics.output <- function(qmo_obj){
    suppressWarnings(
        if(all(is.na(qmo_obj$rna_qc_metrics))){
            rna_qc_metrics_df <- data.frame()
        } else {
            rna_qc_metrics_df <- qmo_obj$rna_qc_metrics %>%
                pivot_longer(!metric_uom, names_to = "sample_id") %>%
                pivot_wider(names_from = metric_uom)
        }
    )
    return(rna_qc_metrics_df)
}

#' Get expanded rna qc metrics from combined.quality.metrics.output object
#'
#' @param qmo_obj qmo_obj
#' @return A data frame
#' @method get_rna_expanded_metrics combined.quality.metrics.output
#' @export
get_rna_expanded_metrics.combined.quality.metrics.output <- function(qmo_obj){
    suppressWarnings(
        if(all(is.na(qmo_obj$rna_expanded_metrics))){
            rna_expanded_metrics_df <- data.frame()
        } else {
            rna_expanded_metrics_df <- qmo_obj$rna_expanded_metrics %>%
                pivot_longer(!metric_uom, names_to = "sample_id") %>%
                pivot_wider(names_from = metric_uom)
        }
    )
    return(rna_expanded_metrics_df)
}

#' Helper function to parse key-value lines in MetricsOutput.tsv
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

  record <- purrr::map(intermediate, ~ .x[2])
  record_names <- purrr::map_chr(intermediate, ~ .x[1])
  names(record) <- janitor::make_clean_names(record_names)
  return(record)
}

#' Helper function to parse tabular data in MetricsOutput.tsv
#'
#' @param table_string
#'
#' @return data.frame
parse_table <- function(table_string){

  intermediate <- table_string %>% 
    trim_header_and_footer()
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

#' Helper function to handle empty rows in MetricsOutput.tsv tabular data
#'
#' @param intermediate_tbl
#'
#' @return data.frame
handle_empty_table_values <- function(intermediate_tbl){
  if(stringr::str_detect(string = intermediate_tbl, pattern = "\\nNA$")){
    return(NA)
  } else {
    to_clean <- utils::read.table(text = intermediate_tbl, sep = "\t", header = TRUE, fill = TRUE)
    clean_name_df <- janitor::clean_names(to_clean)
    return(clean_name_df)
  }
}

#' Helper function to remove header and footer from MetricsOutput.tsv
#'
#' @param string
#'
#' @return char vector
trim_header_and_footer <- function(string){
  string %>%
    stringr::str_remove(".+\\t\\t\\n") %>%
    stringr::str_remove_all("[\\t]{2,}") %>%
    stringr::str_remove("[\\n\\t]+$")
}