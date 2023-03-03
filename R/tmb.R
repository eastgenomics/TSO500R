#' Read in a TMB_trace.tsv file and store as an object
#'
#' @description Read in a TMB_trace.tsv file
#'
#' @param tmb_file_path a file path to a TMB_trace.tsv file
#'
#' @return A tmb.variant.output object
#' @export
#'
#' @examples
tmb <- function(tmb_file_path){
  new_tmb_variant_output(tmb_file_path)
}

#' Read in a batch of TMB_trace.tsv files into a list
#'
#' @param tmb_directory a file path to a directory containing one of more
#' TMB_trace.tsv files
#'
#' @return A named list of data frame objects
#' @export
read_tmb_trace_data <- function(tmb_directory){
  tmb_files <- list.files(
    path = tmb_directory,
    pattern = "*TMB_Trace.tsv",
    full.names = TRUE
  )
  
  tmb_data = tibble(file = tmb_files) %>%
    mutate(data = lapply(file, read_tsv)) %>%
    unnest(data) %>%
    mutate(sample_id = str_replace(basename(file), "_TMB_Trace.tsv", "")) %>%
    select(-file) %>%
    relocate(sample_id)

  tmb_data
}