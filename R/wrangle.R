#' Read in a list of combined.variant.output objects
#' and return a data frame of small variants per sample
#'
#' @param cvo_list a list of combined.variant.output objects
#'
#' @return A data frame of small variants extracted from
#' each combined.variant.output object, per sample
#'
#' @export
read_small_variants <- function(cvo_list){

  small_variants <- purrr::map(cvo_list, get_small_variants) %>%
    dplyr::bind_rows()

  return(small_variants)
}

#' Process small variant data-frame to requirements
#'
#' @description Processes small-variant data to comply with requirements for
#' further analysis. The function:
#'
#'     1. filters for variants that:
#'       - have a consequence in a pre-defined list (see details)
#'       - are present with depth > 0
#'     2. extracts NP ID and protein information from the P Dot-notation column
#'     3. adds columns to faciliate addition of reference data
#'
#' @details The following variant consequences are currently included:
#' - frameshift_variant
#' - inframe_deletion
#' - inframe_insertion
#' - missense_variant
#' - missense_variant:splice_region_variant
#' - splice_acceptor_variant
#' - splice_donor_variant
#' - splice_donor_variant:intron_variant
#' - start_lost
#' - stop_gained
#' - stop_gained:splice_region_variant
#' - stop_lost
#'
#' @param small_variant_df Data-frame of small variants
#' @return
#' @export
process_small_variant_data <- function(small_variant_df){

  variant_consequences <- c("frameshift_variant", "inframe_deletion",
                            "inframe_insertion", "missense_variant",
                            "missense_variant:splice_region_variant",
                            "splice_acceptor_variant", "splice_donor_variant",
                            "splice_donor_variant:intron_variant", "start_lost",
                            "stop_gained", "stop_gained:splice_region_variant",
                            "stop_lost")

  updated_df <- small_variant_df %>%
    filter_consequences(variant_consequences) %>%
    filter_depth(depth_limit = 0) %>%
    parse_p_dot_notation() %>%
    update_ref_join_columns()

  return(updated_df)
}

#' Filters small variant data for specified variant consequences
#'
#' @param small_variant_df
#' @param consequences
#'
#' @return data.frame
#' @export
filter_consequences <- function(small_variant_df, consequences){
  filtered_df <- small_variant_df %>% dplyr::filter(consequence_s %in% consequences)
  return(filtered_df)
}

#' Helper function to filter small variant data according
#' to specified depth
#'
#' @param small_variant_df
#' @param depth_limit
#'
#' @return
filter_depth <- function(small_variant_df, depth_limit = 0){
  filtered_df <- dplyr::filter(small_variant_df, depth > depth_limit)
  return(filtered_df)
}

#' Parses P-Dot notation column, splitting it into distinct NP ID and
#' amino acid variant columns
#'
#' @param small_variant_df
#'
#' @return
#' @export
parse_p_dot_notation <- function(small_variant_df){
  tidyr::extract(
    data = small_variant_df,
    col = p_dot_notation,
    into = c("np_id", "protein"),
    regex = "(NP_.+):.+\\((\\w{3}\\d+).+$",
    remove = FALSE
  )
}

#' Helper function to make small variant DF joinable to reference data
#'
#' @param small_variant_df
#'
#' @return data frame
update_ref_join_columns <- function(small_variant_df){
  mutated_df <- small_variant_df %>%
    dplyr::mutate(
      protein = paste(gene, protein, sep = "_"),
      coord_id = paste(chromosome, genomic_position, sep = "_")
    )
  return(mutated_df)
}

#' Adds GEL reference data to small variant data
#'
#' @param small_variant_df
#' @param reference_data_list
#'
#' @return
#' @export
add_reference_data <- function(small_variant_df, reference_data_list){
  key_order = c("protein", "coord_id", "gene", "gene", "gene", "gene", "gene")
  to_join <- c(list(small_variant_df), reference_data_list)
  joined_data <- purrr::reduce2(.x = to_join, .y = key_order, .f = left_join)
  return(joined_data)
}

#' Helper function to extract TMB/MSI metrics from
#' list of combined.variant.output objects
#'
#' @param cvo_data
#' @param category
#'
#' @return data.frame
extract_metrics <- function(cvo_data, category = "tmb"){
  df <- purrr::map_dfr(cvo_data, purrr::pluck, category)
  return(df)
}

#' Extracts all TMB/MSI metrics from list of combined.variant.output
#' objects, returning a data frame of TMB/MSI per sample
#'
#' @param cvo_data
#'
#' @return data.frame
#' @export
get_metrics_df <- function(cvo_data){
  tmb_df <- extract_metrics(cvo_data, category = "tmb")
  msi_df <- extract_metrics(cvo_data, category = "msi")
  metrics_df <- dplyr::bind_cols(tmb_df, msi_df) %>%
    dplyr::mutate(sample_id = purrr::map_chr(cvo_data, ~ .x$analysis_details$pair_id)) %>%
    dplyr::select(sample_id, tidyr::everything())
  return(metrics_df)
}
