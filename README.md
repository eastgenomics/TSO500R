# TSO500R

An R package for importing and working with CombinedVariantOutput.csv files produced by TSO500 local app.

## Installation

Clone the repo locally and run:

```r
install.packages("/path/to/TSO500R", repos = NULL)
```

## Usage

TSO500R features an API for interaction with the data in each section of the TSO500 input and output files. First load the package:

```r
library(tso500R)
```

### Loading data

To load a batch of CombinedVariantOutput.tsv files, run the following command on the directory containing the files:

```r
cvo_data_dir <- "test_data/cvo_files/211110_A01295_0033_AHLHC5DRXY_TSO500/"
cvo_data <- read_cvo_data(cvo_data_dir)
```

This will produce a list of `combined.variant.output` objects, which can be further processed. Each section in the CombinedVariantOutput file can be accessed with standard list indexing; for example:

```r

```

### TMB/MSI metrics

Run the following function to extract TMB/MSI data from the batch data:

```r
tmb_msi_df <- get_metrics_df(cvo_data)
```

This will produce a data frame with TMB/MSI metrics, per sample.

### Small variants

You can also extract all of the small variant data across all of the objects. The following function will return a `data.frame` of small variants:

```r
small_variant_df <- read_small_variants(cvo_data)
```

#### Filtering/processing

A processing/filtering function is available, which performs the following:

  1. filters for variants that:
   - have a consequence in a pre-defined list
   - are present with depth > 0
  2. extracts NP ID and protein information from the P Dot-notation column
  3. adds columns to faciliate addition of reference data

The following variant consequences are included:

  - frameshift_variant
  - inframe_deletion
  - inframe_insertion
  - missense_variant
  - missense_variant:splice_region_variant
  - splice_acceptor_variant
  - splice_donor_variant
  - splice_donor_variant:intron_variant
  - start_lost
  - stop_gained
  - stop_gained:splice_region_variant
  - stop_lost

To use the function:

```r
processed_small_variant_df <- process_small_variant_data(small_variant_df)
```

#### Adding reference data

Reference data can be added to the small variant data frame.

```r
ref_data_filepath <- "test_data/TSO_Reference.xlsx"
flowcell_barcode <- str_extract(cvo_data_dir, "AH.+XY")
reference_data <- read_reference(ref_data_filepath)
```

#### Plotting

Plotting functions are also included. These are:

- Plot AF per variant consequence
- Plot AF KDE per sample
- Plot AF histogram per sample

The basic usage is as follows:

```r
plot_af_per_variant_consequence(small_variant_df)
```

You can also add commonly-used theme elements, if you prefer:

```r
plot_af_per_variant_consequence(small_variant_df) %>%
  add_common_theme_elements()
```

Example
```r
ref_data_filepath <- "test_data/TSO_Reference.xlsx"
flowcell_barcode <- str_extract(cvo_data_dir, "AH.+XY")

ref <- read_reference(ref_data_filepath)

small_variant_df <- read_small_variants(cvo_data) %>%
  process_small_variant_data() #%>%
  #add_reference_data(reference_data_list = ref)


metrics_df <- get_metrics_df(cvo_data)
tmb_metric_names <- c("total_tmb", "coding_region_size_in_megabases", "number_of_passing_eligible_variants")
msi_metric_names <- c("usable_msi_sites", "total_msi_sites_unstable", "percent_unstable_msi_sites")

data_to_write <- list(metrics_df %>% select(, all_of(msi_metric_names)),
                      metrics_df %>% select(sample_id, all_of(tmb_metric_names)),
                      small_variant_df)

names(data_to_write) <- paste0(flowcell_barcode, c("_MSI", "_TMB", "_Nonsyn"))

write_workbook("test", data_frames = data_to_write, sheet_names = names(data_to_write))
```

#### Multiple files

### Processing

Alternative

### Reference data
