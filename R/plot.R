#' Plot allele-frequency for small variants per variant consequence
#'
#' @description Plot AF per-sample, grouped by variant consequence
#' @param small_variant_df Data-frame containing small variants
#'
#' @return ggplot object
#' @export
plot_af_per_variant_consequence <- function(small_variant_df){
  g <- ggplot(data = small_variant_df) +
    geom_jitter(aes(x = sample_id, y = allele_frequency, colour = consequence_s),
                size= 2, shape = 19, alpha = 0.7) +
    facet_wrap(~consequence_s, ncol = 5) +
    theme(
      panel.grid.major = element_line(color = "grey", linetype = 3, size = 0.1),
      strip.background = element_blank(),
    )
  return(g)
}

#' Plot allele-frequency histogram for small variants
#'
#' @description Plot allele-frequency histogram for small variants, per-sample
#' @param small_variant_df Data-frame containing small variants
#'
#' @return ggplot object
#' @export
plot_af_histogram <- function(small_variant_df){
  g <- ggplot(small_variant_df, aes(x = allele_frequency, group = sample_id,
                                    colour = sample_id, fill = sample_id)) +
    geom_density(alpha=0.3) +
    facet_wrap(~sample_id, ncol = 4) +
    theme(
      panel.grid.major = element_line(color = "grey",
                                      linetype = 3,
                                      size = 0.4))
  return(g)
}

#' Plot allele-frequency kernel density estimate (KDE) for small variants
#'
#' @description Plot allele-frequency KDE for small variants, per-sample
#' @param small_variant_df Data-frame containing small variants
#'
#' @return ggplot object
#' @export
plot_af_density <- function(small_variant_df){
  g <- ggplot(small_variant_df, aes(x = allele_frequency, group = sample_id,
                                    colour = sample_id, fill = sample_id)) +
    geom_density(alpha=0.3) +
    facet_wrap(~sample_id, ncol = 4) +
    theme(panel.grid.major = element_line(color = "grey",
                                          linetype = 3,
                                          size = 0.4))
  return(g)
}

#' Add theme elements for visual customisation
#'
#' @param ggplot_object ggplot2 plot object
#'
#' @return ggplot object
#' @export
add_common_theme_elements <- function(ggplot_object){
  common_theme_elements <- theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0, size = 8),
    axis.text = element_text(size=8),
    legend.title = element_blank(),
    legend.position = "none",
    panel.background = element_rect(fill = 'white'),
    panel.border = element_rect(fill= NA, color = "black")
  )
  g2 <- ggplot_object + common_theme_elements
  return(g2)
}
