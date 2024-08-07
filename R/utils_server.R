

#' Find the taxonomic label given a Taxon ID and a Rank level
#'
#' @param ta a tidytacos object
#' @param tid Taxon ID
#' @param rank Taxonomic Rank
#'
find_taxon_rank_label <- function(ta, tid=NA, rank){
  if(is.na(tid)){
    return(NA_character_)
  }else{
    ta$taxa %>%
      dplyr::filter(taxon_id==tid) %>%
      dplyr::pull(!!rank)
  }
}

#' Find the two most abundant genus for each sample and add it to the samples table
#'
#' @param ta a tidytacos object
#' @param genus_aggregation TRUE if table needs aggregation to the genus level.
#'     FALSE if the provided tidytacos object is already aggregated.
#' @param dominance TRUE if a column should be added to specify the dominant taxon
#'     per sample.
#' @param dominance_thr a numeric value specifying the minimum relative abundance
#'     of the most abundant taxon to be considered as dominant.
my_add_top_genus <-function(ta, genus_aggregation=F, dominance=T, dominance_thr=0.3){
  if(genus_aggregation){
    my_genus_ta <- ta %>%
      tidytacos::aggregate_taxa(rank = "genus")
  }else{
    my_genus_ta <- ta
  }

  if(!("rel_abundance" %in% colnames(my_genus_ta$counts))){
    my_genus_ta <- my_genus_ta %>% tidytacos::add_rel_abundance()
  }

  my_samples <- my_genus_ta$counts %>%
    dplyr::group_by(sample_id) %>%
    dplyr::summarise(
      top_genus1_id = taxon_id[which.max(count)],
      top_genus1_rel_abundance = round(rel_abundance[which.max(count)],3),
      top_genus1 = sapply(top_genus1_id,
                          find_taxon_rank_label, ta=my_genus_ta, rank="genus"),
      top_genus2_id = taxon_id[order(count, decreasing = TRUE)[2]],
      top_genus2_rel_abundance = round(rel_abundance[order(count,
                                                           decreasing = TRUE)[2]],3),
      top_genus2 = sapply(top_genus2_id,
                          find_taxon_rank_label, ta=my_genus_ta, rank="genus"),
      dominant_taxa = if(top_genus1_rel_abundance >= dominance_thr){top_genus1} else {"No dominance"}
    )

  if(genus_aggregation){
    my_samples$top_genus1_id <- NULL
    my_samples$top_genus2_id <- NULL
  }
  if(!dominance){
    my_samples$dominant_taxa <- NULL
  }

  # We are returning the original tidytacos object, with only the samples table modified
  # The samples table contains now the 1st and 2nd most abundant taxa per sample
  ta %>% purrr::modify_at("samples", dplyr::left_join,
                          my_samples, by=dplyr::join_by(sample_id))
}

#' Upload files from a folder created by tidytacos
#'
#' @param samples the samples.csv file
#' @param taxa the taxa.csv file
#' @param counts the counts.csv file
#'
read_tidytacos_files <- function(samples, taxa, counts){
  samples <- readr::read_csv(samples, col_types = readr::cols())
  taxa <- readr::read_csv(taxa, col_types = readr::cols())
  # Tidyamplicons compatibility
  if (basename(counts) == "counts.csv") {
    counts <- readr::read_csv(counts, col_types = readr::cols())
  } else if (basename(counts) == "abundances.csv") {
    counts <- readr::read_csv(counts, col_types = readr::cols()) %>%
      rename(count=abundance)
    message("Converted tidyamplicons to tidytacos object.")
  } else {
    stop(paste("File", counts,
               ", containing count data has unexpected name for a tidytacos format. ",
               "Expected names are 'counts.csv' or 'abundances.csv'"))
  }
  expected_rank_names <- colnames(taxa)[!colnames(taxa) %in% c("taxon","taxon_id","sequence")]

  ta <- tidytacos:::make_tidytacos(
    samples, taxa, counts, sample_name = sample_id, taxon_name = taxon_id
  )

  if ( !all(ta %>% rank_names() %in% expected_rank_names)) {
    warning(paste0(
      "Not all default rank names found. Replacing them with:\n c(\"",
      paste(expected_rank_names, collapse='","'),
      "\")\n\nIf these are not the rank names of your taxon table, \nplease set ",
      "them manually using 'set_rank_names()'"))
    ta <- ta %>% tidytacos::set_rank_names(expected_rank_names)
  }
  ta
}

# For plots
#------------

#' Generate a plot for a column of a dataframe
#'
#' @param my_df a dataframe
#' @param my_col_ix a column index
#' @param plot_type the type of plot
#' @param n_bins the number of bins if a histogram is selected
#'
#' @import ggplot2
plot_num_col_from_df <- function(my_df, my_col_ix,
                                 plot_type=c("histogram","density","boxplot"),
                                 n_bins = 30){
  my_col_name <- names(my_df)[my_col_ix]
  if(plot_type == "histogram"){
    my_df %>%
      ggplot2::ggplot(aes_string(my_col_name)) +
      ggplot2::geom_histogram(bins = n_bins, fill = "#fcd0be", color = "#fcd0be") +
      ggplot2::ylab("Frecuencia") +
      ggplot2::theme_bw() + ggplot2::theme(axis.title = element_text(size = 16))
  }else if(plot_type == "density"){
    my_df %>%
      ggplot2::ggplot(aes_string(my_col_name)) +
      ggplot2::geom_density(fill = "#fcd0be", color = "#fcd0be") +
      ggplot2::stat_density(geom = "line", linewidth = 1) +
      ggplot2::ylab("Densidad") +
      ggplot2::theme_bw() + ggplot2::theme(axis.title = element_text(size = 16))
  }else{
    my_df %>%
      ggplot2::ggplot(aes_string(my_col_name)) +
      ggplot2::geom_boxplot(fill = "#fcd0be") +
      ggplot2::theme_bw() + ggplot2::theme(axis.title = element_text(size = 16))
  }
}

#' Generate a plot for a discrete column of a dataframe
#'
#' @param my_df a dataframe
#' @param my_col_ix a column index
#'
#' @import ggplot2
plot_disc_col_from_df <- function(my_df, my_col_ix){
  my_col_name <- names(my_df)[my_col_ix]
  col_values <- my_df[[my_col_ix]]
  col_levels <- names(sort(table(col_values), decreasing = TRUE))
  col_gg <- my_df %>%
    dplyr::mutate(".x" = factor(col_values, levels=col_levels)) %>%
    ggplot2::ggplot(ggplot2::aes(.x)) + ggplot2::geom_bar(fill = "#fcd0be") +
    ggplot2::geom_text(stat="count", ggplot2::aes(label=after_stat(count)), vjust=-0.5) +
    ggplot2::xlab(my_col_name) +
    ggplot2::ylab("Frecuencia") +
    ggplot2::theme_bw() +
    ggplot2::coord_flip()
  # if(max(nchar(col_levels))*length(col_levels)>40){
  #   col_gg <- col_gg + theme(axis.text.x =
  #                              element_text(size = 12, angle = 45,
  #                                           vjust = 0.5, hjust = 0.5))
  # }else{
  #   col_gg <- col_gg + theme(axis.text.x = element_text(size = 12))
  # }
  col_gg + ggplot2::theme(axis.title = ggplot2::element_text(size = 16))

}
