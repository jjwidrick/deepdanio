#' Save df
#'
#' Save a dataframe as a .csv file to the results_dir.
#'
#' @param df the df to save
#' @param save.label what to name the saved df (within quotation marks)
#' @param settings the experimental settings
#' @param results_dir the directory where the df is saved
#' @return small.df, a df with selected kinematic variables for use in plots
#' @examples
#' @export



save_df <- function(df, save.label, settings, results_dir) {

  video.ID <- df$video.ID[1]

  write_csv(df, paste0(results_dir, "/", video.ID, "_", save.label, ".csv"))

}
