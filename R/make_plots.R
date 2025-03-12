#' Make the final plot
#'
#' Assemble individual plots into a final plot.
#'
#' @param df the results df
#' @param settings the exp settings
#' @param row.df the row.df of results for the plot
#' @param video.ID the video ID for the plot
#' @return a composite plot of kinematic results
#' @examples
#' @export


make_plots <- function(df, settings, row.df, video.ID) {

# Step 1: Make individual plots
  ER_plot_theme     <- theme_minimal() +
                       theme(panel.grid.minor.x=element_blank())
  like.plot         <- plot_likelihood(df, settings, video.ID)
  tail.seg.plot     <- plot_tail_segments(df, settings, ER_plot_theme)
  tail.curve.plot   <- plot_tail_curve(df, settings, ER_plot_theme, row.df)
  tail.ang.vel.plot <- plot_tail_ang_vel(df, settings, ER_plot_theme)
  dist.plot         <- plot_dist_displ(df, settings, ER_plot_theme)
  spd.plot          <- plot_spd_vel(df, settings, ER_plot_theme)
  acc.plot          <- plot_acc(df, settings, ER_plot_theme)

# Step 2. assemble individual plots into a final plot
  plot <- like.plot / (tail.seg.plot | dist.plot) / (tail.curve.plot | spd.plot) / (tail.ang.vel.plot | acc.plot)

  return(plot)
}
