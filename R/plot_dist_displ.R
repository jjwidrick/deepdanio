#' Plot distance and displacement vs movement time.
#'
#' Create a distance and displacement plot using
#'     the results df.
#'
#' @param df the results df
#' @param settings the experimental settings
#' @return a plot
#' @examples
#' @export


plot_dist_displ <- function(df, settings, ER_plot_theme) {

  total.move.ms <- settings$total.move.ms
  latency.ms    <- df$latency.ms[1]

  # cut off at total.move.ms
  plot.df <- df %>% filter(move.ms <= total.move.ms)

  # make plot
  plot <- ggplot(plot.df, aes(x=move.ms, y=cum.dist.BL)) +
  					   geom_path(size=0.75, color="black") +
  	           ylab("body length") +
  	           geom_hline(yintercept=0, linetype="solid", color = "grey70", size=0.5) +
  		         ER_plot_theme +
  	           scale_x_continuous(breaks = seq(-20, total.move.ms, 10),
  	           									  limits = c((-10 - latency.ms), total.move.ms))

  plot <- plot + geom_path(aes(x=move.ms, y=displ.BL), size=0.5, color="#92c5de") +
          ggtitle("distance (black) and displacement (blue)")


  return(plot)
}


