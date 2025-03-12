#' Plot acceleration vs movement time.
#'
#' Create an acceleration plot using
#'     the results df.
#'
#' @param df the results df
#' @param settings the experimental settings
#' @return a plot
#' @examples
#' @export


plot_acc <- function(df, settings, ER_plot_theme) {

  total.move.ms <- settings$total.move.ms
  latency.ms    <- df$latency.ms[1]

  # cut off at total.move.ms
  plot.df <- df %>% filter(move.ms <= total.move.ms)

  # make plot
  plot <- ggplot(plot.df, aes(x=move.ms, y=acc.BL.s2)) +
  					   geom_path(size=0.5, color="#92c5de") +
  	           ylab("body length/s2") +
  	           geom_hline(yintercept=0,           linetype="solid",  color = "grey70", size=0.5) +
  		        # geom_vline(xintercept=stim,          linetype="dashed",  color = "orange", size=0.5) +
  	          # geom_vline(xintercept=stg1.start,    linetype="solid",  color = "grey75",  size=0.25) +
  		        # geom_vline(xintercept=stg1to2,       linetype="solid",  color = "grey75",  size=0.25) +
  	          # geom_vline(xintercept=stg2to3,       linetype="solid",  color = "grey75",  size=0.25) +
  	          # geom_vline(xintercept=total.move.ms, linetype="solid",  color = "grey75",  size=0.25) +
  	           ER_plot_theme +
  	           scale_x_continuous(breaks = seq(-20, total.move.ms, 10),
  	           									  limits = c((-10 - latency.ms), total.move.ms)) +
               ggtitle("acceleration")

  return(plot)
}
