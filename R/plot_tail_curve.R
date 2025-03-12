#' Plot tail curvature vs movement time.
#'
#' Create a tail curvature plot using
#'     the results df. Plot the 'tail.curve.inv.zero' data,
#'     not the raw "tail.curve' data.
#'
#' @param df the results df
#' @param settings the experimental settings
#' @return a plot
#' @examples
#' @export


plot_tail_curve <- function(df, settings, ER_plot_theme, row.df) {

  total.move.ms <- settings$total.move.ms
  threshold     <- settings$pk.vl.threshold
  latency.ms    <- df$latency.ms[1]
  end.ms        <- row.df$stg1.dur.ms

  stg1.pk.deg <- row.df$stg1.tail.final.curve.deg
  stg1.pk.ms  <- row.df$stg1.dur.ms
  point.df1   <- tibble(move.ms = stg1.pk.ms, tail.curve.zero.rotate = stg1.pk.deg)

  stg2.end.deg <- row.df$stg1.tail.final.curve.deg - row.df$stg2.tail.stroke.pk.pk.deg
  stg2.end.ms  <- row.df$stg1.dur.ms + row.df$stg2.dur.ms
  point.df2    <- tibble(move.ms = stg2.end.ms, tail.curve.zero.rotate = stg2.end.deg)

  # cut off at total.move.ms
  plot.df <- df %>% filter(move.ms <= total.move.ms)

  # make plot
  plot <- ggplot(plot.df, aes(x=move.ms, y=tail.curve.zero.rotate)) +
  					   geom_path(size=0.75, color="black") +

               geom_point(data=point.df1, colour="red") +
               geom_point(data=point.df2, colour="red") +

               geom_segment(x=stg1.pk.ms, y=0,
                            xend=stg1.pk.ms, yend=stg1.pk.deg,
                            color="red", linetype="dashed", size=0.35) +

               geom_segment(x   =stg2.end.ms, y   =0,
                            xend=stg2.end.ms, yend=stg2.end.deg,
                            color="red", linetype="dashed", size=0.35) +

  	           ylab("degrees") +
  	           geom_hline(yintercept=0,           linetype="solid",  color = "grey70", size=0.5) +
  		        # geom_vline(xintercept=stim,          linetype="dashed",  color = "orange", size=0.5) +
  	          # geom_vline(xintercept=stg1.start,    linetype="solid",  color = "grey75",  size=0.25) +
  		        # geom_vline(xintercept=stg1to2,       linetype="solid",  color = "grey75",  size=0.25) +
  	          # geom_vline(xintercept=stg2to3,       linetype="solid",  color = "grey75",  size=0.25) +
  	          # geom_vline(xintercept=total.move.ms, linetype="solid",  color = "grey75",  size=0.25) +
  	           ER_plot_theme +
  	           scale_x_continuous(breaks = seq(-20, total.move.ms, 10),
  	           									  limits = c((-10 - latency.ms), total.move.ms)) +

               geom_segment(x=-latency.ms - 10, y=threshold,
                            xend=end.ms, yend=threshold, color="dodgerblue3", linetype="dashed", size=0.35) +
               geom_segment(x=-latency.ms - 10, y=-threshold,
                            xend=end.ms, yend=-threshold, color="dodgerblue3", linetype="dashed", size=0.35) +

               annotate("text", x=-latency.ms - 10, y=threshold,
                        label="first peak\nthreshold",
                        size=3, fontface="italic", hjust = 0, linespace=0.7) +

               ggtitle("tail curvature (baseline and rotation standardized)")

  return(plot)


}
