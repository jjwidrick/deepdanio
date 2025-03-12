#' Plot tail segment angles vs movement time.
#'
#' Create a plot of tail angle for each tail segment using
#'     the results df.
#'
#' @param df the results df
#' @param settings the experimental settings
#' @return a plot
#' @examples
#' @export



plot_tail_segments <- function(df, settings, ER_plot_theme) {

  total.move.ms <- settings$total.move.ms
  latency.ms    <- df$latency[1]

  # color scheme for tail segments
  segment.colors <- c('#4771b2', '#93003a', '#cf3759', '#f4777f', '#ffbcaf')    # reds

  # convert from wide to tall format
  tall.df <- df %>% gather("head.smo.deg", "tail1.smo.deg", "tail2.smo.deg", "tail3.smo.deg", "tail4.smo.deg",
  														 key = "segment",
  														 value = "degrees")

  # re-code factor
  tall.df$segment[tall.df$segment == "head.smo.deg"]  <- "head"
  tall.df$segment[tall.df$segment == "tail1.smo.deg"] <- "tail1"
  tall.df$segment[tall.df$segment == "tail2.smo.deg"] <- "tail2"
  tall.df$segment[tall.df$segment == "tail3.smo.deg"] <- "tail3"
  tall.df$segment[tall.df$segment == "tail4.smo.deg"] <- "tail4"

  # order factor
  tall.df$segment <- factor(tall.df$segment, levels=c("head", "tail1", "tail2", "tail3", "tail4"))

  # cut off at total.move.ms
  tall.df <- tall.df %>% filter(move.ms <= total.move.ms)

  # make plot
  plot <- ggplot(tall.df, aes(x=move.ms, y=degrees, color=segment, group=segment)) +
  						 geom_path(size=0.5) +
  	           scale_colour_manual(values = segment.colors) +
  	           ylab("degrees") +
               ER_plot_theme +
  	           geom_hline(yintercept=0, linetype="solid", color = "grey70", size=0.5) +
  	           scale_x_continuous(breaks = seq(-20, total.move.ms, 10),
  	           									  limits = c((-10 - latency.ms), total.move.ms)) +
               theme(legend.title=element_blank(),
                     legend.direction='horizontal',
                     legend.position = c(0.75, 0.9),
                     legend.text = element_text(margin = margin(t = -5))) +
               guides(color = guide_legend(label.position = "bottom")) +
              # guides(color="none") +
               ggtitle("segment angles")

  return(plot)

}


