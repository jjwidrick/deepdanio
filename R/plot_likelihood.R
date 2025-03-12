#' Plot likelihood vs movement time.
#'
#' Create a multi-panel likelihood plot using
#'     the results df. Each panel
#'     displays a different segment.
#'
#' @param df the results df
#' @param settings the experimental settings
#' @return a plot
#' @examples
#' @export



plot_likelihood <- function(df, settings, video.ID) {

  like.threshold <- settings$like.threshold

  # convert from wide to tall format
  tall.df <- df %>% gather("TS.like", "S1.like", "S2.like", "T1.like", "T2.like", "T3.like", "T4.like",
  														 key = "point.name",
  														 value = "likelihood")

  # re-code factor
  tall.df$point.name[tall.df$point.name == "TS.like"] <- "TS"
  tall.df$point.name[tall.df$point.name == "S1.like"] <- "S1"
  tall.df$point.name[tall.df$point.name == "S2.like"] <- "S2"
  tall.df$point.name[tall.df$point.name == "T1.like"] <- "T1"
  tall.df$point.name[tall.df$point.name == "T2.like"] <- "T2"
  tall.df$point.name[tall.df$point.name == "T3.like"] <- "T3"
  tall.df$point.name[tall.df$point.name == "T4.like"] <- "T4"

  # order factor
  tall.df$point.name <- factor(tall.df$point.name, levels=c("TS", "S1", "S2", "T1", "T2", "T3", "T4"))

  # make plot
  plot <- ggplot(tall.df, aes(x=move.ms, y=likelihood)) +
  						 geom_point(size=1) +
  	           scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1.05), expand = c(0,0)) +
  	           facet_grid(.~point.name, switch = "y", scales = "free") +
               theme(panel.spacing = unit(0.75, "lines"),
                     strip.placement = "outside",
                     strip.text.y = element_text(size=12, colour="black"),
                     strip.text.x = element_text(size=12, colour="black"),
                     strip.background = element_blank()) +
  	           geom_hline(yintercept=like.threshold, linetype="solid", color = "red", size=0.25) +
               theme(plot.title = element_text(hjust = -0.10, size=16)) +
               ggtitle(paste0("exp ID:  ",video.ID))


  return(plot)

}

