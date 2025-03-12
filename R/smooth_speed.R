#' Smooth speed over time.
#'
#' Use loess to smooth the speed response.
#'
#' @param df the results df
#' @param settings the experimental settings
#' @return the original df with a new column of smoothed angle data
#' @examples
#' @export



smooth_speed <- function(df, settings) {


  spd.smo.span <- settings$spd.smo.span


# Step 1: smooth with loess

  spd.model   <- loess(spd.BL.s ~ move.ms, data = df, span = spd.smo.span)
  smo.spd     <- predict(spd.model, se=T)

  smo.spd.df <- data.frame(move.ms = df$move.ms,
  											    smooth = smo.spd$fit,
  											    SE     = smo.spd$se)

  smo.spd.df <- smo.spd.df %>% mutate(smo.spd.BL.s = round(smooth, 3),
  	              			              smo.spd.se   = round(SE, 3)) %>%
  	                           select(-smooth, -SE, -smo.spd.se)


# Step 2: replace smooth speed values < 0 move.ms with zero's
# use move.ms < 0 instead of <= 0 because we want smoothed point at 0 move.ms
  smo.spd.df <- smo.spd.df %>% mutate(smo.spd.BL.s = ifelse(move.ms < 0, 0, smo.spd.BL.s))


# Step 3: add smooth speed to df
  df <- left_join(df, smo.spd.df, by = 'move.ms')

  return(df)


}
