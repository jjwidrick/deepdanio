#' Smooth body segment angular changes over time.
#'
#' Use loess to smooth the response of the specified body segment.
#'
#' @param df the results df
#' @param settings the experimental settings
#' @param angle.df raw angle data (ex. head.angle.df)
#' @return the original df with 2 new columns for each segment:
#'         raw segment angle and smoothed segment angle
#' @examples
#' @export




smooth_curvature <- function(df, settings, angle.df) {

  # Step 1: get 'ang.smo.span'
    ang.smo.span <- settings$ang.smo.span


  # Step 2: smooth data with loess
    temp1      <- angle.df %>% select(segment, orig.ms, deg)
    temp.loess <- loess(deg ~ orig.ms, temp1, span=ang.smo.span)
    temp2      <- predict(temp.loess)
    as.data.frame(temp2)

    new.df <- cbind(temp1, temp2)
    new.df <- new.df %>% dplyr::rename(deg.smo = temp2)



  # Step 3: select final variables
    new.df <- new.df %>% select(segment, deg, deg.smo)



  # Step 4. clean up
    this.segment <- new.df$segment[1]
    name1 <- paste0(this.segment, ".deg")
    name2 <- paste0(this.segment, ".smo.deg")
    new.df <- new.df %>% mutate_if(is.numeric, round, digits=3) %>%
                         dplyr::rename_with(~name1, deg) %>%
                         dplyr::rename_with(~name2, deg.smo) %>%
                         select(-segment)


  # Step 5. insert into original df
    df <- cbind(df, new.df)


  return(df)

}
