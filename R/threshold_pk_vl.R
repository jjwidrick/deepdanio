#' Threshold small peaks and valleys
#'
#' Eliminate small peaks and valleys that may occur before the C-start based on threshold
#'
#' @param df the df
#' @param settings the trial settings (needs threshold value)
#' @return peak and valley df with small peaks and valleys ignored
#' @examples
#' @export




threshold_pk_vl <- function(df, settings) {

# eliminate small peaks and valleys that may occur (and are detected) before the first major peak
# the pk.vl.threshold is set by investigator in settings
# here, any tail.curve.zero values less than the pk.vl.threshold are eliminated from the pk.vl.df

# 1. get threshold value
  pk.vl.threshold <- settings$pk.vl.threshold


# 2. calc tail.curve.zero abs values
# (because "peak" value can be positive or negative in sign)
  pk.vl.df.thres <- df %>% mutate(abs.tail.curve.zero = abs(tail.curve.zero)) %>% arrange(move.ms)


# 3. eliminate any pks and vls preceding stage 1 final curvature
#    The following code truncates 'pk.vl.df.thres' so that it starts at the stage 1 ending row
#    (eliminating any rows that fail to meet threshold).

    # a. get no. of rows
      no.rows <- nrow(pk.vl.df.thres)

    # b. loop through rows, testing whether 'abs.tail.curve.zero' is greater than threshold
    #    if 'abs.tail.curve.zero' fails to reach threshold, delete that row from df
    #    when you encounter the row that exceeds threshold, stop (break out of loop)

      for(i in 1:no.rows) {
        test.row <- 1
        test.value <- pk.vl.df.thres[[test.row,"abs.tail.curve.zero"]]
              if(test.value < pk.vl.threshold) {
                pk.vl.df.thres <- pk.vl.df.thres %>% slice(-1)
            } else {
              break
            }
      }


# 4. clean up (delete abs.tail.curve.zero column)
  pk.vl.df.thres <- pk.vl.df.thres %>% select(-abs.tail.curve.zero)


# 5. sort on time
  pk.vl.df.thres <- pk.vl.df.thres %>% arrange(move.ms)


return(pk.vl.df.thres)

}


