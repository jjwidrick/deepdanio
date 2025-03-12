#' Calculate angular kinematics.
#'
#' Calculate angular kinematics using 'tail.curve.zero.rotate' in which
#'     tail curvature has been standardized for rotation (tail curve
#'     always moves in positive direction during stage 1) and adjusted
#'     for any baseline curvature offset.
#'
#' @param settings experimental settings
#' @param df the results df
#' @return the original df with new columns added
#' @examples
#' @export


calc_angular_kinematics <- function(settings, df) {

    ms.per.frame  <- settings$ms.per.frame
    pre.stim.ms   <- settings$pre.stim.ms
    total.move.ms <- settings$total.move.ms
    latency.ms    <- df$latency.ms

  # calculate angular velocity in degrees/ms (values appropriate for plotting, etc.)
  # note that it is critical to use 'tail.curve.zero.rotate' instead of just 'tail.curve'
    df <- df %>% mutate(tail.vel.deg.ms  = (lead(tail.curve.zero.rotate) - lag(tail.curve.zero.rotate))/(2*ms.per.frame))
    df <- df %>% mutate(tail.vel.deg.ms  = ifelse(move.ms <= 0, 0, tail.vel.deg.ms))


  # calculate angular acceleration in degrees/ms2
    df <- df %>% mutate(tail.acc.deg.ms2 = (lead(tail.vel.deg.ms) - lag(tail.vel.deg.ms))/(2*ms.per.frame))
    df <- df %>% mutate(tail.acc.deg.ms2 = ifelse(move.ms <= 0, 0, tail.acc.deg.ms2))

  # format values
    df <- df %>% mutate(tail.vel.deg.ms   = round(tail.vel.deg.ms,  3),
                        tail.acc.deg.ms2  = round(tail.acc.deg.ms2, 3))



  # add frames for central difference method (in upcoming script)
  # central difference calculation requires extra frames:
  #   one extra frame for velocity calculation and another extra frame
  #   for acceleration calculation

    total.df.ms <- pre.stim.ms + latency.ms + total.move.ms + 2

  # now, discard frames less than 'total.df.ms'
    df <- df %>% filter(orig.ms <= total.df.ms)



    return(df)

}
