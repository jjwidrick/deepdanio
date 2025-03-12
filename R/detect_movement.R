#' Detect movement and latency
#'
#' Detect when the larvae starts to move. Calculate latency.
#'     Add a new column based on movement time (move.ms).
#'
#' @param settings experimental settings
#' @param df the results df
#' @return the original df with latency and move.ms columns
#' @examples
#' @export



detect_movement <- function(settings, df) {

  move.detect    <- settings$move.detect
  move.threshold <- settings$move.threshold
  ms.per.frame   <- settings$ms.per.frame
  pre.stim.ms    <- settings$pre.stim.ms
  sec.per.frame  <- settings$sec.per.frame
  total.move.ms  <- settings$total.move.ms


  # get running difference in tail and body curvature
  temp.df <- df %>% mutate(tail.curve.diff  = lead(tail.curve) - tail.curve,
  												 body.curve.diff  = lead(body.curve) - body.curve) %>%
  	               # filter(orig.ms >= 10)
                    filter(orig.ms >= pre.stim.ms)


  # function for detecting movement
  # movement is detected as a change (in degrees) of more than the user
  #     defined move.threshold (in CW or CCW direction)
  detect_movement_fun <- function(x){
  	x > (move.threshold) | x < -(move.threshold)
  }


  # choose whether to use tail or body curvature for movement detection
  if (move.detect == "body.curve") {
  detect.var <- "body.curve.diff"
  }
  if (move.detect == "tail.curve") {
  detect.var <- "tail.curve.diff"
  }


  # calculations
  total.vec           <- pull(temp.df, get(detect.var))
  total.latency.frame <- total.vec %>% detect_index(detect_movement_fun)
  total.latency.ms    <- (total.latency.frame * ms.per.frame)
  total.latency.ms    <- as.double(as.character(total.latency.ms))
  latency.ms          <- round( total.latency.ms, 2)



  # use latency to make move.ms column
    df <- df %>% mutate(stim.ms    = orig.ms - pre.stim.ms,
												latency.ms = latency.ms,
									      move.ms    = stim.ms - latency.ms)
    df <- df %>% relocate(stim.ms, move.ms, latency.ms, .after = orig.ms)


return(df)
}





