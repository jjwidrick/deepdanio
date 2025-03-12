#' curvature baseline adjustment
#'
#' Larvae often start with a baseline body curvature that is not zero,
#'     i.e. there is some positive or negative curvature offset. This
#'     offset must be removed prior to making calculations such as
#'     peak curvature, etc. This script adjusts curvature values to a zero starting baseline.
#'
#' @param df the results df (with a 'tail.curve.inv' column)
#' @return the original df with a new 'tail.curve.inv.zero' column
#' @examples
#' @export



# standardize curvature baseline to zero
# ---------------------------------------------------------------------------------------
# many fish do not start swimming at a curvature of zero
# some fish have a small body bend
# some fish, esp. mut, have a large body bend
# we are only interested in the change in curvature after fish starts swimming
# in this chunk, we remove the starting bend or curvature offset
#   by determining beginning curvature and subtracting this from all subsequent values


curvature_baseline_adj <- function(df) {

# get the tail curvature at move.ms = 0 (curve.zero)
  test.row <- df %>% filter(move.ms == 0)
  curve.zero <- test.row$tail.curve

# adjust curvature by subtracting curve value at zero (curve.zero)
  df <- df %>% mutate(tail.curve.zero = tail.curve - curve.zero, .after=tail.curve)

return(df)

}


