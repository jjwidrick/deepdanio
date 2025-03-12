#' Calculate tail curvature
#'
#' Calculate tail curvature as the sum of the angles
#'     of each tail segment. ALso calculate total body vurvature
#'     (tail curvatures and head curvature). Use smoothed curvature data
#'     in calculation.
#'
#' @param df the results df
#' @return the original df with tail.curve and body.curve
#' @examples
#' @export


calc_tail_curvature <- function(df) {

    df <- df %>% mutate(tail.curve  = tail1.smo.deg + tail2.smo.deg + tail3.smo.deg + tail4.smo.deg)
    # note that for body curve, add negative of head.smo.deg
    df <- df %>% mutate(body.curve = tail.curve + -(head.smo.deg))
    df <- df %>% mutate(tail.curve = round(tail.curve, 3),
                        body.curve = round(body.curve, 3)) %>%
                 relocate(body.curve, .before = tail.curve)

return(df)
}
