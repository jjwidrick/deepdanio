#' Rotational adjustment test
#'
#' Test whether a rotational adjustment in tail curvature is needed
#'
#' @param df the df
#' @param rot.adj logical variable for whether a rotational adjustment is needed
#' @return true (adjustment necessary) or false (no adjustment necessary)
#' @examples
#' @export





# test pk.vl.df to see if rotational adjustment is needed
# a return value of TRUE indicates adjustment required

rotation_adj_test <- function(df) {

    row.one <- df %>% slice(1)

    if(row.one$tail.curve.zero < 0) {
      rot.adj <- TRUE
    } else {
      rot.adj <- FALSE
    }

  return(rot.adj)

}
