#' Adjust rotation direction in pk.vl.df.thres
#'
#' Adjust pk.vl.df.thres
#'
#' @param df the df
#' @param rot.adj logical variable for whether a rotational adjustment is needed
#' @return output description
#' @examples
#' @export


# standardize rotation so that first peak is always in positive direction

rotation_adj_pk_vl <- function(df, rot.adj) {

    if(rot.adj == TRUE) {
      df <- df %>% mutate(tail.curve.zero.rotate = tail.curve.zero * -1)
    } else {
      df <- df %>% mutate(tail.curve.zero.rotate = tail.curve.zero * 1)
    }

  return(df)

}

