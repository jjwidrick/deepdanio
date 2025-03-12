#' Adjust rotation direction in main.df
#'
#' Adjust rotation direction in main.df
#'
#' @param df the df
#' @param rot.adj logical variable for whether a rotational adjustment is needed
#' @return output description
#' @examples
#' @export






rotation_adj_response <- function(df, rot.adj) {

    if(rot.adj == TRUE) {
      df <- df %>% mutate(tail.curve.zero.rotate = tail.curve.zero * -1)
    } else {
      df <- df %>% mutate(tail.curve.zero.rotate = tail.curve.zero * 1)

  return(df)

}



}
