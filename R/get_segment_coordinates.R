#' Calculate the angle between two body segments.
#'
#' Specify a segment, and this function will get the obtain
#'    the mm values from the results df and rename than for input
#'    into the 'calc_angle_two_segments' function.
#'
#' @param df the results df
#' @param settings the experimental settings
#' @param segment a segment (ex. "head" or "tail1")
#' @return the seg.df used for input into 'calc_angle_two_segments'
#' @examples
#' @export



get_segment_coordinates <- function(df, settings, segment) {

    if (segment == "head") {
      seg.df <- df %>% select(orig.ms, S2x.mm, S2y.mm, S1x.mm, S1y.mm, TSx.mm, TSy.mm) %>%
                       dplyr::rename(Mx.mm = TSx.mm,
                           			  	 My.mm = TSy.mm,
      										        	 Px.mm = S1x.mm,
      										           Py.mm = S1y.mm,
      										        	 Bx.mm = S2x.mm,
      										      		 By.mm = S2y.mm) %>%
                       mutate(segment = segment, .before = orig.ms)
    } else if (segment == "tail1") {
      seg.df <- df %>% select(orig.ms, T1x.mm, T1y.mm, S2x.mm, S2y.mm, S1x.mm, S1y.mm) %>%
                       dplyr::rename(Mx.mm = T1x.mm,
                       			  			 My.mm = T1y.mm,
  										        			 Px.mm = S2x.mm,
  										        			 Py.mm = S2y.mm,
  										        			 Bx.mm = S1x.mm,
  										      				 By.mm = S1y.mm) %>%
                       mutate(segment = segment, .before = orig.ms)
    } else if (segment == "tail2") {
      seg.df <- df %>% select(orig.ms, T2x.mm, T2y.mm, T1x.mm, T1y.mm, S2x.mm, S2y.mm) %>%
                       dplyr::rename(Mx.mm = T2x.mm,
                     			  			   My.mm = T2y.mm,
										        			   Px.mm = T1x.mm,
										        			   Py.mm = T1y.mm,
										        			   Bx.mm = S2x.mm,
										      				   By.mm = S2y.mm) %>%
                       mutate(segment = segment, .before = orig.ms)
    } else if (segment == "tail3") {
      seg.df <- df %>% select(orig.ms, T3x.mm, T3y.mm, T2x.mm, T2y.mm, T1x.mm, T1y.mm) %>%
                       dplyr::rename(Mx.mm = T3x.mm,
                       			  			 My.mm = T3y.mm,
  										        			 Px.mm = T2x.mm,
  										        			 Py.mm = T2y.mm,
  										        			 Bx.mm = T1x.mm,
  										      				 By.mm = T1y.mm) %>%
                       mutate(segment = segment, .before = orig.ms)
    } else if (segment == "tail4") {
      seg.df <- df %>% select(orig.ms, T4x.mm, T4y.mm, T3x.mm, T3y.mm, T2x.mm, T2y.mm) %>%
                       dplyr::rename(Mx.mm = T4x.mm,
                       			  			 My.mm = T4y.mm,
  										        			 Px.mm = T3x.mm,
  										        			 Py.mm = T3y.mm,
  										        			 Bx.mm = T2x.mm,
  										      				 By.mm = T2y.mm) %>%
                       mutate(segment = segment, .before = orig.ms)
    }

return(seg.df)

}

