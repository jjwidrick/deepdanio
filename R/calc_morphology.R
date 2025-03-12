#' Calculate body length and body bend..
#'
#' Calculate the length of the larvae.
#'     Calculate body bend as the angle between
#'     the swim bladder segment and the first tail segment.
#'     Both calculations use the 10 pre-stimulus frames.
#'     Inserts these values into the original df.
#'
#' @param df the results df
#' @param settings the experimental settings
#' @return the original df with body length and body bend added
#' @examples
#' @export

calc_morphology <- function(df, settings) {

  pre.stim.ms <- settings$pre.stim.ms


# STEP 1: calc BL

  # cal length function
    calc_length <- function(Ax, Ay, Bx, By) {
      length = sqrt( ((Ax - Bx)^2) + ((Ay - By)^2) )
    }

  # extract data from the pre-stim period
	  sub.df <- df %>% filter(orig.ms < pre.stim.ms)

  # calculate length of each body segment
  	sub.df <- sub.df %>% mutate( head.mm = calc_length(TSx.mm, TSy.mm, S1x.mm, S1y.mm),
  														   body.mm = calc_length(S1x.mm, S1y.mm, S2x.mm, S2y.mm),
  														   tail1.mm = calc_length(S2x.mm, S2y.mm, T1x.mm, T1y.mm),
  	                             tail2.mm = calc_length(T1x.mm, T1y.mm, T2x.mm, T2y.mm),
  	                             tail3.mm = calc_length(T2x.mm, T2y.mm, T3x.mm, T3y.mm),
  	                             tail4.mm = calc_length(T3x.mm, T3y.mm, T4x.mm, T4y.mm),
  															 total.length.mm = head.mm +
  															 	                 body.mm +
  															 	                 tail1.mm +
  															 	                 tail2.mm +
  															 	                 tail3.mm +
  															 	                 tail4.mm)

  # now take mean across the pre-stim period
    BL.mm <- mean(sub.df$total.length.mm)
    BL.se <- (sd(sub.df$total.length.mm)) / (sqrt(length(sub.df$total.length.mm)))

    BL.mm <- round(BL.mm, 2)
    BL.se <- round(BL.se, 3)

  # add BL to df
	  df <- df %>% mutate(BL.mm = BL.mm, .after = latency.ms)




# STEP 2: calculate pre-stim body bend

	# A. create a segment coordinate df
	# body bend is defined as the absolute value of the angle between segment S1-S2 and S2-T1
    seg.df <- df %>% filter(orig.ms < pre.stim.ms) %>%
    	               select(orig.ms, S2x.mm, S2y.mm, S1x.mm, S1y.mm, T1x.mm, T1y.mm) %>%
                     dplyr::rename(Mx.mm = S1x.mm,
                         			  	 My.mm = S1y.mm,
    										        	 Px.mm = S2x.mm,
    										        	 Py.mm = S2y.mm,
    										        	 Bx.mm = T1x.mm,
    										      		 By.mm = T1y.mm) %>%
                       mutate(segment = "bend", .before = orig.ms)

  # B. calculate angle
    angle.df <- calc_angle_two_segments(seg.df)

  # take absolute value
    angle.df <- angle.df %>% mutate(abs.deg = abs(deg))

  # calc mean
    abs.deg.mean <- round( mean(angle.df$abs.deg), 2)

  # add bend angle to df
    df <- df %>% mutate(body.bend.deg = abs.deg.mean, .after = BL.mm)


# Step 3: return original df with morphological variables

	  return(df)

}



