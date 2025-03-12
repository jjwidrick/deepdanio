#' Calculate linear kinematics.
#'
#' Using the keypoint specified in the settings file (usually S2),
#'     calculate the distance, displacement, speed, velocity, and
#'     acceleration of this keypoint across time.
#'
#' @param df the results df
#' @param settings experimental settings
#' @return the original df with new columns added
#' @examples
#' @export


calc_linear_kinematics <- function(df, settings) {

  total.move.ms <- settings$total.move.ms
  sec.per.frame <- settings$sec.per.frame

# Step 1: distance and displacement

  # A: get x and y at start of movement (for displacement calculation)
  start.df <- df %>% filter(move.ms == 0)
  S2x0.mm    <- start.df[["S2x.mm"]]
  S2y0.mm    <- start.df[["S2y.mm"]]

  # B: distance and displacement calculations
  df <- df %>% mutate(
          dist.pt.pt.mm = ifelse(move.ms > 0, sqrt(  (S2x.mm - lag(S2x.mm))^2 + (S2y.mm - lag(S2y.mm))^2), 0 ),
          cum.dist.mm   = cumsum(dist.pt.pt.mm),
          cum.dist.BL   = round(cum.dist.mm/BL.mm, 3),
  				displ.mm      = ifelse(move.ms > 0, sqrt( ((S2y.mm - S2y0.mm)^2) + ((S2x.mm - S2x0.mm)^2) ), 0),
  				displ.BL      = round(displ.mm/BL.mm, 3))

  # C: format values
  df <- df %>% mutate(dist.pt.pt.mm	= round(dist.pt.pt.mm, 3),
                      cum.dist.mm	  = round(cum.dist.mm, 3),
                      cum.dist.BL	  = round(cum.dist.BL, 3),
                      displ.mm	    = round(displ.mm, 3),
                      displ.BL      = round(displ.BL, 3))

  # D: clean up (omit kinematics in mm)
  df <- df %>% select(-dist.pt.pt.mm, -cum.dist.mm, -displ.mm)




# Step 2: speed, velocity, acceleration

  # A: calculate speed
  df <- df %>% mutate(spd.BL.s  = (lead(cum.dist.BL) - lag(cum.dist.BL))/(2*sec.per.frame))

  # B: calculate velocity
  df <- df %>% mutate(vel.BL.s  = (lead(displ.BL) - lag(displ.BL))/(2*sec.per.frame))

  # C: calculate acceleration
  df <- df %>% mutate(acc.BL.s2 = (lead(vel.BL.s) - lag(vel.BL.s))/(2*sec.per.frame))

  # D: delete values that occur before movement
  df <- df %>% mutate(spd.BL.s  = ifelse(move.ms <= 0, 0, spd.BL.s),
	                    vel.BL.s  = ifelse(move.ms <= 0, 0, vel.BL.s),
	                    acc.BL.s2 = ifelse(move.ms <= 0, 0, acc.BL.s2))

  # E: clean up
  df <- df %>% mutate(spd.BL.s  = round(spd.BL.s,  3),
                      vel.BL.s  = round(vel.BL.s,  3),
                      acc.BL.s2 = round(acc.BL.s2, 0))


  # Step 3: trim off the extra frames used by central difference calcs
  df <- df %>% filter(move.ms <= total.move.ms)

return(df)


}



