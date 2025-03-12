#' Calculate the angle between two body segments.
#'
#' Calculate degrees between segments across frames.
#'    two segments are described by three keypoints
#'    "M" defines the end point of the segment to measure
#'    "P" defines the point to pivot about
#'        (it also defines one end of the measured segment
#'        and one end of the baseline segment)
#'    "B" defines the end point of the reference baseline
#'    seg.df contains the coordinates for these three keypoints
#'
#' @param seg.df a df of the coordinates of the two segments
#' @return a new df with the angle between segments vs time
#' @examples
#' @export





# calculate the angle between two segments
# two segments requires three keypoints
#
# note:
# "M" defines the end point of the segment to measure
# "P" defines the point to pivot about
#    (it also defines one end of the measured segment and one end of the baseline segment)
# "B" defines the end point of the reference baseline



calc_angle_two_segments <- function(seg.df) {

  # Step 1: translate points to new origin
  # "P", the pivot point, is the new origin
    seg.df <- seg.df %>% mutate(x.factor = (0 - Px.mm),
    										        y.factor = (0 - Py.mm))

    seg.df <- seg.df %>% mutate(tMx = Mx.mm + x.factor,
            										tMy = My.mm + y.factor,
            										tPx = Px.mm + x.factor,
            										tPy = Py.mm + y.factor,
            										tBx = Bx.mm + x.factor,
            										tBy = By.mm + y.factor)



  # Step 2: rotate reference baseline segment, P-B

    # A. calculate angle between body segment and horizontal
    seg.df <- seg.df %>% mutate(B.rad = atan2(tBy, tBx),
    										        B.deg = B.rad * (180/pi))

    # B. calculate angle for rotation
    #    need to take into account if B--P segment extends into quad 1,2 or 3,4
    #    i.e. if By < or > zero
    seg.df <- seg.df %>%  mutate(B.rot.rad = case_when(tBy > 0 ~  pi - B.rad,  # needs to be pi (not 180) because we are in radians
    																			             tBy < 0 ~ -pi - B.rad,
    																			             tBy == 0 ~ 0),   # in this case, B--P is already horizontal on y-axis
    								             B.rot.deg = B.rot.rad * (180/pi))

    # C. rotate the point B about P ((Px, Py) is the origin)
    seg.df <- seg.df %>% mutate(new.Bx = (tBx * cos(B.rot.rad)) - (tBy * sin(B.rot.rad)),
    										        new.By = (tBx * sin(B.rot.rad)) + (tBy * cos(B.rot.rad)),
    										        new.Px = (tPx * cos(B.rot.rad)) - (tPy * sin(B.rot.rad)),
    										        new.Py = (tPx * sin(B.rot.rad)) + (tPy * cos(B.rot.rad)))



  # STEP 3: rotate the segment to be measured, the P-M segment
    # rotate it the same amount as the reference P-B segment was rotated above
    # after this step, the two segments are in their original alignment
    #   only the baseline segment is now horizontal and the point P is at the origin

    # A. find the angle of the segment to measure (segment is P to M)
    #    not necessary, but good value to have for diagnostics
    seg.df <- seg.df %>% mutate(M.rad = atan2(tMy, tMx),
    										        M.deg = M.rad * (180/pi))

    # B. calculate the angle to rotate the measured segment
    seg.df <- seg.df %>% mutate(M.rot.rad = B.rot.rad,
    										        M.rot.deg = M.rot.rad * (180/pi))

    # C. rotate the measured segment
    seg.df <- seg.df %>% mutate(new.Mx = (tMx * cos(M.rot.rad)) - (tMy * sin(M.rot.rad)),
    										        new.My = (tMx * sin(M.rot.rad)) + (tMy * cos(M.rot.rad)))





  # Step 4: measure angle between P-M segment and (horizontal) body segment
    angle.df <- seg.df %>% mutate(final.rad = atan2(new.My, new.Mx),
    										          deg       = final.rad * 180/pi)            # "deg" is the final deg value


    return(angle.df)

}






