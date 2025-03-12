#' Process current .csv file.
#'
#' Process the DeepLabCut output .csv file that is in the current
#'     working directory.
#'       1. convert x, y pixel coordinates into mm coordinates
#'       2. calculate angular and linear kinematics
#'       3. identify the three stages of the escape response
#'       4. collapse multiple trials per fish into a mean value per fish
#'       5. make plots of selected responses
#'       6. make a summary table of selected variables
#'       7. save three df's: 'by_ms', 'one_row', and 'pk_vl'
#'     Your working directory should be setup as follows:
#'       1. A sub-directory named 'info'. Within this sub-directory a
#'          file labeled 'settings.txt' that contains information about
#'          the experiment.
#'       2. A data directory which contains a SINGLE .csv file,
#'          the file you want to analyze. During routine data collection
#'          this directory is your working directory. This parameter can
#'          be modified for cases where the experiment for analysis may
#'          be in a different directory.
#'       3. A directory where the df's created will be stored.
#'          This is usually a sub-directory named "results'.
#'
#' @param settings_path The path to your 'settings.txt' file.
#'     This file contains important information about your experiment.
#'     It is normally in a sub-directory labelled 'info'. Therefore, this parameter
#'     is usually "./info/settings.txt".
#' @param data_dir The path to the current .csv file you want to analyze.
#'     Normally, this is the .csv file that was just analyzed by DeepLabCut,
#'     i.e. your current video. Therefore, this path is usually the current
#'     working directory, or "./". Note that only ONE .csv file can be in this
#'     directory.
#' @param results_dir The sub-directory where you want the df's created to be store.
#'     An example would be "./results", a sub-directory of your working directory.
#' @return Three df's, a panel of plots, and a summary table.
#' @examples
#' @export




#+ echo=FALSE, warning=FALSE, message=FALSE



kin_process <- function(video.filename, video.ID, settings_path, data_dir, results_dir, plots, pk_vl_save) {

# EXTRACT KEYPOINT COORDINATES --------------------------

# get settings for this experiment
  settings <- get_settings(settings_path)

# open the .csv output file from DeepLabCut
  wide.df  <- get_csv_data(video.filename, video.ID, data_dir, settings)

# get coordinates
  head.df  <- get_segment_coordinates(wide.df, settings, "head")
  tail1.df <- get_segment_coordinates(wide.df, settings, "tail1")
  tail2.df <- get_segment_coordinates(wide.df, settings, "tail2")
  tail3.df <- get_segment_coordinates(wide.df, settings, "tail3")
  tail4.df <- get_segment_coordinates(wide.df, settings, "tail4")



# CALCULATE ANGULAR KINEMATICS -----------------------------

# calculate angle between segments of each coordinate df
  head.angle.df  <- calc_angle_two_segments(head.df)
  tail1.angle.df <- calc_angle_two_segments(tail1.df)
  tail2.angle.df <- calc_angle_two_segments(tail2.df)
  tail3.angle.df <- calc_angle_two_segments(tail3.df)
  tail4.angle.df <- calc_angle_two_segments(tail4.df)

# smooth response
  wide.df <- smooth_curvature(wide.df, settings, head.angle.df)
  wide.df <- smooth_curvature(wide.df, settings, tail1.angle.df)
  wide.df <- smooth_curvature(wide.df, settings, tail2.angle.df)
  wide.df <- smooth_curvature(wide.df, settings, tail3.angle.df)
  wide.df <- smooth_curvature(wide.df, settings, tail4.angle.df)

# calculate tail curvature
  wide.df <- calc_tail_curvature(wide.df)

# detect movement, calc latency, add move.ms column
  wide.df <- detect_movement(settings, wide.df)




# STANDARDIZE TAIL CURVATURE RESPONSE ----------------------------------

# step 1: remove any curvature offset at start of movement (move.ms = 0)
    wide.df <- curvature_baseline_adj(wide.df)

# step 2: detect peaks and valleys
    pk.vl.df <- detect_pks_vls(wide.df, settings)

# step 3: apply threshold (threshold not working)
    pk.vl.df.thres <- threshold_pk_vl(pk.vl.df, settings)

# step 4: adjust rotation
  # a) test if a rotation adjustment is needed
     rot.adj <- rotation_adj_test(pk.vl.df.thres)

  # b) adjust rotation direction in pk.vl.df.thres (based on 'rot.adj' value)
     pk.vl.df.adj <- rotation_adj_pk_vl(pk.vl.df.thres, rot.adj)

  # c) adjust rotation direction in main.df (based on 'rot.adj' value)
     wide.df <- rotation_adj_response(wide.df, rot.adj)




# CURVATURE CALCULATIONS --------------------------------------------

# calculate tail curvature, detect movement, calc latency, and calc angular kinematics
# because of last section, wide.df now has 'tail.curve.zero.rotate' column, i.e. tail
# curvature that has been adjusted for any initial baseline offset and standardized
#     for rotation
# all angular kinematic calculations based on 'tail.curve.zero.rotate'
  wide.df <- calc_angular_kinematics(settings, wide.df)

# clean up df's that are no longer needed
  rm(head.df, tail1.df, tail2.df, tail3.df, tail4.df)
  rm(head.angle.df,
     tail1.angle.df, tail2.angle.df, tail3.angle.df, tail4.angle.df)





# CALCULATE LINEAR KINEMATICS ---------------------------------------

# calculate morphology (BL, bend angle)
  wide.df <- calc_morphology(wide.df, settings)

# calculate kinematic variables (dist, displ, spd, vel, acc)
  wide.df <- calc_linear_kinematics(wide.df, settings)





# FINALIZE MAIN df --------------------------------------------------

# smooth speed w/loess
  wide.df <- smooth_speed(wide.df, settings)

# save wide df (it is now complete)
  save_df(wide.df, "all_ms", settings, results_dir)





# FIND PEAKS AND VALLEYS OF TAIL CURVATURE --------------------------

# calc stage beginnings and endings
  pk.vl.final.df <- pk_vl_calcs(wide.df, pk.vl.df.adj, settings)

# save 'pk.vl.final.df' only if oprion is set to TRUE
  if(pk_vl_save == TRUE){
  save_df(pk.vl.final.df, "pk_vl", settings, results_dir)
  }

# remove df's no longer needed ('pk.vl.final.df' used later)
  rm(pk.vl.df)





# COLLAPSE TRIALS INTO MEAN VALUE ----------------------------------

# conduct stage-by-stage calcs
# collapse data into a single row
  row.df <- calcs_by_stage(wide.df, settings, pk.vl.final.df)

# save collapsed data as 'one_row'
  save_df(row.df, "one_row", settings, results_dir)

# remove df's no longer needed
  rm(pk.vl.final.df)




# PRESENT RESULTS (for single video analysis)  -----------------

if (plots == TRUE) {
    # make final ER plot and print
      plots <- make_plots(wide.df, settings, row.df, video.ID)
      print(plots)
    # generate summary table and print
      summary <- make_summary(wide.df, settings, row.df)
      print(summary)
}





# CLEAR MEMORY ----------------------------------------------------
  invisible(gc())



}

