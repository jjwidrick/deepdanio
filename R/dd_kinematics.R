#' dd_kinematics
#'
#' This is the starting script that assembles information
#' for the type of kinematic analysis, then calls kin_analysis 
#'
#' @param analysis_mode either "single", "batch" or "DAQ"
#' @param single_video_ID for single analysis mode, the ID of the trial
#' @param DLC_data_dir path to the DLC .csv pose estimate directory
#' @param settings_file path to the settings.txt file
#' @param kin_results_dir path to a directory for kinematic results
#' @param plots TRUE to make plots, FALSE to suppress plots
#' @param pk_vl_save TRUE to create pk_vl_df.csv, FALSE to suppress
#' @return parameters to start kin_analysis
#' @examples
#' @export
#' 
#' 

dd_kinematics <- function(analysis_mode, single_video_ID, 
                          DLC_data_dir, settings_file, kin_results_dir, 
                          plots, pk_vl_save) {
     
    if(analysis_mode == "DAQ") {
      data_dir    <- "./"
      results_dir <- kin_results_dir
      video.ID.no <- NULL
      video.filename.list <- list.files(data_dir, pattern = glob2rx("*.csv*"), full.names=FALSE)
      video.filename      <- video.filename.list[1]
      video.ID            <- str_split(video.filename, "DLC")[[1]][1]
      settings.list       <- list.files(data_dir, pattern = glob2rx("*.txt*"), full.names=FALSE)
      settings_path       <- settings_file
      kin_process(video.filename, video.ID, settings_path, data_dir, results_dir, plots, pk_vl_save)
    }

    
    if (analysis_mode == "single") {
      video.ID            <- single_video_ID
      video.ID.DLC        <- paste0(video.ID, "DLC")
      video.filename.list <- list.files(DLC_data_dir, pattern = glob2rx("*.csv*"), full.names=FALSE)
      video.filename.df   <- as.data.frame(video.filename.list)
      video.filename1     <- video.filename.df %>% filter(grepl(video.ID.DLC, video.filename.list))
      video.filename      <- video.filename1[[1]]
      settings_path       <- settings_file
      data_dir            <- paste0(DLC_data_dir, "/")
      results_dir         <- kin_results_dir
      kin_process(video.filename, video.ID, settings_path, data_dir, results_dir, plots, pk_vl_save)
    }
    
  
    if (analysis_mode == "batch") {
      data_dir            <- paste0(DLC_data_dir, "/")  # directory holding multiple .csv files
      results_dir         <- paste0(kin_results_dir, "/")
      settings_path       <- settings_file
      video.filename.list <- list.files(data_dir, pattern = glob2rx("*.csv*"), full.names=FALSE)
      
      # now loop through video.filename.list, analyzing each video one-by-one
      
      for(i in 1:length(video.filename.list)){
        video.filename    <- video.filename.list[i]
        video.ID            <- str_split(video.filename, "DLC")[[1]][1]
        kin_process(video.filename, video.ID, settings_path, data_dir, results_dir, plots, pk_vl_save)
      }
      
    }
  
}


  





  