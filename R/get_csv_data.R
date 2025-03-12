#' Open the .csv coordinate file
#'
#' Open the .csv output coordinate data file from DeepLabCut.
#'     Obtain a video.ID from the .csv filename.
#'     Read the data and create an output dataframe.
#'
#' @param data_dir The data directory path. This is most likely "./" when using
#'    to evaluate current video.
#' @param settings The setting for this experiment.
#' @return a results df that forms the starting point for all subsequent analyses
#' @examples
#' @export
#'


get_csv_data <- function(video.filename, video.ID, data_dir, settings) {

# Step 1: get video name and make video-ID
#    video.filename <- list.files(data_dir, pattern = glob2rx("*DLC*"), full.names=FALSE)
#    video.ID       <- str_sub(video.filename, start = 1L, end = 5L)
# THIS INFO NOW EXTRACTED BEFORE CALLING THIS FUNCTION'

# Step 2: read DLC.csv file
    data_path <- paste(data_dir, video.filename, sep="")
    df        <- read_csv(data_path, skip=3,
                        col_names = c("orig.frame",
                                      "TSx", "TSy", "TS.like",
                                      "S1x", "S1y", "S1.like",
                                      "S2x", "S2y", "S2.like",
                                      "T1x", "T1y", "T1.like",
                                      "T2x", "T2y", "T2.like",
                                      "T3x", "T3y", "T3.like",
                                      "T4x", "T4y", "T4.like"))

# Step 3: clean up file
    df <- df %>% mutate(video.ID = video.ID,
                        orig.ms  = orig.frame * (settings$sec.per.frame * 1000))
    df <- df %>% relocate(video.ID, .before = orig.frame)
    df <- df %>% relocate(orig.ms,  .after = video.ID)
    df <- df %>% select(-orig.frame)


# Step 4: convert pixels to mm
    mm.per.pix <- settings$mm.per.pix

    df <- df %>%
          # a) first convert the x coordinates
                 mutate(TSx.mm = TSx * mm.per.pix) %>%
                 mutate(S1x.mm = S1x * mm.per.pix) %>%
                 mutate(S2x.mm = S2x * mm.per.pix) %>%
                 mutate(T1x.mm = T1x * mm.per.pix) %>%
                 mutate(T2x.mm = T2x * mm.per.pix) %>%
                 mutate(T3x.mm = T3x * mm.per.pix) %>%
                 mutate(T4x.mm = T4x * mm.per.pix) %>%
          # b) now convert the y coordinates
                 mutate(TSy.mm = TSy * mm.per.pix) %>%
                 mutate(S1y.mm = S1y * mm.per.pix) %>%
                 mutate(S2y.mm = S2y * mm.per.pix) %>%
                 mutate(T1y.mm = T1y * mm.per.pix) %>%
                 mutate(T2y.mm = T2y * mm.per.pix) %>%
                 mutate(T3y.mm = T3y * mm.per.pix) %>%
                 mutate(T4y.mm = T4y * mm.per.pix) %>%
          # c) format columns
                 mutate(across(where(is.numeric), round, 3)) %>%
          # d) clean up
                 select(video.ID, orig.ms,
                        TSx.mm, TSy.mm, TS.like,
                        S1x.mm, S1y.mm, S1.like,
                        S2x.mm, S2y.mm, S2.like,
                        T1x.mm, T1y.mm, T1.like,
                        T2x.mm, T2y.mm, T2.like,
                        T3x.mm, T3y.mm, T3.like,
                        T4x.mm, T4y.mm, T4.like)

# Step 5: return df's

  return(df)

}
