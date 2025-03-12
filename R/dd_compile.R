#' Compile experiments
#'
#' Assemble metadata and individual kinematic results into complete df's
#'
#' @param project a name for the project
#' @param data_dir the path to the data directory ("./" for current dir)
#' @param results_dir the path to the directory used for storing results
#' @param metadata_file the path to the metadata file
#' @return a 'by ms' df and a 'mean' df
#' @examples
#' @export


dd_compile <- function(project, data_dir, results_dir, metadata_file) {

  
# COMPLETE directory paths ---------------------------------------------
  data_path    <- paste0(data_dir, "/")
  results_path <- paste0(results_dir, "/")

# GET run date ---------------------------------------------------------
  run.date <- format(Sys.Date(), '%y%m%d')

  

# prep info file ---------------------------------------------------

# open info file
    info.df <- read_csv(metadata_file)

# convert video.ID  to character
    info.df$video.ID <- as.character(info.df$video.ID)

# number each fish
    numbered.info.df <- number_fish(info.df)

# select variables
    select.info.df <- numbered.info.df %>%
                      select(video.ID, fish.ID, fish.no, strain, clutch,
                             dpf, phenotype, genotype, treatment, degC)




# prep trials df ----------------------------------------------------

# make a list of the "*one_row.csv" files
    file.list <- list.files(data_path,
                            pattern="*one_row_*",
                            full.names=FALSE)

# loop through list, making the df
    df1 <- NULL
    trial.df <- NULL
    filepath <- NULL

        for(i in 1:length(file.list)) {
              filepath <- paste0(data_path, file.list[i])
        		  df1 <- read_csv(filepath)
        		  trial.df <- rbind(trial.df, df1)
        }



# join info and trials df ----------------------------------------------------

final.trial.df <- left_join(trial.df, select.info.df, by = "video.ID")

  # remove trial.df from memory
    rm(trial.df)

final.trial.df <- final.trial.df %>% select(video.ID, strain, clutch, dpf, fish.ID, fish.no,
                          phenotype, genotype, treatment, degC,
                          everything())



# add new columns to trials.df ------------------------------------------------

# add trial.no
    final.trial.df <- final.trial.df %>%
                      arrange(video.ID) %>%
                      group_by(fish.ID) %>%
                      mutate(trial.no = row_number(), .before=phenotype)

# add max.no.trials
    final.trial.df <- final.trial.df %>%
                      group_by(fish.ID) %>%
                      mutate(max.no.trials = max(trial.no)) %>%
                      relocate(max.no.trials, .before=phenotype)

# add compile.date
    final.trial.df <- final.trial.df %>%
                      mutate(compile.date = run.date) %>%
                      relocate(extract.date, .before="compile.date")

# save new df
    filename1 <- paste0(results_path, "by_trials_", run.date, ".csv")
    write_csv(final.trial.df, filename1)



    
# make mean.df ------------------------------------------------------------

# Make a df consisting of the trials collapsed into a mean for each fish

# collapse across trials
  mean.df <- final.trial.df %>%
             arrange(video.ID) %>%
             group_by(fish.ID, fish.no, clutch, strain, phenotype, genotype, dpf, treatment) %>%
             summarize(
            max.no.trials             = mean(max.no.trials),
            BL.mm                     = mean(BL.mm),
            body.bend.deg             = mean(body.bend.deg),
            latency.ms                = mean(latency.ms),

            overall.dist.BL           = mean(overall.dist.BL),
            final.displ.BL            = mean(final.displ.BL),
            overall.pk.spd.BL.s       = mean(overall.pk.spd.BL.s),
            overall.pk.acc.BL.s2      = mean(overall.pk.acc.BL.s2),

            stg1.dur.ms               = mean(stg1.dur.ms),
            stg1.dist.BL              = mean(stg1.dist.BL),
            stg1.displ.BL             = mean(stg1.displ.BL),
            stg1.ave.spd.BL.s         = mean(stg1.ave.spd.BL.s),
            stg1.exit.spd.BL.s        = mean(stg1.exit.spd.BL.s),
            stg1.tail.final.curve.deg = mean(stg1.tail.final.curve.deg),
            stg1.tail.stroke.max.vel.deg.ms = mean(stg1.tail.stroke.max.vel.deg.ms),

            stg2.dur.ms               = mean(stg2.dur.ms),
            stg2.dist.BL              = mean(stg2.dist.BL),
            stg2.displ.BL             = mean(stg2.displ.BL),
            stg2.ave.spd.BL.s         = mean(stg2.ave.spd.BL.s),
            stg2.chg.spd.BL.s         = mean(stg2.chg.spd.BL.s),
            stg2.exit.spd.BL.s        = mean(stg2.exit.spd.BL.s),
            stg2.max.acc.BL.s2        = mean(stg2.max.acc.BL.s2),
            stg2.tail.stroke.pk.pk.deg = mean(stg2.tail.stroke.pk.pk.deg),
            stg2.tail.stroke.max.vel.deg.ms = mean(stg2.tail.stroke.max.vel.deg.ms),

            stg3.total.dur.ms         = mean(stg3.total.dur.ms),
            stg3.total.dist.BL        = mean(stg3.total.dist.BL ),
            stg3.total.displ.BL       = mean(stg3.total.displ.BL),
            stg3.ave.spd.BL.s         = mean(stg3.ave.spd.BL.s),
            stg3.exit.spd.BL.s        = mean(stg3.exit.spd.BL.s),
            stg3.chg.spd.BL.s         = mean(stg3.chg.spd.BL.s),
            stg3.tail.stroke.max.vel.deg.ms = mean(stg3.tail.stroke.max.vel.deg.ms),
            stg3.no.strokes           = mean(stg3.no.strokes),
            stg3.ave.TSD.BL           = mean(stg3.ave.TSD.BL),
            stg3.ave.TSF.Hz           = mean(stg3.ave.TSF.Hz),
            stg3.ave.TSA.deg          = mean(stg3.ave.TSA.deg))

# round numeric values
  mean.df <- mean.df %>% mutate(across(where(is.numeric), round, 3))

# sort by fish.no
  mean.df <- mean.df %>% arrange(fish.no)

# save df
  filename2 <- paste0(results_path, "fish_means_", run.date, ".csv")
  write_csv(mean.df, filename2)



# make by_ms.df --------------------------------------------------------------

# make a list of the "*one_row.csv" files
    file.list <- list.files(data_path,
                            pattern="*all_ms_*",
                            full.names=FALSE)
# loop through list
    df4 <- NULL
    ms.df <- NULL
    filepath <- NULL

    for(i in 1:length(file.list)) {
          filepath <- paste0(data_path, file.list[i])
    		  df4 <- read_csv(filepath)
    		  ms.df <- rbind(ms.df, df4)
    }

# join with info
    ms.df <- left_join(ms.df, select.info.df, by = "video.ID")

# add trial.no column
    ms.df <- ms.df %>% arrange(video.ID, orig.ms) %>%
                   group_by(fish.ID) %>%
                   mutate(trial.no = as.numeric(factor(video.ID)))

# add max.no.trials
    ms.df <- ms.df %>% group_by(fish.ID) %>%
                       mutate(max.no.trials = max(trial.no)) %>%
                       relocate(max.no.trials, .before=strain)

# relocate fish.info variables
    ms.df <- ms.df %>% select(video.ID, trial.no, max.no.trials, strain, clutch, dpf, fish.ID, fish.no,
                          phenotype, genotype, treatment, degC,
                          everything())
# save
  filename3 <- paste0(results_path, "by_ms_", run.date, ".csv")
  write_csv(ms.df, filename3)



  
# clean up ------------------------------------------------------------------

  invisible(gc())

}


