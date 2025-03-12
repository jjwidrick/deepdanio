#' Get settings for the experiment.
#'
#' Open the settings file, add variables, make calibration calculations,
#'     and return the setting df used when analyzing this particular experiment.
#'
#' @param settings_path the path to the settings txt file
#' @return the settings data
#' @examples
#' @export



get_settings <- function(settings_path) {


# STEP 1: READ SETTINGS

  # read settings file
    settings <- read_tsv(settings_path)

  # get today's date
    date <- format(Sys.time(), '%Y-%m-%d')

  # add date to temp.df
    new.row <- c("extract.date", date)
    settings <- rbind (settings, new.row)

  # make df with proper variable classes
    settings <- settings %>% select(name, value) %>%
                      data.table::transpose(make.names = "name") %>%
                      mutate_at(vars(pix.per.mm, fps, total.ms, pre.stim.ms, total.move.ms,
                                     ang.smo.span, dis.smo.span, spd.smo.span,
                                     move.threshold, pk.vl.threshold, like.threshold),
                                as.numeric)

  # calculate additional calibration constants
    settings <- settings %>% mutate(sec.per.frame = 1/fps,
                        ms.per.frame  = sec.per.frame*1000,
                        mm.per.pix    = round( 1/pix.per.mm, 4))

  # re-order variables
    settings <- settings %>% select(pix.per.mm, mm.per.pix, fps, sec.per.frame, ms.per.frame,
                        total.ms, pre.stim.ms, total.move.ms, pt.for.dis.dpl, ang.smo.span,
                        dis.smo.span, spd.smo.span, move.detect, move.threshold,
                        stage.detect, pk.vl.threshold, like.threshold, extract.date)

   return(settings)
}


