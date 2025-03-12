#' Make a summary of results
#'
#' Compiles a summary of results, including
#'     overall diatance and displacement,
#'     peak velocity and accelration, and
#'     tail beat frequency and amplitude.
#'
#' @param df the results df
#' @param settings the experimental settings
#' @param row.df overall and stage-specific kinematic df
#' @return summary.df, a df with selected kinematic variables for use in plots
#' @examples
#' @export



make_summary <- function(df, settings, row.df) {

  summary.df <- row.df %>% select(video.ID, BL.mm,
                overall.dist.BL, final.displ.BL,
                overall.pk.spd.BL.s, overall.pk.acc.BL.s2,
                stg1.dur.ms, stg2.dur.ms, stg3.total.dur.ms,
                stg1.dist.BL, stg2.dist.BL, stg3.total.dist.BL,
                stg1.tail.final.curve.deg, stg2.tail.stroke.pk.pk.deg,
                stg3.no.strokes,
                stg3.ave.TSD.BL, stg3.ave.TSF.Hz, stg3.ave.TSA.deg) %>%
           rename("BL (mm)" = BL.mm,
                  "dist (BL)"           = overall.dist.BL,
                  "displ (BL)"           = final.displ.BL,
                  "pk spd (BL/s)"                 = overall.pk.spd.BL.s,
                  "pk accel (BL/s2)"                = overall.pk.acc.BL.s2,
                  "stg1 dur (ms)"              = stg1.dur.ms,
                  "stg2 dur (ms)"              = stg2.dur.ms,
                  "stg3 total dur (ms)"        = stg3.total.dur.ms,
                  "stg1 dist (BL)"              = stg1.dist.BL,
                  "stg2 dist (BL)"              = stg2.dist.BL,
                  "stg3 total dist (BL)"        = stg3.total.dist.BL,
                  "stg1 final curve (deg)"     = stg1.tail.final.curve.deg,
                  "stg2 stroke pk-pk amp (deg)" = stg2.tail.stroke.pk.pk.deg,
                  "stg3 no. full strokes"           = stg3.no.strokes,
                  "stg3 ave stroke dist (BL)"   = stg3.ave.TSD.BL,
                  "stg3 ave stroke freq (Hz)"  = stg3.ave.TSF.Hz,
                  "stg3 ave stroke amp (deg)" = stg3.ave.TSA.deg)

  summary.df %>% kbl() %>% kable_styling(font_size = 11) %>%
                           scroll_box(width = "900px", height = "150px")
}
