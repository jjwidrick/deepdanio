#' Calculate kinematics on a stage-by-stage basis.
#'
#' Calculate overall distance, displacement, peak speed, and peak acceleration.
#'    Calculate appropriate kinematic variables for stage 1, 2, and 3.
#'    Compile all values into a new df (consisting of a single row for the
#'    particular trial under analysis.)
#'
#' @param df the results df
#' @param settings the experimental settings
#' @param pk.vl.final.df a df containing the transitions between stages
#' @return a new df consisting of a single row of kinematic values
#' @examples
#' @export
#'
#'



calcs_by_stage <- function(df, settings, pk.vl.final.df) {

# Step 1: prepare df

  # A: eliminate pre-stim rows from df, creating move.df
  move.df <- df %>% filter(move.ms >= 0)

  # B: delete 'orig.ms' and 'stim.ms' columns
  move.df <- move.df %>% select(-orig.ms, -stim.ms)



# Step 2: calc overall variables

  overall.first.row <- head(move.df, n = 1)
  overall.last.row  <- tail(move.df, n = 1)

  # overall distance
  overall.dist.BL <- overall.last.row$cum.dist.BL - overall.first.row$cum.dist.BL
  overall.dist.BL <- round(overall.dist.BL, 3)

  # final displacement
  final.displ.BL <- overall.last.row$displ.BL
  final.displ.BL <- round(final.displ.BL, 3)

  # peak speed
  overall.pk.spd.BL.s <- max(move.df$smo.spd.BL.s)
  overall.pk.spd.BL.s <- round(overall.pk.spd.BL.s, 3)

  # peak acceleration
  overall.pk.acc.BL.s2 <- max(move.df$acc.BL.s2)
  overall.pk.acc.BL.s2 <- round(overall.pk.acc.BL.s2, 0)
 # overall.pk.acc.BL.s2 <- sprintf("%6d", overall.pk.acc.BL.s2)



# Step 3: calc stage 1 variables

  # temp df derived from 'pk.vl.curve.dist.df'
  stg1.end             <- pk.vl.final.df %>% filter(stroke.no == 1)

  # temp df derived from 'move.ms'
  stg1.last.row        <- move.df %>% filter(move.ms == stg1.end$move.ms)
  stg1.all             <- move.df %>% filter(move.ms <= stg1.end$move.ms)

  # derived from stg1.end (from 'pk.vl.curve.dist.df')
  stg1.dist.BL         <- stg1.end$dist.BL
  stg1.dur.ms          <- stg1.end$move.ms
  stg1.ave.spd.BL.s    <- stg1.dist.BL/(stg1.dur.ms / 1000)
  stg1.ave.spd.BL.s    <- format(round(stg1.ave.spd.BL.s, 3), nsmall = 3)
  stg1.tail.final.curve.deg <- stg1.end$stroke.amp.deg

  # derived from stg1.last.row (from 'move.df')
  stg1.displ.BL        <- stg1.last.row$displ.BL
  stg1.exit.spd.BL.s   <- stg1.last.row$smo.spd.BL.s

  # derived from stg1.all (from 'move.df')
  #stg1.tail.stroke.max.vel.deg.ms <- max(abs(stg1.all$tail.vel.deg.ms))
  #   to prevent using an initial bend that is eliminated by pk.vl.threshold
  #   (which because of standardization always moves in negative direction,
  #   therefore its max vel would always be negative), changed from absolute value
  #   to actual value in calc of this stage only.
  stg1.tail.stroke.max.vel.deg.ms <- max(stg1.all$tail.vel.deg.ms)



# Step 3: calc stage 2 variables

  # temp df derived from 'pk.vl.curve.dist.df'
  stg2.end             <- pk.vl.final.df %>% filter(stroke.no == 2)

  # temp df derived from 'move.ms'
  stg2.last.row        <- move.df %>% filter(move.ms == stg2.end$move.ms)
  stg2.all             <- move.df %>% filter(move.ms > stg1.end$move.ms & move.ms <= stg2.end$move.ms)

  # derived from stg2.end
  stg2.dist.BL         <- stg2.end$dist.BL - stg1.end$dist.BL
  stg2.dur.ms          <- stg2.end$stroke.dur.ms
  stg2.ave.spd.BL.s    <- stg2.dist.BL/(stg2.dur.ms / 1000)
  stg2.ave.spd.BL.s    <- format(round(stg2.ave.spd.BL.s, 3), nsmall = 3)
  stg2.tail.stroke.pk.pk.deg <- stg2.end$stroke.amp.deg


  # derived from stg2.last.row
  stg2.displ.BL        <- stg2.last.row$displ.BL - stg1.last.row$displ.BL
  stg2.exit.spd.BL.s   <- stg2.last.row$smo.spd.BL.s
  stg2.chg.spd.BL.s    <- stg2.exit.spd.BL.s - stg1.exit.spd.BL.s

  # derived from stg1.all
  stg2.tail.stroke.max.vel.deg.ms <- max(abs(stg2.all$tail.vel.deg.ms))
  stg2.max.acc.BL.s2    <- max(stg2.all$acc.BL.s2)
  stg2.max.acc.BL.s2    <- sprintf("%6d", stg2.max.acc.BL.s2)



# Step 4: calc stage 3 variables

# HERE, THERE ARE BIG DIFFERENCES FROM THE FIRST TWO STAGES.
# The last row of 'pk.vl.curve.dist.df' is the last FULL stroke, NOT the end of the stage.
# To get the total distance, etc. you need to get the final row of 'move.ms'

    stg3.pk.vl           <- pk.vl.final.df %>% filter(stroke.no >= 3)
    stg3.pk.vl.start     <- head(stg3.pk.vl, n = 1)
    stg3.pk.vl.end       <- tail(stg3.pk.vl, n = 1)

    stg3.last.row        <- tail(move.df, n = 1)
    stg3.all             <- move.df %>% filter(move.ms > stg2.end$move.ms)


    stg3.total.dist.BL   <- stg3.last.row$cum.dist.BL  - stg2.last.row$cum.dist.BL
    stg3.total.displ.BL  <- stg3.last.row$displ.BL - stg2.last.row$displ.BL
    stg3.total.dur.ms    <- stg3.last.row$move.ms  - stg2.last.row$move.ms
    stg3.ave.spd.BL.s    <- stg3.total.dist.BL/(stg3.total.dur.ms / 1000)
    stg3.ave.spd.BL.s    <- format(round(stg3.ave.spd.BL.s, 3), nsmall = 3)

    stg3.exit.spd.BL.s   <- stg3.last.row$smo.spd.BL.s
    stg3.chg.spd.BL.s    <- stg3.exit.spd.BL.s - stg2.exit.spd.BL.s

    stg3.tail.stroke.max.vel.deg.ms <- max(abs(stg3.all$tail.vel.deg.ms))

    stg3.no.strokes      <- stg3.pk.vl.end$stroke.no - 2  # need to subtract the first 2 strokes
    stg3.no.tail.beats   <- stg3.no.strokes / 2

    stg3.full.stroke.dur.ms  <- stg3.pk.vl.end$move.ms - (stg1.dur.ms + stg2.dur.ms)
    stg3.full.stroke.dist.BL <- stg3.pk.vl.end$dist.BL - (stg1.dist.BL + stg2.dist.BL)

    stg3.ave.dist.stroke.BL  <- stg3.full.stroke.dist.BL / stg3.no.strokes
    stg3.ave.dist.stroke.BL  <- round(stg3.ave.dist.stroke.BL, 3)

    # as tail STROKES (to get as tail beats, i.e. TBD, TBF, TBA, divide by 2)
    stg3.ave.TSD.BL  <- stg3.ave.dist.stroke.BL
    stg3.ave.TSF.Hz  <- mean(stg3.pk.vl$stroke.Hz)
    stg3.ave.TSA.deg <- mean(stg3.pk.vl$stroke.amp.deg)

    stg3.ave.TSD.BL  <- round(stg3.ave.TSD.BL, 3)
    stg3.ave.TSF.Hz  <- round(stg3.ave.TSF.Hz, 0)
    stg3.ave.TSA.deg <- round(stg3.ave.TSA.deg, 0)



# Step 5: make results df

    video.ID      <- df$video.ID[1]
    BL.mm         <- df$BL.mm[1]
    body.bend.deg <- df$body.bend.deg[1]
    latency.ms    <- df$latency.ms[1]

    row.df <- tibble(
            video.ID,
            BL.mm,
            body.bend.deg,
            latency.ms,

            overall.dist.BL,
            final.displ.BL,
            overall.pk.spd.BL.s,
            overall.pk.acc.BL.s2,

            stg1.dur.ms,
            stg1.dist.BL,
            stg1.displ.BL,
            stg1.ave.spd.BL.s,
            stg1.exit.spd.BL.s,
            stg1.tail.final.curve.deg,
            stg1.tail.stroke.max.vel.deg.ms,

            stg2.dur.ms,
            stg2.dist.BL,
            stg2.displ.BL,
            stg2.ave.spd.BL.s,
            stg2.chg.spd.BL.s,
            stg2.exit.spd.BL.s,
            stg2.max.acc.BL.s2,
            stg2.tail.stroke.pk.pk.deg,
            stg2.tail.stroke.max.vel.deg.ms,

            stg3.total.dur.ms,
            stg3.total.dist.BL,
            stg3.total.displ.BL,
            stg3.ave.spd.BL.s,
            stg3.exit.spd.BL.s,
            stg3.chg.spd.BL.s,
            stg3.tail.stroke.max.vel.deg.ms,
            stg3.no.strokes,
            stg3.ave.TSD.BL,
            stg3.ave.TSF.Hz,
            stg3.ave.TSA.deg)

    row.df <- cbind(row.df, settings)

  return(row.df)

}
