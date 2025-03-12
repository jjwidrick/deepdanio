#' Identify kinematic variables at each stage transition.
#'
#' This function takes the 'pl_vl_df' and numbers the
#'     stage transition points and calculates variables
#'     at each stage transition. These values are then used
#'     in subsequent functions.
#'
#' @param df the results df
#' @param pk.vl.df.adj a df with tail curve peaks and valleys
#' @return pk.vl.final.df, a df with stage transition values
#' @examples
#' @export



pk_vl_calcs <- function(df, pk.vl.df.adj, settings) {

# Step 0: get total.move.ms
  total.move.ms <- settings$total.move.ms

# Step 1: add a row with zero time point ("start") from df
  zero.df           <- df %>% filter(move.ms == 0)
  video.ID          <- zero.df$video.ID
  zero.ms           <- zero.df$move.ms
  zero.curve        <- zero.df$tail.curve.zero
  zero.curve.rotate <- zero.df$tail.curve.zero.rotate
  zero.pk.vl        <- "start"
  insert            <- c(video.ID, zero.pk.vl, zero.ms, zero.curve, zero.curve.rotate)
  pk.vl.df.adj      <- rbind(insert, pk.vl.df.adj)

# Step 2: sort by move.ms (make sure move.ms and tail.curve.zero.rotate are numeric for sorting)
  pk.vl.df.adj$move.ms <- as.numeric(as.character(pk.vl.df.adj$move.ms))
  pk.vl.df.adj$curve   <- as.numeric(as.character(pk.vl.df.adj$tail.curve.zero.rotate))
  pk.vl.df.adj         <- pk.vl.df.adj %>% arrange(move.ms)

# Step 3: create 'extreme.no' column that numbers stroke transition points in tail curve
  pk.vl.df.adj <- pk.vl.df.adj %>% mutate(extreme.no = row_number() - 1) %>%   # - 1 to account for zero row
                           relocate(extreme.no, .before = move.ms) %>%
                           rename(curve.deg = curve)

# Step 4: truncate pk.vl.df.adj at 'total.move.ms'
  truncated.pk.vl.df.adj <- pk.vl.df.adj %>% filter(move.ms <= total.move.ms)
  move.ms.list <- truncated.pk.vl.df.adj$move.ms
  max.extreme.no <- nrow(truncated.pk.vl.df.adj)

# Step 5: using move.ms.list from 'truncated.pk.vl.df.adj', get the
#           corresponding distances from 'wide.df' to make 'dist.df'
    dist.df <- NULL
    for(i in 1:max.extreme.no) {
      this.move.ms <- move.ms.list[i]
      dist.row     <- df %>% filter(move.ms == this.move.ms)
      dist         <- dist.row$cum.dist.BL
      dist.df      <- rbind(dist.df, dist)
    }

# Step 6: merge distances into pk.vl.df.adj, forming 'new.pk.vl.df.adj'
  pk.vl.final.df <- cbind(truncated.pk.vl.df.adj, dist.df)
  row.names(pk.vl.final.df) <- NULL
  pk.vl.final.df
  pk.vl.final.df <- pk.vl.final.df %>%
                    rename(dist.BL = dist.df) %>%
                    select(video.ID, pk.vl, extreme.no, move.ms, curve.deg, dist.BL)

# Step 7: calculations
  # add stroke column
  pk.vl.final.df <- pk.vl.final.df %>% mutate(stroke.no = extreme.no)

  # cal duration of each stroke
  pk.vl.final.df <- pk.vl.final.df %>% mutate(stroke.dur.ms = abs(move.ms - lag(move.ms)))

  # cal freq of each stroke and each tail beat (2 strokes per tail beat)
  pk.vl.final.df <- pk.vl.final.df %>% mutate(stroke.Hz = 1000 / stroke.dur.ms)

  # cal dist covered by each stroke
  pk.vl.final.df <- pk.vl.final.df %>% mutate(stroke.dist.BL = abs(dist.BL - lag(dist.BL)))

  # cal pk to pk amp of each stroke
  pk.vl.final.df <- pk.vl.final.df %>% mutate(stroke.amp.deg = abs(curve.deg - lag(curve.deg)))

  # round
  pk.vl.final.df <- pk.vl.final.df %>%
                    mutate(across(c('stroke.Hz', 'stroke.dist.BL', 'stroke.amp.deg'), round, 3))


return(pk.vl.final.df)

}

