#' Detect peaks and valleys
#'
#' what function does (sentence case, no period)
#'
#' @param df the results df
#' @param settings experimental settings
#' @return a df identifying peaks and valleys
#' @examples
#' @export



detect_pks_vls <- function(df, settings) {

  # this function uses 'tail.curve.zero', not 'tail.curve'

  video.ID <- df$video.ID[1]

# Step 1: create 'move.vec' AS VECTOR OF 'tail.curve.zero'
  move.vec <- df %>% filter(move.ms > 0) %>%
	                          pull(tail.curve.zero)

# Step 2: find peaks
  pk.df <- as.data.frame(findpeaks(move.vec))
  pk.df <- pk.df %>% rename(tail.curve.zero  = V1,
  													move.ms          = V2) %>%
  	                 mutate(pk.vl = "peak") %>%
  	                 select(pk.vl, move.ms, tail.curve.zero)

# Step 3: find valleys
  vl.df <- as.data.frame(findpeaks(-move.vec))
  vl.df <- vl.df %>% rename(tail.curve.zero   = V1,
  													move.ms           = V2) %>%
  	                        mutate(pk.vl           = "valley",
  															   tail.curve.zero = -1 * tail.curve.zero) %>%
  	                        select(pk.vl, move.ms, tail.curve.zero)

# Step 4: make combined peak and valley df, add video.ID,  and sort
  pk.vl.df <- rbind(pk.df, vl.df)
  pk.vl.df <- pk.vl.df %>% mutate(video.ID = video.ID, .before = pk.vl)
  pk.vl.df <- pk.vl.df %>% arrange(move.ms)


  return(pk.vl.df)

}



