#' Number fish
#'
#' Consecutively number fish for a more compact way to identify individual fish
#'
#' @param info.df the info.df created by kin_compile
#' @return the info.df with the new column 'fish.no'
#' @examples
#' @export


number_fish <- function(info.df) {

# number each fish consecutively (fish.no) ----------------------------------

    # order by video.ID
    info.df <- info.df %>% arrange(video.ID)

    # get no.fish and no.rows
    #no.videos <- length(unique(info.df$video.ID))
    no.fish   <- length(unique(info.df$fish.ID))
    no.rows   <- length(info.df$fish.ID)

    # start by creating column with fish.no = 1 for all fish
    info.df <- info.df %>% mutate(fish.no = 1) %>%
      relocate(fish.no, .after = fish.ID)

    # start counting at "1"
    fish.count <- 1
    current.row <- info.df[[1, "fish.ID"]]

    # loop - if next row of fish.ID changes, change fish.no
    for(i in 1:no.rows) {
      next.row <- info.df$fish.ID[i]
      if(next.row == current.row) {
        info.df$fish.no[i] <- fish.count
      } else {
        fish.count = fish.count + 1
        info.df$fish.no[i] <- fish.count
      }
      current.row <- next.row
    }

    return(info.df)
}

