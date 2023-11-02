# Written by Nathanael Larigaldie

#' @title Get possible scroll_lag arguments
#'
#' @description Returns a list of suggested scroll_lag arguments under 100ms
#' as a function of monitor refresh rate and the next frame rank
#'
#' @param refresh_rate The refresh rate of the monitor
#' @param n_frame The next frame rank (e.g. n=1 for the next frame)
#'
#' @return Either prints a list of different possible scroll_lag arguments under
#' 100ms, or returns the exact scroll_lag for the specified n_frame
#' @examples
#' # Display all suggested scroll_lag arguments for a 120Hz monitor:
#' get_scroll_lag(refresh_rate = 120)
#'
#' # Get the scroll_lag argument assuming that, on average, changes will be
#' # displayed after 3 monitor frames on a 60Hz monitor:
#' get_scroll_lag(refresh_rate = 60, n_frame = 3)
#' @export
get_scroll_lag <- function(refresh_rate = 60, n_frame = 0)
{
  scroll_lag <- c()
  if(refresh_rate <= 0)
  {
    refresh_rate = 60
  }
  if(n_frame < 0)
  {
    n_frame = 0
  }
  if(n_frame == 0)
  {
    n_frame <- 1
    while((scr_lag = 0.5 * (1 / refresh_rate) * 1000 +
           (n_frame - 1) * (1 / refresh_rate) * 1000) < 100)
    {
      scroll_lag[length(scroll_lag) + 1] <- scr_lag
      n_frame <- n_frame + 1
    }
    reply <- paste0("Suggested scroll_lag under 100ms with a ", refresh_rate,
                    "Hz monitor:\n",
                    "-----------------------------------------------------",
                    "\n", "next frame: ", round(scroll_lag[1], 3), "\n")

    for(i in (seq_len(length(scroll_lag[-1])) + 1))
    {
      reply <- paste0(reply, i, " frames: ", round(scroll_lag[i], 3), "\n")
    }
    cat(reply)
  }
  else
  {
    return(0.5 * (1 / refresh_rate) * 1000 +
           (n_frame - 1)*(1 / refresh_rate) * 1000)
  }
}
