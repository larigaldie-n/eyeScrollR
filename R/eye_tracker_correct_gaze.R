# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

require(tidyverse)

shift_image_by_dimension <- function(coordinate, shift_before, shift_after, screen_dimension, outside_image_is_na)
{
  if (is.na(coordinate) || (outside_image_is_na && (coordinate < shift_before || coordinate > (screen_dimension - (shift_after + shift_before)))))
  {
    return(NA)
  }
  else
  {
    return(coordinate - shift_before)
  }
}

resolve_anchor_box <- function(array_anchors, x, y)
{
  for (i in 1:dim(array_anchors)[3])
  {
    if(x >= array_anchors[1,1,i] && x <= array_anchors[3,1,i])
    {
      if(y >= array_anchors[2,1,i] && y <= array_anchors[4,1,i])
      {
        return (y + (array_anchors[4,2,i] - array_anchors[4,1,i]))
      }
    }
  }
  return (NA)
}

check_anchors_rules <- function(anchors, rules)
{
  if (!is.list(anchors))
  {
    stop("anchors must be an empty list or a list of arrays")
  }
  for (anchor in anchors)
  {
    if (!is.array(anchor))
    {
      stop("anchors must be an empty list or a list of arrays")
    }
    else if (dim(anchor)[1]!=4 || dim(anchor)[2]!=2)
    {
      stop("anchor arrays must be 4x2 matrices of coordinates")
    }
    else
    {
      for (i in 1:dim(anchor)[3])
      {
        if ((anchor[3,1,i] - anchor[1,1,i] != anchor[3,2,i] - anchor[1,2,i]) || (anchor[4,1,i] - anchor[2,1,i] != anchor[4,2,i] - anchor[2,2,i]))
        {
          stop("origin (screen) and destination (image) anchor rectangles must be the same size")
        }
      }
    }
  }
  if (!is.list(rules))
  {
    stop("rules must be an empty list or a list of functions")
  }
  for (rule in rules)
  {
    if (typeof(rule)!="closure")
    {
      stop("rules must be an empty list or a list of functions")
    }
  }
  if (length(rules) > length(anchors))
  {
    stop("There can't be more rules than anchor arrays")
  }
  while (length(rules) < length(anchors))
  {
    rules <- append(rules, rule_true)
  }
  return(rules)
}

rule_true <- function (timestamp, event, x, y, array_anchors, flag, scroll)
{
  return (TRUE)
}

rule_before_scrolling <- function (timestamp, event, x, y, array_anchors, flag, scroll)
{
  if (scroll < 30)
  {
    return (TRUE)
  }
  else
  {
    return(FALSE)
  }
}

rule_after_scrolling <- function (timestamp, event, x, y, array_anchors, flag, scroll)
{
  if (scroll >= 30)
  {
    return (TRUE)
  }
  else
  {
    return(FALSE)
  }
}

check_rules_true <- function(rules, data_line, flags, anchors, scroll)
{
  for (rule_num in 1:length(rules))
  {
    flags[rule_num] <- rules[[rule_num]](data_line$timestamp, data_line$event, data_line$corrected_x, data_line$corrected_y, anchors[[rule_num]], flags[rule_num], scroll)
  }
  return(flags)
}

enforce_rules <- function(flags, anchors, data_line)
{
  y <- NA
  for (flag_num in 1:length(flags))
  {
    if (flags[flag_num])
    {
      y <- resolve_anchor_box(anchors[[flag_num]], data_line$corrected_x, data_line$corrected_y)
      if (!is.na(y))
      {
        break
      }
    }
  }
  return(y)
}

shift_scroll <- function(event, data_line, scroll, min_scroll, max_scroll)
{
  if (grepl(event, data_line$event, fixed=TRUE))
  {
    scroll_delta = strtoi(unlist(strsplit(unlist(strsplit(data_line$event, ";", fixed=TRUE))[5], ":", fixed = TRUE))[2])
    if (scroll_delta<0)
    {
      scroll <- min(c(scroll - scroll_delta, max_scroll))
    }
    else if (scroll_delta>0)
    {
      scroll <- max(c(scroll - scroll_delta, min_scroll))
    }
  }
  return (scroll)
}

# prepare_smooth_scroll <- function (event, data_line, smooth_scroll, smooth_scroll_table, scroll, min_scroll, max_scroll)
# {
#   if (grepl(event, data_line$event, fixed=TRUE))
#   {
#     scroll_delta = strtoi(unlist(strsplit(unlist(strsplit(data_line$event, ";", fixed=TRUE))[5], ":", fixed = TRUE))[2])
#     if (max(smooth_scroll_table$timestamp) < data_line$timestamp)
#     {
#       if (scroll_delta<0)
#       {
#         scroll_delta <- min(c(scroll - scroll_delta, max_scroll))
#         smooth_scroll_table <- data.frame(timestamp = data_line$timestamp:(data_line$timestamp+smooth_scroll-1), delta_scroll = scroll - seq(0,scroll_delta, length.out=smooth_scroll))
#       }
#       else if (scroll_delta>0)
#       {
#         scroll_delta <- max(c(scroll - scroll_delta, min_scroll))
#         smooth_scroll_table <- data.frame(timestamp = data_line$timestamp:(data_line$timestamp+smooth_scroll-1), delta_scroll = scroll - seq(0,scroll_delta, length.out=smooth_scroll))
#       }
#     }
#     else
#     {
#       smooth_scroll_table <- dplyr::filter(smooth_scroll_table, timestamp>=data_line$timestamp)
#       if (scroll_delta<0)
#       {
#         scroll_delta <- min(c(scroll - scroll_delta, max_scroll))
#         smooth_scroll_table <- data.frame(timestamp = data_line$timestamp:(data_line$timestamp+smooth_scroll-1), delta_scroll = scroll - seq(0,scroll_delta, length.out=smooth_scroll))
#       }
#       else if (scroll_delta>0)
#       {
#         scroll_delta <- max(c(scroll - scroll_delta, min_scroll))
#         smooth_scroll_table <- data.frame(timestamp = data_line$timestamp:(data_line$timestamp+smooth_scroll-1), delta_scroll = scroll - seq(0,scroll_delta, length.out=smooth_scroll))
#       }
#     }
#   }
#   return (smooth_scroll_table)
# }

# test_anchors <- list(array(c(c(0,0,1902,95), c(0,0,1902,95), c(1607,96,1902,905), c(1607,96,1902,905)), dim = c(4,2,2)), array(c(c(0,0,1902,63), c(0,32,1902,95), c(1607,63,1902,900), c(1607,63,1902,900)), dim = c(4,2,2)), array(c(c(1607,906,1920,1080), c(1607,6481,1920,6655)), dim = c(4,2,1)))
# test_rules = list(rule_before_scrolling, rule_after_scrolling, rule_true)
# eye_tracker_gaze_scroll(anchors = test_anchors, rules = test_rules)

#' @title eye_tracker_gaze_scroll
#'
#' @description corrects the eye-tracking coordinate data to fit in an image
#'     scrolled vertically by the participant
#'
#' @param file_name The name of the .csv file with the coordinates & input events
#' @param time_shift A time shift parameter to synchronize the .csv file with a video recording
#' @param smooth_scroll Time it takes for a scroll to complete. Usually 135ms on Windows computer. 0 means instantaneous scroll (in ms; default = 135)
#' @param timestamp_start The starting timestamp at which the participant was watching the scrollable
#' @param timestamp_stop The final timestamp at which the participant was watching the scrollable
#' @param screen_width The width of the screen on which the experiment was done (in pixels)
#' @param screen_height The height of the screen on which the experiment was done (in pixels)
#' @param shift_top The number of pixels above the area of interest on the screen (and therefore, outside of the image)
#' @param shift_left Same, but on the left of the AOI
#' @param shift_bottom Same, but below the AOI
#' @param shift_right Same, but on the right of the AOI
#' @param image_height The total height of the image (in pixels)
#' @param image_width The total width of the image (in pixels)
#' @param starting_scroll [Optional] If the participant did not start watching the scrollable setting from the top, you can indicate the y coordinate at which he started here (in pixels)
#' @param output_file The name of the output .csv file
#' @param anchors A list of potentially immovable areas inside the scrollable (e.g. anchored menus in a website) - see manual for a correct usage
#' @param rules A list of functions that can act as rules to activate/deactivate immovables areas in the scrollable (e.g. a menu that disappears after X pixels have been scrolled) - See manual for a correct
#' @param outside_image_is_na Indicates if values outside the AOI (e.g. the windows bar) should be set to NA or kept in the file. Coordinates above/before the AOI will become negative.
#' @param drop_na Indicates if NA y values should be dropped in the final file/dataset
#'
#' @return The Dataset with corrected x and y's
#' @examples
#' data(toydata)
#' output_table <- overview_tab(dat = toydata, id = ccode, time = year)
#' @export
eye_tracker_gaze_scroll <- function (file_name = "mouse events and fixation coordinates.csv", time_shift=388, smooth_scroll=135, timestamp_start=53753, timestamp_stop=113579, screen_width=1920, screen_height=1080, shift_top=88, shift_left=0, shift_bottom=50, shift_right=0, image_height = 6655, image_width=1920, starting_scroll = 0, output_file = "eye_tracker_gaze_corrected.csv", anchors = list(array(c(c(0,0,1902,95), c(0,0,1902,95), c(1607,96,1902,905), c(1607,96,1902,905)), dim = c(4,2,2)), array(c(c(0,0,1902,63), c(0,32,1902,95), c(1607,63,1902,900), c(1607,63,1902,900)), dim = c(4,2,2)), array(c(c(1607,906,1920,1080), c(1607,6481,1920,6655)), dim = c(4,2,1))), rules = list(rule_before_scrolling, rule_after_scrolling, rule_true), outside_image_is_na = TRUE, drop_na=TRUE)
{
  #TODO: Smooth scrolling? Apparently it's 135ms to scroll
  #TODO: column names as parameters?
  event <- "WM_MOUSEWHEEL"
  eyes_data <- read.csv(file = file_name, header = FALSE, colClasses = c("NULL", rep(NA, 14)), col.names = c("", "timestamp", "U1", "U2", "U3", "U4", "U5", "U6", "U7", "event_controller", "event", "U8", "U9", "x", "y"))
  eyes_data$timestamp <- eyes_data$timestamp - time_shift
  eyes_data <- dplyr::filter(eyes_data, timestamp > timestamp_start, timestamp < timestamp_stop)
  smooth_scroll_table <- data.frame(timestamp = 0, scroll_shift = 0)
  scroll <- starting_scroll
  corrected_y <- c()
  min_scroll <- 0
  rules <- check_anchors_rules(anchors, rules)
  flags <- c(rep(TRUE, length(rules)))
  max_scroll <- image_height - screen_height - shift_top - shift_bottom
  eyes_data$corrected_y <- sapply(eyes_data$y, shift_image_by_dimension, shift_before = shift_top, shift_after = shift_bottom, screen_dimension = screen_height, outside_image_is_na)
  eyes_data$corrected_x <- sapply(eyes_data$x, shift_image_by_dimension, shift_before = shift_left, shift_after = shift_right, screen_dimension = screen_width, outside_image_is_na)

  for (line in 1:dim(eyes_data)[1])
  {
    # prepare_smooth_scroll(event, eyes_data[line ,], smooth_scroll, smooth_scroll_table, scroll, min_scroll, max_scroll)
    scroll <- shift_scroll(event, eyes_data[line ,], scroll, min_scroll, max_scroll)
    flags <- check_rules_true(rules, eyes_data[line, ], flags, anchors, scroll)
    if (!is.na(eyes_data[line, ]$corrected_y))
    {
      corrected_y[line] <- enforce_rules(flags, anchors, eyes_data[line, ])
      if (is.na(corrected_y[line]))
      {
        corrected_y[line] <- eyes_data[line, ]$corrected_y+scroll
      }
    }
    else
    {
      corrected_y[line] <- NA
    }
  }

  eyes_data$corrected_y <- -corrected_y
  # eyes_data$corrected_y <- image_height -corrected_y
  if (drop_na)
  {
    eyes_data <- eyes_data[complete.cases(eyes_data[, 'corrected_y']),]
  }
  write.csv(eyes_data, file = output_file, row.names = FALSE, na="")
  return(eyes_data)
}

#TODO: working heatmap
generate_heatmap <- function(data)
{
  # ggplot(data, aes(corrected_x,corrected_y))  +
  #   annotation_raster(img, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
  #   stat_density2d(geom = "polygon", aes(fill=..level..)) +
  #   geom_point(size=0.5)+
  #   scale_fill_gradient(low="green",high="red") +
  #   scale_x_continuous(limits=c(0,dim(img)[2]),expand=c(0,0))+
  #   scale_y_continuous(limits=c(0,dim(img)[1]),expand=c(0,0))+
  #   coord_fixed()


  # return(ggplot(data = data) + aes(x = corrected_x, y = corrected_y, color = 'red', fill = 'red')+
  #   annotation_raster(img, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
  #   geom_point(alpha = 0.5, shape = 21, show.legend=FALSE) +
  #     scale_x_continuous(limits=c(0,dim(img)[2]),expand=c(0,0))+
  #     scale_y_continuous(limits=c(0,dim(img)[1]),expand=c(0,0))+
  #   ggtitle('heatmap'))

  ggplot(data , aes(x = corrected_x, y = corrected_y, color = 'red', fill = 'red'))+
           geom_point(alpha = 0.5, shape = 21, show.legend=FALSE) +
           xlim(0, 1920)+
           ylim(-7000, 0)+
           ggtitle('heatmap')

}
