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

#' @export
rule_true <- function (timestamp, event, x, y, array_anchors, flag, scroll)
{
  return (TRUE)
}

#' @export
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

#' @export
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
  for (rule_num in seq_len(length(rules)))
  {
    flags[rule_num] <- rules[[rule_num]](data_line$timestamps, data_line$event, data_line$corrected_x, data_line$corrected_y, anchors[[rule_num]], flags[rule_num], scroll)
  }
  return(flags)
}

enforce_rules <- function(flags, anchors, data_line)
{
  y <- NA
  for (flag_num in seq_len(length(flags)))
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

shift_scroll <- function(event, data_line, scroll, min_scroll, max_scroll, scroll_pixels)
{
  if (grepl(event, data_line$event, fixed=TRUE))
  {
    scroll_delta <- strtoi(unlist(strsplit(unlist(strsplit(data_line$event, ";", fixed=TRUE))[5], ":", fixed = TRUE))[2])
    if (scroll_delta<0)
    {
      scroll <- min(c(scroll + scroll_pixels, max_scroll))
    }
    else if (scroll_delta>0)
    {
      scroll <- max(c(scroll - scroll_pixels, min_scroll))
    }
  }
  return (scroll)
}

#' @export
scroll_calibration <- function(screen_width = 2560, screen_height = 1440, shift_top = 103, shift_left = 0, shift_bottom = 40, shift_right = 0, scroll_pixels = 100)
{
  return(as.list(environment()))
}

# columns ?

# test_anchors <- list(array(c(c(0,0,1902,95), c(0,0,1902,95), c(1607,96,1902,905), c(1607,96,1902,905)), dim = c(4,2,2)), array(c(c(0,0,1902,63), c(0,32,1902,95), c(1607,63,1902,900), c(1607,63,1902,900)), dim = c(4,2,2)), array(c(c(1607,906,1920,1080), c(1607,6481,1920,6655)), dim = c(4,2,1)))
# test_rules = list(rule_before_scrolling, rule_after_scrolling, rule_true)
# img <- readPNG()
# data <- eye_tracker_gaze_scroll(anchors = test_anchors, rules = test_rules)

#' @title eye_tracker_gaze_scroll
#'
#' @description corrects the eye-tracking coordinate data to fit in a webpage
#'     scrolled vertically by the participant
#'
#' @param file_name The name of the .csv file with the coordinates & input events
#' @param time_shift A time shift parameter to synchronize the .csv file with a video recording (e.g. if the recording started after the start of the .csv)
#' @param timestamp_start The starting timestamp from the .csv file at which the participant was watching the scrollable (AFTER time_shift applied)
#' @param timestamp_stop The final timestamp from the .csv file at which the participant was watching the scrollable (AFTER time_shift applied)
#' @param image_height The total height of the image (in pixels)
#' @param image_width The total width of the image (in pixels)
#' @param starting_scroll [Optional] If the participant did not start watching the webpage from the top, you can indicate the y coordinate at which he started here (in pixels)
#' @param output_file The name of the output .csv file. If empty, will just return the data without creating a new file
#' @param anchors A list of potentially immovable areas inside the scrollable (e.g. anchored menus in a website) - see manual for a correct usage
#' @param rules A list of functions that can act as rules to activate/deactivate immovables areas in the scrollable (e.g. a menu that disappears after X pixels have been scrolled) - See manual for a correct
#' @param outside_image_is_na Indicates if values outside the AOI (e.g. the windows bar) should be set to NA or kept in the file/dataset. If set to False, coordinates above/before the AOI will become negative.
#' @param na.rm Indicates if lines with NA y values should be dropped in the final file/dataset
#' @param calibration A calibration list (see the scroll_calibration function)
#'
#' @return The Dataset with corrected x and y's
#' @examples
#' data(toydata)
#' output_table <- overview_tab(dat = toydata, id = ccode, time = year)
#' @export
eye_tracker_gaze_scroll <- function (file_name = "mouse events and fixation coordinates.csv", time_shift=388, timestamp_start=53753, image_height=6655, image_width=1920, timestamp_stop=113579, starting_scroll = 0, output_file = "eye_tracker_gaze_corrected.csv", anchors = list(array(c(c(0,0,1902,95), c(0,0,1902,95), c(1607,96,1902,905), c(1607,96,1902,905)), dim = c(4,2,2)), array(c(c(0,0,1902,63), c(0,32,1902,95), c(1607,63,1902,900), c(1607,63,1902,900)), dim = c(4,2,2)), array(c(c(1607,906,1920,1080), c(1607,6481,1920,6655)), dim = c(4,2,1))), rules = list(rule_before_scrolling, rule_after_scrolling, rule_true), outside_image_is_na = TRUE, na.rm=TRUE, calibration)
{
  #TODO: column names as parameters?
  scroll_pixels <- calibration$scroll_pixels
  screen_width <- calibration$screen_width
  screen_height <- calibration$screen_height
  shift_top <- calibration$shift_top
  shift_right <- calibration$shift_right
  shift_bottom <- calibration$shift_bottom
  shift_left <- calibration$shift_left
  event <- "WM_MOUSEWHEEL"
  eyes_data <- utils::read.csv(file = file_name, header = FALSE, colClasses = c("NULL", rep(NA, 14)), col.names = c("", "timestamps", "U1", "U2", "U3", "U4", "U5", "U6", "U7", "event_controller", "event", "U8", "U9", "x", "y"))
  eyes_data$timestamps <- eyes_data$timestamps - time_shift
  eyes_data <- dplyr::filter(eyes_data, timestamps > timestamp_start, timestamps < timestamp_stop)
  scroll <- starting_scroll
  corrected_y <- c()
  min_scroll <- 0
  rules <- check_anchors_rules(anchors, rules)
  flags <- c(rep(TRUE, length(rules)))
  max_scroll <- image_height - screen_height - shift_top - shift_bottom
  eyes_data$corrected_y <- vapply(eyes_data$y, shift_image_by_dimension, shift_before = shift_top, shift_after = shift_bottom, screen_dimension = screen_height, outside_image_is_na)
  eyes_data$corrected_x <- vapply(eyes_data$x, shift_image_by_dimension, shift_before = shift_left, shift_after = shift_right, screen_dimension = screen_width, outside_image_is_na)

  for (line in 1:dim(eyes_data)[1])
  {
    # prepare_smooth_scroll(event, eyes_data[line ,], smooth_scroll, smooth_scroll_table, scroll, min_scroll, max_scroll)
    scroll <- shift_scroll(event, eyes_data[line ,], scroll, min_scroll, max_scroll, scroll_pixels)
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
  if (na.rm)
  {
    eyes_data <- eyes_data[stats::complete.cases(eyes_data[, 'corrected_y']),]
  }
  if (output_file != "")
  {
    utils::write.csv(eyes_data, file = output_file, row.names = FALSE, na="")
  }
  return(eyes_data)
}

#TODO: working heatmap
generate_heatmap <- function(data, img)
{

  data$corrected_y <- dim(img)[1] + data$corrected_y
  ggplot2::ggplot(data, ggplot2::aes(corrected_x, corrected_y))  +
    ggplot2::annotation_raster(img, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
    ggplot2::stat_density2d(geom = "polygon", ggplot2::aes(fill=..level.., alpha = 0.2)) +
    ggplot2::geom_point(size=0.5)+
    ggplot2::scale_fill_gradient(low="green", high="red") +
    ggplot2::scale_x_continuous(limits=c(0,dim(img)[2]),expand=c(0,0))+
    ggplot2::scale_y_continuous(limits=c(0,dim(img)[1]),expand=c(0,0))+
    ggplot2::coord_fixed()
}

# Future features?
# prepare_smooth_scroll <- function (event, data_line, smooth_scroll, smooth_scroll_table, scroll, min_scroll, max_scroll, scroll_pixels)
# {
#   if (grepl(event, data_line$event, fixed=TRUE))
#   {
#     scroll_delta = strtoi(unlist(strsplit(unlist(strsplit(data_line$event, ";", fixed=TRUE))[5], ":", fixed = TRUE))[2])
#     if (max(smooth_scroll_table$timestamp) < data_line$timestamp)
#     {
#       if (scroll_delta<0)
#       {
#         scroll_delta <- min(c(scroll + scroll_pixels, max_scroll))
#         smooth_scroll_table <- data.frame(timestamp = data_line$timestamp:(data_line$timestamp+smooth_scroll-1), delta_scroll = scroll - seq(0,scroll_delta, length.out=smooth_scroll))
#       }
#       else if (scroll_delta>0)
#       {
#         scroll_delta <- max(c(scroll - scroll_pixels, min_scroll))
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
