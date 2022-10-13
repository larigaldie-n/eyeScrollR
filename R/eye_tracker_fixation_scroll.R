# Checks if the fixed_areas and rules lists are correctly formed
check_fixed_areas_rules <- function(fixed_areas, rules)
{
  if (!is.list(fixed_areas))
  {
    stop("fixed_areas must be an empty list or a list of arrays")
  }
  for (fixed_area in fixed_areas)
  {
    if (!is.array(fixed_area))
    {
      stop("fixed_areas must be an empty list or a list of arrays")
    }
    else if (dim(fixed_area)[1]!=4 || dim(fixed_area)[2]!=2)
    {
      stop("fixed_areas arrays must be 4x2 matrices of coordinates")
    }
    else
    {
      for (i in 1:dim(fixed_area)[3])
      {
        if ((fixed_area[3,1,i] - fixed_area[1,1,i] != fixed_area[3,2,i] - fixed_area[1,2,i]) || (fixed_area[4,1,i] - fixed_area[2,1,i] != fixed_area[4,2,i] - fixed_area[2,2,i]))
        {
          stop("origin (screen) and destination (image) fixed_areas rectangles must be the same size")
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
  if (length(rules) > length(fixed_areas))
  {
    stop("There can't be more rules than fixed_areas arrays")
  }
  while (length(rules) < length(fixed_areas))
  {
    rules <- append(rules, rule_true)
  }
  return(rules)
}

# Checks if each rule in the rules list is true or false
check_rules_true <- function(rules, data_line, flags, fixed_areas, scroll)
{
  for (rule_num in seq_len(length(rules)))
  {
    flags[rule_num] <- rules[[rule_num]](data_line, fixed_areas[[rule_num]], flags[rule_num], scroll)
  }
  return(flags)
}

# Checks if a calibration line has been found
check_calibration_line <- function(line, coordinate, reverse)
{
  red <- c(1,0,0)
  green <- c(0,1,0)
  blue <- c(0,0,1)
  color <- "red"
  if(reverse == FALSE)
  {
    if(coordinate>1)
    {
      if(all(line[coordinate-1,] == red))
      {
        return(FALSE)
      }
    }
    while(coordinate+1<dim(line)[1])
    {
      if(color == "red")
      {
        if(all(line[coordinate+1,] == red))
        {
          color <- "red"
        }
        else if (all(line[coordinate+1,] == green))
        {
          color <- "green"
        }
        else
        {
          return(FALSE)
        }
      }
      else if (color == "green")
      {
        if(all(line[coordinate+1,] == green))
        {
          color <- "green"
        }
        else if (all(line [coordinate+1,] == blue))
        {
          return(TRUE)
        }
        else
        {
          return(FALSE)
        }
      }
      coordinate <- coordinate + 1
    }
  }
  else
  {
    if(coordinate<dim(line)[1])
    {
      if(all(line[coordinate+1,] == red))
      {
        return(FALSE)
      }
    }
    while(coordinate-1>0)
    {
      if(color == "red")
      {
        if(all(line[coordinate-1,] == red))
        {
          color <- "red"
        }
        else if (all(line[coordinate-1,] == green))
        {
          color <- "green"
        }
        else
        {
          return(FALSE)
        }
      }
      else if (color =="green")
      {
        if(all(line[coordinate-1,] == green))
        {
          color <- "green"
        }
        else if (all(line [coordinate-1,] == blue))
        {
          return(TRUE)
        }
        else
        {
          return(FALSE)
        }
      }
      coordinate <- coordinate - 1
    }
  }
  return(FALSE)
}

# Checks which rules are true, and returns y coordinates of fixations if
# it is inside a fixed area if its association rule its true
enforce_rules <- function(flags, fixed_areas, data_line)
{
  y <- NA
  for (flag_num in seq_len(length(flags)))
  {
    if (flags[flag_num])
    {
      y <- resolve_fixed_box(fixed_areas[[flag_num]], data_line$'Corrected.X', data_line$'Corrected.Y')
      if (!is.na(y))
      {
        break
      }
    }
  }
  return(y)
}

# Check if a fixation point is inside a given fixed area
resolve_fixed_box <- function(array_fixed_areas, x, y)
{
  for (i in 1:dim(array_fixed_areas)[3])
  {
    if(x >= array_fixed_areas[1,1,i] && x <= array_fixed_areas[3,1,i])
    {
      if(y >= array_fixed_areas[2,1,i] && y <= array_fixed_areas[4,1,i])
      {
        return (y + (array_fixed_areas[4,2,i] - array_fixed_areas[4,1,i]))
      }
    }
  }
  return (NA)
}

# Shifts the original coordinate (on one dimension) to fit the image coordinate
# and returns NA to any coordinate outside the image according to outside_image_is_na
shift_image_by_dimension <- function(coordinate, shift_before, shift_after, screen_dimension, outside_image_is_na)
{
  if (is.na(coordinate) || (outside_image_is_na && (coordinate < shift_before || coordinate > (screen_dimension - shift_after))))
  {
    return(NA)
  }
  else
  {
    return(coordinate - shift_before)
  }
}

# Gets the Data message, and updates the scroll delta according to it
shift_scroll <- function(event, data_line, scroll, min_scroll, max_scroll, scroll_pixels, top_left_x, top_left_y, bottom_right_x, bottom_right_y)
{
  if (grepl(event, data_line$Data, fixed=TRUE))
  {
    event_list <- unlist(strsplit(data_line$Data, ";", fixed=TRUE))
    event_loc_x <- strtoi(unlist(strsplit(event_list[2], ":", fixed = TRUE))[2])
    event_loc_y <- strtoi(unlist(strsplit(event_list[3], ":", fixed = TRUE))[2])
    scroll_delta <- strtoi(unlist(strsplit(event_list[5], ":", fixed = TRUE))[2])
    if (event_loc_x >= top_left_x && event_loc_y >= top_left_y && event_loc_x <= bottom_right_x && event_loc_y <= bottom_right_y)
    {
      if (scroll_delta<0)
      {
        scroll <- min(c(scroll + scroll_pixels, max_scroll))
      }
      else if (scroll_delta>0)
      {
        scroll <- max(c(scroll - scroll_pixels, min_scroll))
      }
    }
  }
  return (scroll)
}

#' @title rule_true
#'
#' @description Helper function to enforce fixed area rules (this one is always
#'     true, meaning that this rule should always be enforced). NB: you should
#'     never have to call this function yourself, but you can use it as a basis
#'     to create your own rule. All arguments are automatically passed to any
#'     rule function by eye_tracker_fixation_scroll
#'
#' @param data_line The current line in the .csv file
#' @param array_fixed_areas The array of fixed areas linked to this rule
#' @param flag A boolean that says if this rule is TRUE or FALSE at the moment
#' @param scroll The total amount of pixels that have been scrolled down from
#'     the top of the website at the moment
#'
#' @return A boolean saying if this rule should now be enforced or not
#' @examples
#' rules <- list(rule_before_scrolling, rule_after_scrolling, rule_true)
#' @export
rule_true <- function (data_line, array_fixed_areas, flag, scroll)
{
  return (TRUE)
}

#' @title rule_before_scrolling
#'
#' @description Helper function to enforce fixed area rules (this one checks
#'     if the total amount of pixels that have been scrolled down from the top
#'     of the website is inferior to 30). NB: you should never
#'     have to call this function yourself, but you can use it as a basis to
#'     create your own rule. All arguments are automatically passed to any
#'     rule function by eye_tracker_fixation_scroll
#'
#' @param data_line The current line in the .csv file
#' @param array_fixed_areas The array of fixed areas linked to this rule
#' @param flag A boolean that says if this rule is TRUE or FALSE at the moment
#' @param scroll The total amount of pixels that have been scrolled down from
#'     the top of the website at the moment
#'
#' @return A boolean saying if this rule should now be enforced or not
#' @examples
#' rules <- list(rule_before_scrolling, rule_after_scrolling, rule_true)
#' @export
rule_before_scrolling <- function (data_line, array_fixed_areas, flag, scroll)
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

#' @title rule_after_scrolling
#'
#' @description Helper function to enforce fixed area rules (this one checks
#'     if the total amount of pixels that have been scrolled down from the top
#'     of the website is superior to 30). NB: you should never
#'     have to call this function yourself, but you can use it as a basis to
#'     create your own rule. All arguments are automatically passed to any
#'     rule function by eye_tracker_fixation_scroll
#'
#' @param data_line The current line in the .csv file
#' @param array_fixed_areas The array of fixed areas linked to this rule
#' @param flag A boolean that says if this rule is TRUE or FALSE at the moment
#' @param scroll The total amount of pixels that have been scrolled down from
#'     the top of the website at the moment
#'
#' @return A boolean saying if this rule should now be enforced or not
#' @examples
#' rules <- list(rule_before_scrolling, rule_after_scrolling, rule_true)
#' @export
rule_after_scrolling <- function (data_line, array_fixed_areas, flag, scroll)
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

#' @title scroll_calibration_manual
#'
#' @description Creates a calibration list for use in the eye_tracker_fixation_scroll
#'     function. Uses values taken by hand
#'
#' @param screen_width Resolution width of the screen on which the experiment is conducted (in pixels). E.g. 1920
#' @param screen_height Resolution height of the screen on which the experiment is conducted (in pixels). E.g. 1080
#' @param top_left_x x coordinate of the top left pixel of the browser viewing area
#' @param top_left_y y coordinate of the same pixel
#' @param bottom_right_x x coordinate of the bottom right pixel of the browser viewing area
#' @param bottom_right_y y coordinate of the same pixel
#' @param scroll_pixels The amount of pixels being scrolled by each mouse scroll
#'     (depends on the browser). Output by the calibration webpage
#'
#' @return A list representing the calibration to the screen & browser used for the experiment
#' @examples
#' calibration <- scroll_calibration_manual(1920, 1080, 88, 0, 40, 0, 100)
#' @export
scroll_calibration_manual <- function(screen_width, screen_height, top_left_x, top_left_y, bottom_right_x, bottom_right_y, scroll_pixels)
{
  shift_right <- screen_width - bottom_right_x - 1
  shift_bottom <- screen_height - bottom_right_y - 1
  l <- list(screen_width = screen_width, screen_height = screen_height, top_left_x = top_left_x, top_left_y = top_left_y, bottom_right_x = bottom_right_x, bottom_right_y = bottom_right_y, shift_right = shift_right, shift_bottom = shift_bottom, scroll_pixels = scroll_pixels)
  return(l)
}

#' @title scroll_calibration_auto
#'
#' @description Creates a calibration list for use in the eye_tracker_fixation_scroll
#'     function. Uses a screenshot of a full screen including the calibration webpage
#'
#' @param calibration_image The screenshot, loaded as a large array (e.g. using readPNG)
#' @param scroll_pixels The amount of pixels being scrolled by each mouse scroll
#'     (depends on the browser). Output by the calibration webpage
#'
#' @return A list representing the calibration to the screen & browser used for the experiment
#' @examples
#' \dontrun{
#' library(png)
#' img <- readPNG("calibrate.png")
#' calibration <- scroll_calibration_auto(calibration_image = img, scroll_pixels = 100)
#' }
#' @export
scroll_calibration_auto <- function(calibration_image, scroll_pixels)
{
  red <- c(1,0,0)
  green <- c(0,1,0)
  blue <- c(0,0,1)
  screen_width <- dim(calibration_image)[2]
  screen_height <- dim(calibration_image)[1]
  calibration_image <- calibration_image[,,-c(4)]

  for (height in 1:dim(calibration_image)[1])
  {
    for (width in 1:dim(calibration_image)[2])
    {
      if (all(calibration_image[height,width,] == red))
      {
        if (check_calibration_line(calibration_image[height,,], width, FALSE) && check_calibration_line(calibration_image[,width,], height, FALSE))
        {
          top_left_x <- width - 1
          top_left_y <- height - 1
        }
        else if(check_calibration_line(calibration_image[height,,], width, TRUE) && check_calibration_line(calibration_image[,width,], height, TRUE))
        {
          bottom_right_x <- width - 1
          bottom_right_y <- height - 1
        }
      }
    }
  }
  if (!exists("top_left_x") || !exists("bottom_right_x"))
  {
    stop("Could not calibrate. Either the image does not include the calibration
          webpage, or your should calibrate by hand using the
          scroll_calibration_manual function.")
  }
  shift_right <- screen_width - bottom_right_x - 1
  shift_bottom <- screen_height - bottom_right_y - 1
  cat(paste("Screen width: ", screen_width, ", Screen height: ", screen_height,
        ",\nTop left x: ", top_left_x, ", Top left y: ", top_left_y,
        ",\nBottom right x: ", bottom_right_x, ", Bottom right y: ", bottom_right_y,
        "\n", sep=""))
  l <- list(screen_width = screen_width, screen_height = screen_height, top_left_x = top_left_x, top_left_y = top_left_y, bottom_right_x = bottom_right_x, bottom_right_y = bottom_right_y, shift_right = shift_right, shift_bottom = shift_bottom, scroll_pixels = scroll_pixels)
  return(l)
}

#' @title eye_tracker_fixation_scroll
#'
#' @description corrects the eye-tracking coordinate data to fit in a webpage
#'     scrolled vertically by the participant
#'
#' @param eyes_data The dataset
#' @param timestamp_start The starting timestamp from the .csv file at which the participant was watching the scrollable (AFTER time_shift applied)
#' @param timestamp_stop The final timestamp from the .csv file at which the participant was watching the scrollable (AFTER time_shift applied)
#' @param image_height The total height of the image (in pixels)
#' @param image_width The total width of the image (in pixels)
#' @param calibration A calibration list (see the scroll_calibration function)
#' @param time_shift [Optional] A time shift parameter to synchronize the .csv file with a video recording (e.g. if the recording started after the start of the .csv). Default: 0
#' @param fixed_areas [Optional] A list of potentially immovable areas inside the scrollable (e.g. fixed menus in a website) - see manual for a correct usage. Default: empty list
#' @param rules [Optional] A list of functions that can act as rules to activate/deactivate immovables areas in the scrollable (e.g. a menu that disappears after X pixels have been scrolled) - See manual for a correct. Default: empty list
#' @param starting_scroll [Optional] If the participant did not start watching the webpage from the top, you can indicate the y coordinate at which he started here (in pixels). Default: 0
#' @param output_file [Optional] The name of the output .csv file. If an empty string, will just return the data without creating a new file. Default: empty string
#' @param outside_image_is_na [Optional] Indicates if values outside the AOI (e.g. the windows bar) should be set to NA or kept in the file/dataset. If set to FALSE, coordinates above/before the AOI will become negative. Default: TRUE
#' @param na.rm [Optional] Indicates if lines with NA y values should be dropped in the final file/dataset. Default: TRUE
#'
#' @return The Dataset with corrected x and y's
#' @examples
#' \dontrun{
#' test_fixed_areas <- list(array(c(c(0,0,1902,95), c(0,0,1902,95),
#'     c(1607,96,1902,905), c(1607,96,1902,905)), dim = c(4,2,2)),
#'     array(c(c(0,0,1902,63), c(0,32,1902,95), c(1607,63,1902,900),
#'     c(1607,63,1902,900)), dim = c(4,2,2)), array(c(c(1607,906,1920,1080),
#'     c(1607,6481,1920,6655)), dim = c(4,2,1)))
#' test_rules = list(rule_before_scrolling, rule_after_scrolling, rule_true)
#' test_calibration <- scroll_calibration_manual(1920, 1080, 88, 0, 40, 0, 100)
#' test_data <- eye_tracker_fixation_scroll(fixed_areas = test_fixed_areas,
#'     rules = test_rules, calibration = test_calibration)
#' }
#' @export
#' @importFrom rlang .data
eye_tracker_fixation_scroll <- function (eyes_data, timestamp_start, timestamp_stop, image_height, image_width, calibration, time_shift=0, starting_scroll = 0, output_file = "", fixed_areas = list(), rules = list(), outside_image_is_na = TRUE, na.rm=TRUE)
{
  scroll_pixels <- calibration$scroll_pixels
  screen_width <- calibration$screen_width
  screen_height <- calibration$screen_height
  top_left_y <- calibration$top_left_y
  bottom_right_x <- calibration$bottom_right_x
  bottom_right_y <- calibration$bottom_right_y
  top_left_x <- calibration$top_left_x
  shift_bottom <- calibration$shift_bottom
  shift_right <- calibration$shift_right
  event <- "WM_MOUSEWHEEL"
  eyes_data$Timestamp.shifted <- eyes_data$Timestamp - time_shift
  eyes_data <- dplyr::filter(eyes_data, .data$Timestamp.shifted > timestamp_start, .data$Timestamp.shifted < timestamp_stop)
  scroll <- starting_scroll
  corrected_y <- c()
  min_scroll <- 0
  rules <- check_fixed_areas_rules(fixed_areas, rules)
  flags <- c(rep(TRUE, length(rules)))
  max_scroll <- image_height - (screen_height - top_left_y - shift_bottom)
  eyes_data$'Corrected.Y' <- vapply(eyes_data$'Fixation Y', shift_image_by_dimension, shift_before = top_left_y, shift_after = shift_bottom, screen_dimension = screen_height, outside_image_is_na = outside_image_is_na, FUN.VALUE = 1.0)
  eyes_data$'Corrected.X' <- vapply(eyes_data$'Fixation X', shift_image_by_dimension, shift_before = top_left_x, shift_after = shift_right, screen_dimension = screen_width, outside_image_is_na = outside_image_is_na, FUN.VALUE = 1.0)

  for (line in 1:dim(eyes_data)[1])
  {
    # prepare_smooth_scroll(event, eyes_data[line ,], smooth_scroll, smooth_scroll_table, scroll, min_scroll, max_scroll)
    scroll <- shift_scroll(event, eyes_data[line ,], scroll, min_scroll, max_scroll, scroll_pixels, top_left_x, top_left_y, bottom_right_x, bottom_right_y)
    flags <- check_rules_true(rules, eyes_data[line, ], flags, fixed_areas, scroll)
    if (!is.na(eyes_data[line, ]$'Corrected.Y'))
    {
      corrected_y[line] <- enforce_rules(flags, fixed_areas, eyes_data[line, ])
      if (is.na(corrected_y[line]))
      {
        corrected_y[line] <- eyes_data[line, ]$'Corrected.Y'+scroll
      }
    }
    else
    {
      corrected_y[line] <- NA
    }
  }

  eyes_data$'Corrected.Y' <- corrected_y
  if (na.rm)
  {
    eyes_data <- eyes_data[stats::complete.cases(eyes_data[, 'Corrected.Y']),]
  }
  if (output_file != "")
  {
    utils::write.csv(eyes_data, file = output_file, row.names = FALSE, na="")
  }
  return(eyes_data)
}

#' @title generate_heatmap
#'
#' @description Creates a heatmap based on the dataset
#'
#' @param data The dataset output by the eye_tracker_fixation_scroll
#' @param heatmap_image An image on which to apply the heatmap
#'
#' @return A plot with the image and the heatmap
#' @examples
#' \dontrun{
#' img <- readPNG("test.png")
#' test_data <- eye_tracker_fixation_scroll(fixed_areas = test_fixed_areas,
#'     rules = test_rules, calibration = test_calibration)
#' generate_heatmap(data = test_data, heatmap_image = img)
#' }
#' @export
#' @importFrom rlang .data
generate_heatmap <- function(data, heatmap_image)
{

  data$'Corrected.Y' <- dim(heatmap_image)[1] - data$'Corrected.Y'
  ggplot2::ggplot(data, ggplot2::aes(.data$'Corrected.X', .data$'Corrected.Y'))  +
    ggplot2::annotation_raster(heatmap_image, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
    ggplot2::stat_density2d(geom = "polygon", ggplot2::aes(fill=.data$..level.., alpha = 0.15)) +
    ggplot2::geom_point(size=1)+
    ggplot2::scale_fill_gradient(low="green", high="red") +
    ggplot2::scale_x_continuous(limits=c(0,dim(heatmap_image)[2]),expand=c(0,0))+
    ggplot2::scale_y_continuous(limits=c(0,dim(heatmap_image)[1]),expand=c(0,0))+
    ggplot2::coord_fixed()
}

# Future features?
# prepare_smooth_scroll <- function (event, data_line, smooth_scroll, smooth_scroll_table, scroll, min_scroll, max_scroll, scroll_pixels)
# {
#   if (grepl(event, data_line$Data, fixed=TRUE))
#   {
#     scroll_delta = strtoi(unlist(strsplit(unlist(strsplit(data_line$Data, ";", fixed=TRUE))[5], ":", fixed = TRUE))[2])
#     if (max(smooth_scroll_table$Timestamp) < data_line$Timestamp)
#     {
#       if (scroll_delta<0)
#       {
#         scroll_delta <- min(c(scroll + scroll_pixels, max_scroll))
#         smooth_scroll_table <- data.frame(timestamp = data_line$Timestamp:(data_line$Timestamp+smooth_scroll-1), delta_scroll = scroll - seq(0,scroll_delta, length.out=smooth_scroll))
#       }
#       else if (scroll_delta>0)
#       {
#         scroll_delta <- max(c(scroll - scroll_pixels, min_scroll))
#         smooth_scroll_table <- data.frame(timestamp = data_line$Timestamp:(data_line$Timestamp+smooth_scroll-1), delta_scroll = scroll - seq(0,scroll_delta, length.out=smooth_scroll))
#       }
#     }
#     else
#     {
#       smooth_scroll_table <- dplyr::filter(smooth_scroll_table, timestamp>=data_line$Timestamp)
#       if (scroll_delta<0)
#       {
#         scroll_delta <- min(c(scroll - scroll_delta, max_scroll))
#         smooth_scroll_table <- data.frame(timestamp = data_line$Timestamp:(data_line$Timestamp+smooth_scroll-1), delta_scroll = scroll - seq(0,scroll_delta, length.out=smooth_scroll))
#       }
#       else if (scroll_delta>0)
#       {
#         scroll_delta <- max(c(scroll - scroll_delta, min_scroll))
#         smooth_scroll_table <- data.frame(timestamp = data_line$Timestamp:(data_line$Timestamp+smooth_scroll-1), delta_scroll = scroll - seq(0,scroll_delta, length.out=smooth_scroll))
#       }
#     }
#   }
#   return (smooth_scroll_table)
# }
