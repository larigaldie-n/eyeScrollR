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
    else if (dim(fixed_area)[1] != 4 || dim(fixed_area)[2] != 2)
    {
      stop("fixed_areas arrays must be 4x2 matrices of coordinates")
    }
    else
    {
      for (i in 1:dim(fixed_area)[3])
      {
        if ((fixed_area[3, 1, i] - fixed_area[1, 1, i] != fixed_area[3, 2, i] - fixed_area[1, 2, i]) ||
            (fixed_area[4, 1, i] - fixed_area[2, 1, i] != fixed_area[4, 2, i] - fixed_area[2, 2, i]))
        {
          stop(
            "origin (screen) and destination (image) fixed_areas rectangles must be the same size"
          )
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
    if (typeof(rule) != "closure")
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
check_rules_true <-
  function(rules,
           data_line,
           flags,
           fixed_areas,
           scroll)
  {
    for (rule_num in seq_len(length(rules)))
    {
      flags[rule_num] <-
        rules[[rule_num]](data_line, fixed_areas[[rule_num]], flags[rule_num], scroll)
    }
    return(flags)
  }

# Checks if a calibration line has been found
check_calibration_line <- function(line, coordinate, reverse)
{
  red <- c(1, 0, 0)
  green <- c(0, 1, 0)
  blue <- c(0, 0, 1)
  color <- "red"
  if (reverse == FALSE)
  {
    if (coordinate > 1)
    {
      if (all(line[coordinate - 1,] == red))
      {
        return(FALSE)
      }
    }
    while (coordinate + 1 < dim(line)[1])
    {
      if (color == "red")
      {
        if (all(line[coordinate + 1,] == red))
        {
          color <- "red"
        }
        else if (all(line[coordinate + 1,] == green))
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
        if (all(line[coordinate + 1,] == green))
        {
          color <- "green"
        }
        else if (all(line [coordinate + 1,] == blue))
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
    if (coordinate < dim(line)[1])
    {
      if (all(line[coordinate + 1,] == red))
      {
        return(FALSE)
      }
    }
    while (coordinate - 1 > 0)
    {
      if (color == "red")
      {
        if (all(line[coordinate - 1,] == red))
        {
          color <- "red"
        }
        else if (all(line[coordinate - 1,] == green))
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
        if (all(line[coordinate - 1,] == green))
        {
          color <- "green"
        }
        else if (all(line [coordinate - 1,] == blue))
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
enforce_rules <-
  function(flags,
           fixed_areas,
           data_line,
           columns_to_correct)
  {
    y <- NA
    for (flag_num in seq_len(length(flags)))
    {
      if (flags[flag_num])
      {
        y <-
          resolve_fixed_box(fixed_areas[[flag_num]], data_line[[columns_to_correct[1]]], data_line[[columns_to_correct[2]]])
        if (!is.na(y))
        {
          break
        }
      }
    }
    return(y)
  }

# Check if a fixation point is inside a given fixed area
resolve_fixed_box <- function(fixed_areas_bundle, x, y)
{
  if (!is.na(x) && !is.na(y))
  {
    for (i in 1:dim(fixed_areas_bundle)[3])
    {
      if (x >= fixed_areas_bundle[1, 1, i] &&
          x <= fixed_areas_bundle[3, 1, i])
      {
        if (y >= fixed_areas_bundle[2, 1, i] &&
            y <= fixed_areas_bundle[4, 1, i])
        {
          return (y + (fixed_areas_bundle[4, 2, i] - fixed_areas_bundle[4, 1, i]))
        }
      }
    }
  }
  return (NA)
}

# Shifts the original coordinate (on one dimension) to fit the image coordinate
# and returns NA to any coordinate outside the image according to outside_image_is_na
shift_image_by_dimension <-
  function(coordinate,
           shift_before,
           shift_after,
           screen_dimension,
           outside_image_is_na)
  {
    if (is.na(coordinate) ||
        (outside_image_is_na &&
         (
           coordinate < shift_before ||
           coordinate > (screen_dimension - shift_after)
         )))
    {
      return(NA)
    }
    else
    {
      return(coordinate - shift_before)
    }
  }

# Gets the Data message, and updates the scroll delta according to it
shift_scroll <-
  function(event,
           data_line,
           scroll,
           min_scroll,
           max_scroll,
           scroll_pixels,
           top_left_x,
           top_left_y,
           bottom_right_x,
           bottom_right_y)
  {
    if (grepl(event, data_line$Data, fixed = TRUE))
    {
      event_list <- unlist(strsplit(data_line$Data, ";", fixed = TRUE))
      for (i in seq_len(length(event_list)))
      {
        if (grepl("X:", event_list[i], fixed = TRUE))
        {
          event_loc_x <-
            strtoi(unlist(strsplit(event_list[i], ":", fixed = TRUE))[2])
        }
        else if (grepl("Y:", event_list[i], fixed = TRUE))
        {
          event_loc_y <-
            strtoi(unlist(strsplit(event_list[i], ":", fixed = TRUE))[2])
        }
        else if (grepl("ScrollDelta:", event_list[i], fixed = TRUE))
        {
          scroll_delta <-
            strtoi(unlist(strsplit(event_list[i], ":", fixed = TRUE))[2])
        }
      }

      if (event_loc_x >= top_left_x &&
          event_loc_y >= top_left_y &&
          event_loc_x <= bottom_right_x &&
          event_loc_y <= bottom_right_y)
      {
        if (scroll_delta < 0)
        {
          scroll <- min(c(scroll + scroll_pixels, max_scroll))
        }
        else if (scroll_delta > 0)
        {
          scroll <- max(c(scroll - scroll_pixels, min_scroll))
        }
      }
    }
    return (scroll)
  }

#' @title Creates a bundle of fixed areas mappings
#'
#' @description Puts together several fixed areas mappings to make them all
#' (de)activated by the same rule
#'
#' @param ... A varying number of pairs of coordinate vectors. The first of these
#' pairs should always represent an area on the screen, and the second one an
#' area on the unscrolled image. Each vector should take the form c(top_left_x,
#' top_left_y, bottom_right_x, bottom_right_y)
#'
#' @return A bundle of fixed areas mappings, to be put in a list
#' @examples
#' \dontrun{
#' top_fixed_area_screen <- c(0, 89, 1919, 276)
#' top_fixed_area_image <- c(0, 0, 1919, 187)
#' right_fixed_area_screen <- c(1632, 277, 1919, 936)
#' right_fixed_area_image <- c(1632, 188, 1919, 847)
#' area_bundle <- fixed_areas_bundle(top_fixed_area_screen,
#'                                   top_fixed_area_image,
#'                                   right_fixed_area_screen,
#'                                   right_fixed_area_image)
#' fixed_areas <- list(area_bundle)
#' }
#' @export
fixed_areas_bundle <- function(...)
{
  data <- list(...)
  if (length(data) == 0 || length(data) %% 2 != 0)
  {
    stop("You must pass screen-to-image mapping pairs of coordinate vectors to this function")
  }
  for (i in seq_len(length(data)))
  {
    if (!is.vector(data[[i]]) || length(data[[i]]) != 4)
    {
      stop(
        "Every argument should be a coordinate vector in the form: c(top_left_x, top_left_y, bottom_right_x, bottom_right_y)"
      )
    }
  }
  return(array(unlist(data), dim = c(4, 2, (length(
    data
  ) / 2))))
}

#' @title A rule that is always true
#'
#' @description Helper function to enforce fixed area rules (this one is always
#'     true, meaning that this rule should always be enforced). This rule is the
#'     default one for any fixed area bundle which has no rule attached to it.
#'     All arguments are automatically passed to any rule function by
#'     eye_scroll_correct
#'
#' @param data_line The current line in the .csv file. Includes all the original
#' columns, along with a new "Timestamp.Shifted" column (Timestamp - time_shit),
#' along with "Corrected.X" and "Corrected.Y" columns, which are "Fixation.X"
#' and "Fixation.Y" respectively shifted by top_left_x and top_left_y of the
#' calibration stage
#' @param fixed_areas_bundle The bundle of fixed areas linked to this rule
#' @param flag A boolean that says if this rule is TRUE or FALSE at the moment
#' @param scroll The total amount of pixels that have been scrolled down from
#'     the top of the website at the moment
#'
#' @return A boolean saying if this rule should now be enforced or not
#' @examples
#' rule_true <- function (data_line, fixed_areas_bundle, flag, scroll)
#' {
#'   return (TRUE)
#' }
#' rules <- list(rule_before_scrolling, rule_after_scrolling, rule_true)
#' @export
rule_true <- function (data_line,
                       fixed_areas_bundle,
                       flag,
                       scroll)
{
  return (TRUE)
}

#' @title Generic rule to activate a bundle before a certain amount of scrolled
#' pixels
#'
#' @description Helper function to enforce fixed area rules (this one checks
#'     if the total amount of pixels that have been scrolled down from the top
#'     of the website is inferior to 30). NB: you should probably not
#'     have to call this function yourself, but you can use it as a basis to
#'     create your own rule. All arguments are automatically passed to any
#'     rule function by eye_scroll_correct
#'
#' @param data_line The current line in the .csv file. Includes all the original
#' columns, along with a new "Timestamp.Shifted" column (Timestamp - time_shit),
#' along with "Corrected.X" and "Corrected.Y" columns, which are "Fixation.X"
#' and "Fixation.Y" respectively shifted by top_left_x and top_left_y of the
#' calibration stage
#' @param fixed_areas_bundle The array of fixed areas linked to this rule
#' @param flag A boolean that says if this rule is TRUE or FALSE at the moment
#' @param scroll The total amount of pixels that have been scrolled down from
#'     the top of the website at the moment
#'
#' @return A boolean saying if this rule should now be enforced or not
#' @examples
#' rule_before_scrolling <- function (data_line, fixed_areas_bundle, flag, scroll)
#' {
#'   if (scroll < 30)
#'   {
#'     return (TRUE)
#'   }
#'   else
#'   {
#'     return(FALSE)
#'   }
#' }
#' rules <- list(rule_before_scrolling, rule_after_scrolling, rule_true)
#' @export
rule_before_scrolling <-
  function (data_line,
            fixed_areas_bundle,
            flag,
            scroll)
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

#' @title Generic rule to activate a bundle after a certain amount of scrolled
#' pixels
#'
#' @description Helper function to enforce fixed area rules (this one checks
#'     if the total amount of pixels that have been scrolled down from the top
#'     of the website is superior to 30). NB: you should probably not
#'     have to call this function yourself, but you can use it as a basis to
#'     create your own rule. All arguments are automatically passed to any
#'     rule function by eye_scroll_correct
#'
#' @param data_line The current line in the .csv file. Includes all the original
#' columns, along with a new "Timestamp.Shifted" column (Timestamp - time_shit),
#' along with "Corrected.X" and "Corrected.Y" columns, which are "Fixation.X"
#' and "Fixation.Y" respectively shifted by top_left_x and top_left_y of the
#' calibration stage
#' @param fixed_areas_bundle The array of fixed areas linked to this rule
#' @param flag A boolean that says if this rule is TRUE or FALSE at the moment
#' @param scroll The total amount of pixels that have been scrolled down from
#'     the top of the website at the moment
#'
#' @return A boolean saying if this rule should now be enforced or not
#' @examples
#' rule_after_scrolling <- function (data_line, fixed_areas_bundle, flag, scroll)
#' {
#'   if (scroll >= 30)
#'   {
#'     return (TRUE)
#'   }
#'   else
#'   {
#'     return(FALSE)
#'   }
#' }
#' rules <- list(rule_before_scrolling, rule_after_scrolling, rule_true)
#' @export
rule_after_scrolling <-
  function (data_line,
            fixed_areas_bundle,
            flag,
            scroll)
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

#' @title Manual calibration
#'
#' @description Creates a calibration list for use in the eye_scroll_correct
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
scroll_calibration_manual <-
  function(screen_width,
           screen_height,
           top_left_x,
           top_left_y,
           bottom_right_x,
           bottom_right_y,
           scroll_pixels)
  {
    shift_right <- screen_width - bottom_right_x - 1
    shift_bottom <- screen_height - bottom_right_y - 1
    l <-
      list(
        screen_width = screen_width,
        screen_height = screen_height,
        top_left_x = top_left_x,
        top_left_y = top_left_y,
        bottom_right_x = bottom_right_x,
        bottom_right_y = bottom_right_y,
        shift_right = shift_right,
        shift_bottom = shift_bottom,
        scroll_pixels = scroll_pixels
      )
    return(l)
  }

#' @title Automatic calibration
#'
#' @description Creates a calibration list for use in the eye_scroll_correct
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
scroll_calibration_auto <-
  function(calibration_image, scroll_pixels)
  {
    red <- c(1, 0, 0)
    green <- c(0, 1, 0)
    blue <- c(0, 0, 1)
    screen_width <- dim(calibration_image)[2]
    screen_height <- dim(calibration_image)[1]
    calibration_image <- calibration_image[, ,-c(4)]

    for (height in 1:dim(calibration_image)[1])
    {
      for (width in 1:dim(calibration_image)[2])
      {
        if (all(calibration_image[height, width,] == red))
        {
          if (check_calibration_line(calibration_image[height, ,], width, FALSE) &&
              check_calibration_line(calibration_image[, width,], height, FALSE))
          {
            top_left_x <- width - 1
            top_left_y <- height - 1
          }
          else if (check_calibration_line(calibration_image[height, ,], width, TRUE) &&
                   check_calibration_line(calibration_image[, width,], height, TRUE))
          {
            bottom_right_x <- width - 1
            bottom_right_y <- height - 1
          }
        }
      }
    }
    if (!exists("top_left_x") || !exists("bottom_right_x"))
    {
      stop(
        "Could not calibrate. Either the image does not include the calibration
          webpage, or your should calibrate by hand using the
          scroll_calibration_manual function."
      )
    }
    shift_right <- screen_width - bottom_right_x - 1
    shift_bottom <- screen_height - bottom_right_y - 1
    cat(
      paste(
        "Screen width: ",
        screen_width,
        ", Screen height: ",
        screen_height,
        ",\nTop left x: ",
        top_left_x,
        ", Top left y: ",
        top_left_y,
        ",\nBottom right x: ",
        bottom_right_x,
        ", Bottom right y: ",
        bottom_right_y,
        "\n",
        sep = ""
      )
    )
    l <-
      list(
        screen_width = screen_width,
        screen_height = screen_height,
        top_left_x = top_left_x,
        top_left_y = top_left_y,
        bottom_right_x = bottom_right_x,
        bottom_right_y = bottom_right_y,
        shift_right = shift_right,
        shift_bottom = shift_bottom,
        scroll_pixels = scroll_pixels
      )
    return(l)
  }

#' @title Corrects eye-tracking coordinate data
#'
#' @description The core function that corrects the eye-tracking coordinate data
#' to fit in a webpage scrolled vertically by the participant
#'
#' @param eyes_data The dataset. Please note that it MUST include correctly named columns (see below)
#' @param timestamp_start The starting timestamp from the dataset at which the participant was watching the webpage (INCLUDING the time_shift, if applicable)
#' @param timestamp_stop The final timestamp from the dataset at which the participant was watching the webpage (INCLUDING the time_shift, if applicable)
#' @param image_width The total width of the webpage image (in pixels)
#' @param image_height The total height of the webpage image (in pixels)
#' @param calibration A calibration list (see the \code{\link{scroll_calibration_auto}} or the \code{\link{scroll_calibration_manual}} functions)
#' @param time_shift [Optional] A time shift parameter to synchronize the dataset with another source (e.g. if a screen recording started after timestamp 0 of the .csv). Default: 0
#' @param starting_scroll [Optional] If the participant did not start watching the webpage from the top, you can indicate the y coordinate at which he started here (in pixels). Default: 0
#' @param output_file [Optional] The name of the output .csv file. If an empty string, will just return the data without creating a file. Default: empty string
#' @param fixed_areas [Optional] A list of potentially immovable areas inside the webpage (e.g. fixed menus in a website) - see the Fixed Areas article for more information. Default: empty list
#' @param rules [Optional] A list of functions that can act as rules to activate/deactivate immovables areas in the webpage (e.g. a menu that disappears after X pixels have been scrolled) - See the Fixed Areas article for more information. Default: empty list
#' @param scroll_lag [Optional] Determines the lag (in milliseconds) between the scroll message and the scroll correction. Default: 0 (WARNING: NOT IMPLENTED YET)
#' @param outside_image_is_na [Optional] Indicates if values outside the AOI (e.g. the windows bar) should be set to NA or kept in the file/dataset. If set to FALSE, coordinates above/before the AOI will become negative. Default: TRUE
#' @param progression_bar [Optional] Indicates if the function should display a progression bar. Default: TRUE
#'
#' @details
#' \strong{The Dataset} \cr\cr
#' The dataset must include correctly named columns, which are:
#' \itemize{
#' \item A "Data" column, with user-generated events (such as keystrokes or browser changes)
#' \item A "Timestamp" column with the timestamps for each fixation point and event
#' \item A "Gaze.X" column with the x coordinate of gaze
#' \item A "Gaze.Y" column with the y coordinate of gaze
#' \item A "Fixation.X" column with the x coordinate of fixation points
#' \item A "Fixation.Y" column with the y coordinate of fixation points
#' }
#' Data and Timestamp columns are mandatory, but you may choose to only include Gaze data or Fixation data (or both)
#'
#' @return Returns the same dataset with added columns for corrected coordinates ("Corrected.Gaze.X", "Corrected.Gaze.Y", "Corrected.Fixation.X" and "Corrected.Fixation.Y"), shifted timestamps ("Timestamp.Shifted"), and the amount of pixels scrolled down ("Scrolled")
#' @examples
#' \dontrun{
#' library(eyeScrollR)
#' calibration <- scroll_calibration_auto(calibration_image = calibration_image,
#'                                        scroll_pixels = 100)
#' data <- eye_scroll_correct(eyes_data = dataset, timestamp_start = 2000,
#'                                     timstamp_stop = 50000, image_width = 1920,
#'                                     image_height = 10000, calibration = calibration)
#' generate_heatmap(data = data, heatmap_image = heatmap_image)
#' }
#'

#' @export
#' @importFrom rlang .data
eye_scroll_correct <-
  function (eyes_data,
            timestamp_start,
            timestamp_stop,
            image_width,
            image_height,
            calibration,
            time_shift = 0,
            starting_scroll = 0,
            output_file = "",
            fixed_areas = list(),
            rules = list(),
            scroll_lag = (1/120)*1000,
            outside_image_is_na = TRUE,
            progression_bar = TRUE)
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
    eyes_data$Timestamp.Shifted <- eyes_data$Timestamp - time_shift
    eyes_data <-
      dplyr::filter(
        eyes_data,
        .data$Timestamp.Shifted >= timestamp_start,
        .data$Timestamp.Shifted <= timestamp_stop
      )
    scroll <- starting_scroll
    scroll_vector <- c()
    future_scrolls <- c()
    future_scrolls_timestamps <- c()
    min_scroll <- 0
    columns_to_correct <- c()
    corrected_columns <- dplyr::tibble(.rows = dim(eyes_data)[1])
    if ("Gaze.X" %in% names(eyes_data) &&
        "Gaze.Y" %in% names(eyes_data))
    {
      columns_to_correct <- append(columns_to_correct, "Gaze.X")
      columns_to_correct <- append(columns_to_correct, "Gaze.Y")
    }
    if ("Fixation.X" %in% names(eyes_data) &&
        "Fixation.Y" %in% names(eyes_data))
    {
      columns_to_correct <- append(columns_to_correct, "Fixation.X")
      columns_to_correct <- append(columns_to_correct, "Fixation.Y")
    }
    if (length(columns_to_correct) == 0)
    {
      stop(
        "Please input a dataset with either Gaze, Fixation or both data, with column names Gaze.X, Gaze.Y, Fixation.X and Fixation.Y"
      )
    }
    if (progression_bar == TRUE)
    {
      pb <- utils::txtProgressBar(min = 0, max = nrow(eyes_data), style = 3, width = 50, char = "=")
    }

    rules <- check_fixed_areas_rules(fixed_areas, rules)
    flags <- c(rep(TRUE, length(rules)))
    for (i in seq_len(length(fixed_areas)))
    {
      fixed_areas[[i]] <-
        fixed_areas[[i]] - c(top_left_x, top_left_y, top_left_x, top_left_y, 0, 0, 0, 0)
    }
    max_scroll <-
      image_height - (bottom_right_y - top_left_y + 2) # +2 because bottom_right_y and top_left_y are coordinates

    for (i in seq(1, length(columns_to_correct), 2))
    {
      corrected_columns[columns_to_correct[i]] <-
        vapply(
          eyes_data[[columns_to_correct[i]]],
          shift_image_by_dimension,
          shift_before = top_left_x,
          shift_after = shift_right,
          screen_dimension = screen_width,
          outside_image_is_na = outside_image_is_na,
          FUN.VALUE = 1.0
        )
      corrected_columns[columns_to_correct[i + 1]] <-
        vapply(
          eyes_data[[columns_to_correct[i + 1]]],
          shift_image_by_dimension,
          shift_before = top_left_y,
          shift_after = shift_bottom,
          screen_dimension = screen_height,
          outside_image_is_na = outside_image_is_na,
          FUN.VALUE = 1.0
        )
    }

    for (line in 1:dim(eyes_data)[1])
    {
      if (scroll_lag <= 0)
      {
        scroll <-
          shift_scroll(
            event,
            eyes_data[line ,],
            scroll,
            min_scroll,
            max_scroll,
            scroll_pixels,
            top_left_x,
            top_left_y,
            bottom_right_x,
            bottom_right_y
          )
      }
      else
      {
        if(length(future_scrolls)>0)
        {
          last_scroll <- future_scrolls[length(future_scrolls)]
          if(future_scrolls_timestamp[1] <= eyes_data[line ,]$Timestamp.Shifted)
          {
            future_scrolls <- future_scrolls[-1]
            future_scrolls_timestamp <- future_scrolls_timestamp[-1]
          }
        }
        else
        {
          last_scroll <- scroll
        }
        scroll_new <-
          shift_scroll(
            event,
            eyes_data[line ,],
            last_scroll,
            min_scroll,
            max_scroll,
            scroll_pixels,
            top_left_x,
            top_left_y,
            bottom_right_x,
            bottom_right_y
          )
        if (scroll_new != last_scroll)
        {
          future_scrolls[length(future_scrolls) + 1] <- scroll_new
          future_scrolls_timestamp[length(future_scrolls_timestamps) + 1] <- eyes_data[line ,]$Timestamp.Shifted + scroll_lag
        }
      }

      scroll_vector[line] <- scroll
      flags <-
        check_rules_true(rules, eyes_data[line, ], flags, fixed_areas, scroll)

      for (i in seq(1, length(columns_to_correct), 2))
      {
        corrected_y <-
          enforce_rules(flags,
                        fixed_areas,
                        corrected_columns[line, ],
                        columns_to_correct[i:(i + 1)])
        if (is.na(corrected_y))
        {
          corrected_columns[line, columns_to_correct[i + 1]] <-
            corrected_columns[[line, columns_to_correct[i + 1]]] + scroll
        }
        else
        {
          corrected_columns[line, columns_to_correct[i + 1]] <- corrected_y
        }
      }
      if(progression_bar == TRUE)
      {
        utils::setTxtProgressBar(pb, line)
      }
    }

    for (name in names(corrected_columns))
    {
      eyes_data[paste("Corrected.", name, sep = "")] <-
        corrected_columns[name]
    }
    eyes_data$Scroll <- scroll_vector
    if (output_file != "")
    {
      utils::write.csv(eyes_data,
                       file = output_file,
                       row.names = FALSE,
                       na = "")
    }
    return(eyes_data)
  }

#' @title Creates generic heatmap
#'
#' @description Creates a heatmap based on the dataset and the full page image
#'
#' @param data The dataset output by the \code{\link{eye_scroll_correct}} function
#' @param heatmap_image The unscrolled webpage image on which to apply the heatmap
#'
#' @return A plot with the image and the heatmap
#' @examples
#' \dontrun{
#' img <- readPNG("test.png")
#' test_data <- eye_scroll_correct(fixed_areas = test_fixed_areas,
#'     rules = test_rules, calibration = test_calibration)
#' generate_heatmap(data = test_data, heatmap_image = img)
#' }
#' @export
#' @importFrom rlang .data
generate_heatmap <- function(data, heatmap_image)
{
  data <-
    data[stats::complete.cases(data[, 'Corrected.Fixation.Y']),]
  data$Corrected.Fixation.Y <-
    dim(heatmap_image)[1] - data$Corrected.Fixation.Y
  ggplot2::ggplot(data,
                  ggplot2::aes(.data$Corrected.Fixation.X, .data$Corrected.Fixation.Y))  +
    ggplot2::annotation_raster(
      heatmap_image,
      xmin = -Inf,
      xmax = Inf,
      ymin = -Inf,
      ymax = Inf
    ) +
    ggplot2::stat_density2d(geom = "polygon", ggplot2::aes(fill = .data$..level.., alpha = 0.15)) +
    ggplot2::geom_point(size = 1) +
    ggplot2::scale_fill_gradient(low = "green", high = "red") +
    ggplot2::scale_x_continuous(limits = c(0, dim(heatmap_image)[2]), expand =
                                  c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(0, dim(heatmap_image)[1]), expand =
                                  c(0, 0)) +
    ggplot2::coord_fixed()
}
