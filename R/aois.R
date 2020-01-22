
#' Map the x and y positions of looks to Areas of Interest.
#'
#' @param x a dataframe of looking data
#' @param aois an AOI or list of AOIs
#' @param default_onscreen default label to use for a look onscreen that does
#'   not fall into an AOI. Default is `"tracked"`
#' @return an updated dataframe
#'
#' @details
#' The function current assumes the conventions used in our lab. It will create
#' a column called `GazeByAOI` with the label of the AOI for each look. It does
#' this by checking whether the columns `XMean` and `YMean` fall into to the
#' boundaries of the AOI.
#'
#' Offscreen looks receive `NA`.
#'
#' @export
add_aois <- function(x, aois, default_onscreen = "tracked") {
  # Assign default non-missing AOI
  x$GazeByAOI <- default_onscreen

  # Check for each AOI
  for (this_aoi in aois) {
    aoi_location <- this_aoi
    gaze_at_location <- get_frames_with_gaze_in_aoi(x, this_aoi)
    x$GazeByAOI[gaze_at_location] <- this_aoi$aoi_name
  }

  # Add missing looks
  gaze_mistracked <- is.na(x$XMean) | is.na(x$YMean)
  x$GazeByAOI[gaze_mistracked] <- NA
  x
}


get_frames_with_gaze_in_aoi <- function(gazes, img_AOI) {
  x_bounds <- img_AOI$x
  y_bounds <- img_AOI$y
  gaze_in_x_bounds <- check_looks_in_bounds(gazes$XMean, x_bounds[1], x_bounds[2])
  gaze_in_y_bounds <- check_looks_in_bounds(gazes$YMean, y_bounds[1], y_bounds[2])
  gaze_in_aoi <- gaze_in_x_bounds & gaze_in_y_bounds
  gaze_in_aoi
}

check_looks_in_bounds <- function(xs, lower_bound, upper_bound) {
  gaze_in_bounds <- lower_bound <= xs & xs <= upper_bound
  gaze_in_bounds[is.na(gaze_in_bounds)] <- FALSE
  gaze_in_bounds
}


#' Create an AOI object
#'
#' Create an object representing an Area of Interest (AOI). Only rectangles are
#' supported (like a jpeg image in an experiment). Pixel (0,0) is the lower left
#' corner of the screen.
#'
#' @param aoi_name label of the AOI
#' @param x_pix location of the left and right edges in pixels.
#' @param y_pix location of the bottom and top edges in pixels.
#' @param screen_width width of the screen in pixels. Defaults to 1920.
#' @param screen_height width of the screen in pixels. Defaults to 1080.
#' @return an AOI object.
#' @export
create_aoi <- function(aoi_name, x_pix, y_pix, screen_width = 1920, screen_height = 1080) {
  left_prop <- min(x_pix) / screen_width
  right_prop <- max(x_pix) / screen_width
  lower_prop <- min(y_pix) / screen_height
  upper_prop <- max(y_pix) / screen_height

  aoi <- list(
    aoi_name = aoi_name,
    x = c(left_prop, right_prop),
    y = c(lower_prop, upper_prop),
    screen_width = screen_width,
    screen_height = screen_height,
    x_pix = sort(x_pix),
    y_pix = sort(y_pix))

  structure(aoi, class = "AOI")
}




annotate_aoi <- function(AOI, ...) {
 ggplot2::annotate("rect", xmin = min(AOI$x_pix), xmax = max(AOI$x_pix),
           ymin = min(AOI$y_pix), ymax = max(AOI$y_pix), ...)
}
