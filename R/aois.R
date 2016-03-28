

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
