get_frames_with_gaze_in_aoi <- function(gazes, img_AOI) {
  x_bounds <- img_AOI$x
  y_bounds <- img_AOI$y
  gaze_in_x_bounds <- check_looks_in_bounds(gazes$XMean, x_bounds[1], x_bounds[2])
  gaze_in_y_bounds <- check_looks_in_bounds(gazes$YMean, y_bounds[1], y_bounds[2])
  gaze_in_aoi <- gaze_in_x_bounds & gaze_in_y_bounds
  gaze_in_aoi
}

check_looks_in_bounds <- function (xs, lower_bound, upper_bound) {
  gaze_in_bounds <- lower_bound <= xs & xs <= upper_bound
  gaze_in_bounds[is.na(gaze_in_bounds)] <- FALSE
  gaze_in_bounds
}

#' @export
add_aois <- function(x, AOIs, default_onscreen = "tracked") {
  # Assign default non-missing AOI
  x$GazeByAOI <- default_onscreen

  # Check for each AOI
  for (aoi_num in seq_along(AOIs)) {
    aoi_location <- names(AOIs)[aoi_num]
    gaze_at_location <- get_frames_with_gaze_in_aoi(x, AOIs[[aoi_num]])
    x$GazeByAOI[gaze_at_location] <- aoi_location
  }

  # Add missing looks
  gaze_mistracked <- is.na(x$XMean) | is.na(x$YMean)
  x$GazeByAOI[gaze_mistracked] <- NA
  x
}

#' @export
AOI <- function(x_pix, y_pix, width = 1920, height = 1080) {
  left_prop <- min(x_pix) / width
  right_prop <- max(x_pix) / width
  lower_prop <- min(y_pix) / height
  upper_prop <- max(y_pix) / height
  structure(list(x = c(left_prop, right_prop),
                 y = c(lower_prop, upper_prop)), class = "AOI")
}
