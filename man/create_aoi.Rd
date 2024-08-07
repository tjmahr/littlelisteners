% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aois.R
\name{create_aoi}
\alias{create_aoi}
\title{Create an AOI object}
\usage{
create_aoi(aoi_name, x_pix, y_pix, screen_width = 1920, screen_height = 1080)
}
\arguments{
\item{aoi_name}{label of the AOI}

\item{x_pix}{location of the left and right edges in pixels.}

\item{y_pix}{location of the bottom and top edges in pixels.}

\item{screen_width}{width of the screen in pixels. Defaults to 1920.}

\item{screen_height}{width of the screen in pixels. Defaults to 1080.}
}
\value{
an AOI object.
}
\description{
Create an object representing an Area of Interest (AOI). Only rectangles are
supported (like a jpeg image in an experiment). Pixel (0,0) is the lower left
corner of the screen.
}
