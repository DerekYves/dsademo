#' \pkg{dsademo}
#' Provides support information for Derek Darves' teaching audition
#'
#' @importFrom jsonlite fromJSON
#' @importFrom RCurl getURL
#' @importFrom utils read.csv
#' @docType package
#' @name dsademo
NULL

#' Load the Starbucks geo data
#' @return A data.frame
#' @source \href{https://opendata.socrata.com/Business/All-Starbucks-Locations-in-the-US-Map/ddym-zvjk}{socrata}
#' @export
get_sb <- function(){
	x <- read.csv(system.file("extdata", "geo_bucks.txt", package="dsademo"))
	return(x)
}

#' baseurlfunc: Prints the Google Maps API URL
#' @param x NULL
#' @return the maps API URL
#' @examples
#' baseurl <- baseurlfunc()
#' @export
baseurlfunc <- function(x){
	x <- "https://maps.googleapis.com/maps/api/geocode/json?address="
	x
}

#' coffeetime: Calculates the distance to the nearest Starbucks
#' @param place A vector of length 2 containing the numeric lat in position 1,
#' and the numeric long in position 2, e.g. "41.42326, -73.94992"
#' @return A numeric vector of length 1 containing distance to the nearest Starbucks from \code{place} in kilometers.
#' @examples
#' \dontrun{
#' woods <- c(lat=41.42326, lng=-73.94992)
#' coffeetime(woods)
#' }
#' @export
coffeetime <- function(place) {
	sb <- read.csv(system.file("extdata", "geo_bucks.txt", package="dsademo"))
	rad <- pi/180 # radian conversion
	a1 <- sb[['Latitude']] * rad
	a2 <- sb[['Longitude']] * rad
	b1 <- place[1] * rad
	b2 <- place[2] * rad
	dlon <- b2 - a2
	dlat <- b1 - a1
	a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
	c <- 2 * atan2(sqrt(a), sqrt(1 - a))
	R <- 6378.145 # Earth's radius in km
	d <- R * c
	ks <- min(d) # smallest distance in the vector
	return(ks)
}


#' rcurlbuild: Sets the environment for dsademo
#' @param x a character vector of json text
#' @return A formatted object
#' @export
rcurlbuild <- function(x){
	x <- fromJSON('{"city" : "Z\\u00FCrich"}')
	x
}






