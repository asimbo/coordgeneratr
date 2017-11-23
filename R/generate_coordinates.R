#' @title
#' Generate random number of coordinates within a geographic bounding box using Openstreetmap.
#'
#' @description
#' \code{generate_coordinates} returns random location coordinate (latitude, longitude) pairs within a bounding box provided by Openstreetmap
#' @param area character name of area to sample from.
#' @param country character name of a country.
#' @param nsampes integer number of samples to be generated. Default is 10.
#'
#' @details
#' area   character name that will be used to create the bounding box with. Cities and town can be used here. Default is "bulawayo".
#' country   character name of a country. Default is "zimbabwe".
#' nsamples  integer the number of coordinate pairs (lat, long) to be generated. The bounding box is generated from openstreetmap.
#' @return dataframe of latitude and longitude
#' @example
#' coordinate_generate(city = "bulawayo", country = "zimbabwe", nsamples = 10)
#' @importFrom stats runif
#' @seealso runif,\url{http://wiki.openstreetmap.org/wiki/Bounding_Box}

generate_coordinates = function(placename, country, nsamples = 10){
  if (is.na(placename)){
    stop("place name should be a text")
  }
  if (is.na(country)){
    stop("country should be a text")
  }
  if (is.numeric(nsamples)){
    url = paste("http://nominatim.openstreetmap.org/?format=json&addressdetails=1&q=",tolower(placename), "+", tolower(country), "&format=json&limit=1")
    jdata = fromJSON(file = url)
    if (length(jdata) > 0){
      print(jdata[[1]]$address)
      bbx = jdata[[1]]$boundingbox %>% as.numeric()
      gensamples = cbind(runif(nsamples ,bbx[1],bbx[2]),runif(nsamples,bbx[3],bbx[4])) %>% as.data.frame()
      names(gensamples)[1] = "latitude"
      names(gensamples)[2] = "longitude"
      return(gensamples)
    }
    if (length(jdata) < 1){
      stop("No matches found. Check place name spellings")
    }

  }
}
