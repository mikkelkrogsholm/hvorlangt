#' Get distance to POIs
#'
#' This function get s distances to POIs based on an address
#'
#' @param address the address to get POIs for
#' @param token the token for the API
#'
#' @return a tibble
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#'
#' address <- "Amager Boulevard 126, 2300 København S"
#' token <- "eyJkcGZ4IjogImh2b3JsYW5ndGVyZGVyIiwgInByaXZzIjogInIxWjByMEYwazZCdFdxUWNPVXlrQi95NlNVcEp2MlFiZ3lYZXRxNEhZNFhPLzNZclcwK0s5dz09In0.fP4JWis69HmaSg5jVHiK8nemiCu6VaMULSGGJyK4D4PkWq4iA1+nSHWMaHxepKwJ83sEiy9nMNZhv7BcktRNrA"
#'
#' how_far_is_it(address, token)
how_far_is_it <- function(address, token){

  # Custom functions
  parse_output_json <- function(out){
    tibble::tibble(poi_id = out$poi_id,
           type = NA,
           name = out$name,
           from_lat = out$fromlatlng[1],
           from_lng = out$fromlatlng[2],
           travel_seconds = out$travelseconds,
           mot = out$mot,
           route_polyline = out$routepolyline,
           euclidean_meters = out$euclideanmeters,
           to_lat = out$tolatlng[1],
           to_lng = out$tolatlng[2],
           routed_meters = out$routedmeters)
  }

  # Urlencode the address
  address <- utils::URLencode(address)

  # The types
  types_vector <- c("daycare", "doctor", "hospital", "junction", "metro", "school", "stop",
                    "strain", "supermarket", "train", "library", "pharmacy",
                    "coast", "forest", "lake",
                    "airport", "sportshall", "publicbath", "soccerfield", "roadtrain")

  types <- types_vector %>% stringr::str_c(collapse = ",")

  # Build the url
  url <- glue::glue("https://hvorlangterder.poi.viamap.net/v1/nearestpoi/?poitypes={types}&fromaddress={address}&mot=foot&token={token}")

  # Get the data
  output_json <- jsonlite::fromJSON(url)

  # Parse the output
  output_tibble <- purrr::map_dfr(output_json, parse_output_json)

  output_tibble$type <- types_vector

  # Return the data
  return(output_tibble)
}



#' Create distance features
#'
#' @param how_far_df the output of how_far_is_it()
#'
#' @return a tibble
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#'
#' address <- "Amager Boulevard 126, 2300 København S"
#' token <- "eyJkcGZ4IjogImh2b3JsYW5ndGVyZGVyIiwgInByaXZzIjogInIxWjByMEYwazZCdFdxUWNPVXlrQi95NlNVcEp2MlFiZ3lYZXRxNEhZNFhPLzNZclcwK0s5dz09In0.fP4JWis69HmaSg5jVHiK8nemiCu6VaMULSGGJyK4D4PkWq4iA1+nSHWMaHxepKwJ83sEiy9nMNZhv7BcktRNrA"
#'
#' how_far_df <- how_far_is_it(address, token)
#' how_far_features <- create_distance_features(how_far_df)
create_distance_features <- function(how_far_df){

  how_far_features <- how_far_df %>%
    dplyr::select(type, travel_seconds, euclidean_meters, routed_meters) %>%
    tidyr::gather(var, value, -type) %>%
    tidyr::unite(feat, type, var) %>%
    tidyr::spread(feat, value)

  return(how_far_features)
}


