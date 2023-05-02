#' Determine county proxies for the largest cities in the US
#'
#' Create a data set containing county proxy information for the largest
#' cities in the US, according to custom city-county match criteria.
#'
#' For this data, cities are defined as places, which can be incorporated
#' places (such as cities, towns, or villages), or census designated places
#' (CDPs).
#'
#' "Counties" includes several types of county-equivalent entities, such as:
#' * Parishes (Louisiana)
#' * Independent cities (Maryland, Missouri, Nevada, Virginia)
#' * District of Columbia (Washington, DC)
#' * Boroughs, municipalities, and census areas (Alaska)
#'
#' Cities in the US often do not cleanly follow the boundaries of US counties.
#' The following situations are possible for a city:
#' * the city is contained wholly within one county, with a remaining
#' portion of the county not belonging to that city;
#' * the city is equivalent to one or more counties (e.g., the 5 boroughs of
#' New York, NY which are all distinct counties);
#' * the city overlaps with portions of multiple counties. In these cases,
#' some portions of a county that overlaps with the city may be very small,
#' or vice versa.
#'
#'
#'
#' @param n_cities The number of large cities for which county proxies are
#' desired. Defaults to 50.
#' @param year The year of population data used to rank city sizes and
#' determine the largest `n_cities` cities.
#' @param rule The rule used for determining whether a county should be
#' considered an appropriate proxy for the city. Options are `'or'`, `'and'`,
#' `'county'`, and `'city'`.
#'  * `'county'`: Only the `min_p_county` requirement must be satisfied.
#'  `min_p_city` is not considered.
#'  * `'city'`: Only the `min_p_city` requirement is considered.
#'  `min_p_county` is not considered.
#'  * `'and'`: Both `min_p_county` and `min_p_city` requirements must be
#'  satisfied.
#'  * `'or'`: Either the `min_p_county` requirement or `min_p_city`
#'  requirement must be satisfied.
#' @param min_p_county The minimum proportion of the county that must
#' overlap with the city (weighted by population) for the county to be considered
#' an appropriate proxy for the city. Proportions are valued 0 (0%) to 1 (100%).
#' @param min_p_city The minimum proportion of the city that must
#' overlap with the county (weighted by population) for the county to be
#' considered an appropriate proxy for the city. Proportions are valued
#' 0 (0%) to 1 (100%).
#'
#' @return A data frame containing a crosswalk between cities and
#' appropriate county proxies
#' @export
#'
#' @examples
city_county_proxies_fun <- function(n_cities = 50,
                                    year,
                                    rule,
                                    min_p_county,
                                    min_p_city){

  # USES city_pop_by_year, city_county_2020_cw, city_county_2010_cw
  if(is.null(n_cities)){
    stop("No value for n_cities supplied. User must provide exactly one value.")
  } else if(length(n_cities) > 1){
    stop("User provided multiple values for n_cities. Please provide only one.")
  } else if(is.na(n_cities)) {
    stop("The value provided for n_cities is NA.")
  } else if(!is.numeric(n_cities)) {
    stop("The value of n_cities must be numeric.")
  }else if(n_cities > 812){
    stop(paste0("User provided an invalid value for n_cities: ",
                n_cities,
                ". The maximum value accepted is 812."))
  }
  if(is.null(year)){
    stop("No year supplied. User must provide exactly one year.")
  } else if(length(year) > 1){
    stop("User provided multiple values for year. Please provide only one.")
  } else if(is.na(year)) {
    stop("The value provided for year is NA.")
  } else if(!is.numeric(year)) {
    stop("The value of year must be numeric.")
  }else if(!(year %in% 2010:2021)){
    stop(paste0("User provided an invalid year: ",
                year,
                ". Accepted years are 2010 through 2021."))
  }
  if(!(rule %in% c('or', 'and', 'county', 'city'))){
    stop(paste0("Invalid rule type. Please use one of: ",
                "'or', 'and', 'county', 'city'."))}

  if(is.null(min_p_county)){
    stop(paste0("No value for min_p_county supplied. ",
                "User must provide exactly one value."))
  } else if(length(min_p_county) > 1){
    stop(paste0("User provided multiple values for min_p_county. ",
                "Please provide only one."))
  } else if(is.na(min_p_county)) {
    stop("The value provided for min_p_county is NA.")
  } else if(!is.numeric(min_p_county)) {
    stop("The value of min_p_county must be numeric.")
  }else if(min_p_county > 1){
    stop(paste0("User provided an invalid value for min_p_county: ",
                min_p_county,
                ". The maximum value accepted is 1."))
  }else if(min_p_county < 0){
    stop(paste0("User provided an invalid value for min_p_county: ",
                min_p_county,
                ". The minimum value accepted is 0."))
  }
  if(is.null(min_p_city)){
    stop("No value for min_p_city supplied. User must provide exactly one value.")
  } else if(length(min_p_city) > 1){
    stop("User provided multiple values for min_p_city. Please provide only one.")
  } else if(is.na(min_p_city)) {
    stop("The value provided for min_p_city is NA.")
  } else if(!is.numeric(min_p_city)) {
    stop("The value of min_p_city must be numeric.")
  }else if(min_p_city > 1){
    stop(paste0("User provided an invalid value for min_p_city: ",
                min_p_city,
                ". The maximum value accepted is 1."))
  }else if(min_p_city < 0){
    stop(paste0("User provided an invalid value for min_p_city: ",
                min_p_city,
                ". The minimum value accepted is 0."))
  }
  # STEP 1: SELECT N LARGEST CITIES IN A SPECIFIC YEAR
  temp_city_pop <-
    city_pop_by_year[,c("city_name","state_name","state_abbr",
                       as.character(year))]
  colnames(temp_city_pop)[4] <- "pop_est"
  temp_city_pop <- temp_city_pop %>%
    dplyr::filter(!is.na(.data$pop_est)) %>%
    dplyr::arrange(dplyr::desc(.data$pop_est)) %>%
    temp_city_pop <- temp_city_pop[1:n_cities,]
  temp_city_pop$rank <- 1:n_cities

  # STEP 2: SELECT THE CORRECT YEAR OF CITY-COUNTY CROSSWALK
  # DEPENDING ON YEAR, USE 2020 DATA (city_county_2020_cw)
  # OR 2010 DATA (city_county_2010_cw)
  if(year %in% 2010:2019){
    temp_city_county <- city_county_2020_cw
  }else{
    temp_city_county <- city_county_2020_cw
  }

  # STEP 3: MERGE AND USE RULES TO SELECT BEST COUNTY PROXIES FOR THE CITIES
  temp_merge <- merge(temp_city_county,
                      temp_city_pop,
                      by = c("state_abbr", "city_name"),
                      all.y = T) %>%
    dplyr::arrange(.data$rank, .data$p_county, .data$p_city)
  if(rule %in% "or"){
    print(paste0("County selection rule selected: at least ",
                 100*min_p_county,
                 "% of county overlaps with city, OR at least ",
                 100*min_p_city, "% of city overlaps with county."))
    temp_result <- temp_merge %>%
      filter(.data$p_county >= min_p_county | .data$p_city >= min_p_city)
  }else if(rule %in% "and"){
    print(paste0("County selection rule selected: at least ",
                 100*min_p_county,
                 "% of county overlaps with city, AND at least ",
                 100*min_p_city, "% of city overlaps with county."))
    temp_result <- temp_merge %>%
      filter(.data$p_county >= min_p_county & .data$p_city >= min_p_city)
  }else if(rule %in% "county"){
    print(paste0("County selection rule selected: at least ",
                 100*min_p_county,
                 "% of county overlaps with city."))
    temp_result <- temp_merge %>%
      filter(.data$p_county >= min_p_county )
  }else{
    print(paste0("city selection rule selected: at least ",
                 100*min_p_city, "% of city overlaps with county."))
    temp_result <- temp_merge %>%
      filter(.data$p_city >= min_p_city)
  }
  return(temp_result)
}
