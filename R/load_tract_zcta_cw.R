#' Load a census tract-to-ZCTA crosswalk
#'
#' Function to load in a data.frame containing a crosswalk between US census
#' tract and ZCTA (ZIP Code Tabulation Area) for year 2000 or 2010.
#'
#' ### Data sources used:
#' Missouri Census Data Center: Geographic Correspondence Engine.
#' Web page: https://mcdc.missouri.edu/applications/geocorr.html
#'
#' @param year a numeric year, either 2000 or 2010
#'
#' @return A data.frame containing full crosswalk details between census tract
#' and ZCTA.
#'
#' The crosswalk contains one row per combination of overlapping tract and ZCTA.
#'
#' Columns in the returned data are:
#' * `state_fips` - (character) 2-digit state FIPS code .
#' * `county_fips` - (character) 5-digit county FIPS code (2-digit state +
#'  3 digit county).
#' * `tract_fips` - (character) 11-digit census tract FIPS code (2-digit state +
#'  3 digit county + 6 digit tract).
#' * `zcta` - (character) 5-digit ZCTA (ZIP Code Tabulation Area) ID
#' * `pop` - (numeric) population count estimate within the overlapping portion
#' of ZCTA and tract.
#' * `p_tract` - (numeric) the proportion (valued 0-1) of the census tract
#' population residing in the portion which overlaps with the ZCTA.
#' * `p_zcta` - (numeric) the porportion (valued 0-1) of the ZCTA population
#' residing in the portion which overlaps with the census tract.
#'
#' @export
#'
#' @examples
#' tract_zcta_2010_cw <- load_tract_zcta_cw(2010)
#'

load_tract_zcta_cw <- function(year = 2010){
  if(year %in% 2010){
    return(tract_zcta_2010_cw)
  }else if(year %in% 2000){
    return(tract_zcta_2000_cw)
  }else{
    stop("year must be either 2000 or 2010.")
  }
}
