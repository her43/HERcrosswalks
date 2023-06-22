#' Approximate ZCTA-level ACS data with census tracts
#'
#' Approximate ZCTA (ZIP Code Tabulation Area) level ACS variables using
#' available census-tract level ACS data, for any year between 2009 and 2019.
#' This process utilizes both the tidycensus package and a tract-to-ZCTA
#' crosswalk.
#'
#' ACS 5-year survey variables generally begin with "B". Note that the
#' tidycensus format for ACS variables includes an underscore between the
#' table and variable number, e.g., "B01001_001". For a full list of
#' ACS 5-year survey variable names, see tidycensus::load_variables().
#'
#' This process only works for count data. Do not use this function for
#' non-count variables (medians, proportions, etc.).
#'
#' Data source used for ZCTA-tract crosswalks:
#' Missouri Census Data Center: Geographic Correspondence Engine.
#' Web page: https://mcdc.missouri.edu/applications/geocorr.html
#'
#' If you are new to tidycensus, and need to set up an API key for census data,
#' see this help page to get started:
#' https://walker-data.com/tidycensus/articles/basic-usage.html
#'
#' @param state_num A 2-digit state FIPS code. Both numeric and character
#' values are accepted. ACS data can only be downloaded for one state at a time.
#' @param acs_vars A vector of ACS 5-year survey variables.
#' @param acs_year A numeric year which indicates the final year in the 5-year
#' ACS survey data (e.g., `acs_year = 2015` results in 2011-2015 ACS data).
#' The value must be between 2009 and 2019.
#'
#' @return A data frame containing the best approximation of ZCTA-level ACS data
#' for the state and year provided. The data will contain one row per
#' combination of ZCTA & ACS variable. The data frame will contain the following
#' columns:
#'
#' * `state_fips` - (character) 2-digit state FIPS code, matching the provided
#' value of `state_num`.
#' * `zcta` - (character) 5-digit ZCTA ID code
#' * `variable` - (character) the ACS variable(s) requested.
#' * `est_zcta` - (numeric) the ZCTA-level estimate of the ACS variable.
#' * `p_zcta_covered` - (numeric) the proportion (valued 0-1) of the ZCTA
#' (weighted by population) that was able to be matched with census tracts and,
#' in essense, "covered" by the aggregated full and partial tract-level
#' estimates. For example, a value of 0.5 indicates that only 50% of the ZCTA is
#' represented by this approximated value.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @import tidycensus
#' @export
#'
#' @examples
#' temp_pop_ny_2009 <- acs_tract_to_zcta_fun("36","B01001_001",2009)
#' temp_pop_ny_pa_2009 <- do.call(rbind, lapply(c("36","42"),function(i){
#'   acs_tract_to_zcta_fun(i,"B01001_001",2009)}))
#'
acs_tract_to_zcta_fun <- function(state_num, acs_vars, acs_year){
  if(is.null(state_num)){stop("state_num value not provided.")}
  if(is.null(acs_vars)){stop("acs_vars value(s) not provided.")}
  if(is.null(acs_year)){stop("acs_year value not provided.")}
  if(length(state_num) != 1){
    stop("You must provide 1 value for state_num.")
  }
  if(length(acs_year) != 1){
    stop("You must provide 1 value for acs_year, within 2009-2019.")
  }
  if(acs_year %in% 2009){
    tract_zcta_cw <- tract_zcta_2000_cw
    print("using the 2000 tract-to-ZCTA crosswalk")
  }else if(acs_year %in% 2010:2019){
    tract_zcta_cw <- tract_zcta_2010_cw
    print("using the 2010 tract-to-ZCTA crosswalk")
  }else{
    stop(paste0("acs_year ", acs_year," not accepted. You must select an ACS ",
                "year between 2009 and 2019."))
  }
  if(is.character(state_num)){
    if((nchar(state_num) %in% 1)){
      state_num <- sprintf("%02d",as.numeric(state_num) )
    }else if(nchar(state_num) >2){
      stop("Invalid state_num. Must be no more than 2 digits.")
    }
  }else{
    if(state_num %in% 1:76){
      state_num <- sprintf("%02d",as.numeric(state_num) )
    }else{
      stop("Invalid state_num")
    }
  }
  temp_raw <- tidycensus::get_acs(geography = "tract",
                                  variables = acs_vars,
                                  year = acs_year,
                                  state = state_num)
  temp_merged <- temp_raw %>%
    dplyr::select(tract_fips = .data$GEOID, .data$variable, .data$estimate) %>%
    # REMOVE ALL NA VALUES FOR ESTIMATE
    dplyr::filter(!is.na(.data$estimate))
    # MERGE TRACT LEVEL ACS WITH TRACT_ZCTA CW
  temp_merged <- merge(temp_merged,
                       tract_zcta_cw,
                       by = "tract_fips") %>%
    # MULTIPLY ESTIMATE BY % TRACT TO GET PARTIAL VALUE FOR ZCTA
    dplyr::mutate(est_zcta = .data$estimate * .data$p_tract) %>%
    # GROUP BY ZCTA AND SUM ESTIMATES TO GET FULL ZCTA VALUES
    dplyr::group_by(.data$state_fips, .data$zcta, .data$variable) %>%
    dplyr::summarize(est_zcta = sum(.data$est_zcta, na.rm = T),
                     # HOW MUCH OF THE ZCTA IS REPRESENTED
                     p_zcta_covered = sum(.data$p_zcta, na.rm = T)) %>%
    dplyr::ungroup() %>% as.data.frame()
  return(temp_merged)
}
