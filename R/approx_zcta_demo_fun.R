#' Approximate ZCTA-level summary demographic data via tract-level ACS data
#'
#' Approximate a selection of ZCTA (ZIP Code Tabulation Area) level summary
#' demographic data sourced from tract-level ACS data, for any year between
#' 2009 and 2019. This process utilizes both the tidycensus package and a
#' tract-to-ZCTA crosswalk.
#'
#' Data source used for ZCTA-tract crosswalks:
#' Missouri Census Data Center: Geographic Correspondence Engine.
#' Web page: https://mcdc.missouri.edu/applications/geocorr.html
#'
#' If you are new to tidycensus, and need to set up an API key for census data,
#' see this help page to get started:
#' https://walker-data.com/tidycensus/articles/basic-usage.html
#'
#' See also documentation for HERcrosswalks::acs_tract_to_zcta_fun()
#'
#' @param state_num A 2-digit state FIPS code. Both numeric and character
#' values are accepted. ACS data can only be downloaded for one state at a time.
#' If NULL, `state_abbr` will be used instead.
#' @param state_abbr A 2-letter state abbreviation. If NULL, `state_num` will
#' be used instead.
#' @param acs_year A numeric year which indicates the final year in the 5-year
#' ACS survey data (e.g., `acs_year = 2015` results in 2011-2015 ACS data).
#' The value must be between 2009 and 2019.
#' @param all_demo Boolean (T/F). If T, data for all demographic categories
#' (see below) will be returned.
#' @param age Boolean (T/F). If T, the returned data will include the
#' population distribution of 5-year age groups (from ACS table B01001).
#' @param race Boolean (T/F). If T, the returned data will include the
#' population distribution of race groups  (from ACS table B02001).
#' @param race_hisp Boolean (T/F). If T, the returned data will include the
#' population distribution of race/Hispanic ethnicity groups
#' (from ACS table B03002).
#' @param poverty Boolean (T/F). If T, the returned data will include the
#' proportion of the population below poverty level (from ACS table B17001).
#'
#' @return A data frame containing the best approximation of ZCTA-level ACS data
#' for the state and year provided. The data will contain one row per ZCTA.
#' A selection of summary demographic data will be included in the returned
#' data. By default, all data includes a population total. Other proportion
#' variables (valued 0-1) may be included according to parameters above.
#' @examples
#' ny_demo_2009 <-
#'   approx_zcta_demo_fun(state_abbr = "NY", acs_year = 2009, all_demo = TRUE)
#' pa_de_ages_2009 <-
#'   do.call(rbind, lapply(c("42", "10"),function(i){
#'     approx_zcta_demo_fun(state_num = i, acs_year = 2009, age = TRUE)}))
#' de_race_2010 <-
#'   approx_zcta_demo_fun(state_abbr = "DE", acs_year = 2010, race = TRUE)
#' ak_raceeth_2011 <-
#'   approx_zcta_demo_fun(state_num = 2, acs_year = 2011, race_hisp = TRUE)
#' de_pov_2019 <-
#'   approx_zcta_demo_fun(state_num = 10, acs_year = 2019, poverty = TRUE)
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom tidyr spread
#' @export
approx_zcta_demo_fun <- function(state_num = NULL,
                                state_abbr = NULL,
                                acs_year,
                                all_demo = F,
                                age = F,
                                race = F,
                                race_hisp = F,
                                poverty = F){
  if(is.null(state_num)){
    if(is.null(state_abbr)){
      stop("Either state_num or state_abbr is required.")
    }else if(state_abbr %in% states_fips$abbr){
      state_num <- states_fips$fips[states_fips$abbr %in% state_abbr]
    }else{
      stop("state_abbr is not recognized.")
    }
  }else{
    if(is.numeric(state_num)){
      state_num <- sprintf("%02d", state_num)
    }
    if(is.null(state_abbr)){
      if(state_num %in% states_fips$fips){
        state_abbr <- states_fips$abbr[states_fips$fips %in% state_num]
      }else{
        stop("state_num is not recognized.")
      }
    }else{ ## STATE_ABBR IS NOT NULL
      if(state_num %in% states_fips$fips){
        if(state_abbr %in% states_fips$abbr){
          if(!(state_abbr %in%
               states_fips$abbr[states_fips$fips %in% state_num])){
            stop("Mismatched state_num and state_abbr")
          } # ELSE DON'T DO ANYTHING.
        }else{ # STATE_ABBR NOT RECOGNIZED
          warning("state_abbr not recognized, using state_num instead.")
          state_abbr <- states_fips$abbr[states_fips$fips %in% state_num]
        }
      }else if(state_abbr %in% states_fips$abbr){
        warning("state_num not recognized, using state_abbr instead.")
        state_num <- states_fips$fips[states_fips$abbr %in% state_abbr]
      }else{
        stop("state_num and state_abbr are both not recognized.")
      }
    }
  }
  vars_age  <- b01001_(c(3:25, 27:49))
  vars_race <- b02001_(1:6)
  vars_rh   <- b03002_(c(1, 3:7, 12) )
  vars_pov  <- b17001_(1:2)
  vars_get  <- b01001_(1)
  if(all_demo %in% TRUE){
    vars_get <- c(vars_get, vars_age, vars_race, vars_rh, vars_pov, vars_get)
  }else{
    if(age %in% TRUE)      {vars_get <- c(vars_get, vars_age)}
    if(race %in% TRUE)     {vars_get <- c(vars_get, vars_race)}
    if(race_hisp %in% TRUE){vars_get <- c(vars_get, vars_rh)}
    if(poverty %in% TRUE)  {vars_get <- c(vars_get, vars_pov)}
  }
  vars_get <- unique(vars_get)
  if(length(vars_get) %in% 0) { stop("no acs variables selected")}
  # USE HELPER FUNCTION TO DOWNLOAD DATA
  raw_acs <- acs_tract_to_zcta_fun(state_num = state_num,
                                   acs_vars = vars_get,
                                   acs_year = acs_year)
  p_zcta_by_table <- raw_acs %>%
    dplyr::mutate(acs_table = substr(.data$variable, 1, 6),
                  description =
                    dplyr::case_when(.data$acs_table %in% c("B01001") ~
                                "p_zcta_covered_tot",
                              .data$acs_table %in% c("B02001") ~
                                "p_zcta_covered_race",
                              .data$acs_table %in% c("B03002") ~
                                "p_zcta_covered_rh",
                              .data$acs_table %in% c("B17001") ~
                                "p_zcta_covered_pov")) %>%
    dplyr::group_by(.data$zcta, .data$description) %>%
    dplyr::summarize(p_zcta_covered = max(.data$p_zcta_covered, na.rm = T))
  p_zcta_by_table <-dplyr::ungroup( p_zcta_by_table) %>%
    dplyr::select(.data$zcta, .data$description, .data$p_zcta_covered)
  colnames(p_zcta_by_table)[3] <- "estimate"

  acs_data <- merge(raw_acs, acs_demo_raw_vars,
                    by.x = "variable", by.y = "acs_raw_var") %>%
    dplyr::mutate(
      acs_table = substr(.data$variable, 1, 6),
      description = paste0(.data$table_description,
                           "_",
                           .data$var_label)) %>%
    dplyr::group_by(.data$zcta, .data$description) %>%
    dplyr::summarize(estimate = sum(.data$est_zcta, na.rm = T))
  acs_data <- dplyr::ungroup(acs_data)
  # RBIND WITH THE SIMPLIFIED % ZCTA COVERED FOR EACH TABLE TYPE
  acs_data <- rbind(acs_data, p_zcta_by_table) %>%
    # RESHAPE FROM LONG TO WIDE
    tidyr::spread(key = .data$description,
                  value = .data$estimate, fill = NA) %>%
    dplyr::mutate(total = .data$age_total,
                  year = acs_year ,
                  state_fips = state_num,
                  state_abbr = state_abbr)
  if(all_demo %in% TRUE){
    acs_data <- acs_data %>%
      dplyr::mutate(
        p_age_00_04 = .data$age_00_04/.data$age_total,
        p_age_05_09 = .data$age_05_09/.data$age_total,
        p_age_10_14 = .data$age_10_14/.data$age_total,
        p_age_15_19 = ((.data$age_15_17 + .data$age_18_19)/
                         .data$age_total),
        p_age_20_24 = ((.data$age_20 + .data$age_21 +
                           .data$age_22_24)/.data$age_total),
        p_age_25_29 = .data$age_25_29/.data$age_total,
        p_age_30_34 = .data$age_30_34/.data$age_total,
        p_age_35_39 = .data$age_35_39/.data$age_total,
        p_age_40_44 = .data$age_40_44/.data$age_total,
        p_age_45_49 = .data$age_45_49/.data$age_total,
        p_age_50_54 = .data$age_50_54/.data$age_total,
        p_age_55_59 = .data$age_55_59/.data$age_total,
        p_age_60_64 = ((.data$age_60_61 + .data$age_62_64)/
                         .data$age_total),
        p_age_65_69 = ((.data$age_65_66 + .data$age_67_69)/.data$age_total),
        p_age_70_74 = .data$age_70_74/.data$age_total,
        p_age_75_79 = .data$age_75_79/.data$age_total,
        p_age_80_84 = .data$age_80_84/.data$age_total,
        p_age_85    = .data$age_85/.data$age_total,
        p_race_white  = .data$race_white/.data$race_total,
        p_race_black  = .data$race_black/.data$race_total,
        p_race_asian  = .data$race_asian/.data$race_total,
        p_race_pi     = .data$race_pi/.data$race_total,
        p_race_api    = (.data$race_asian + .data$race_pi)/.data$race_total,
        p_race_aian   = .data$race_aian/.data$race_total,
        p_race_other  = ((.data$race_total -
                            (.data$race_white + .data$race_black +
                                .data$race_asian + .data$race_pi +
                                .data$race_aian) ) /
                           .data$race_total),
        p_rh_hisp     = .data$rh_hisp/.data$rh_total,
        p_rh_nhwhite  = .data$rh_nhwhite/.data$rh_total,
        p_rh_nhblack  = .data$rh_nhblack/.data$rh_total,
        p_rh_nhasian  = .data$rh_nhasian/.data$rh_total,
        p_rh_nhpi     = .data$rh_nhpi/.data$rh_total,
        p_rh_nhapi    = (.data$rh_nhasian + .data$rh_nhpi)/.data$rh_total,
        p_rh_nhaian   = .data$rh_nhaian/.data$rh_total,
        p_rh_nhother  = ( (.data$rh_total -
                             (.data$rh_hisp + .data$rh_nhwhite +
                                 .data$rh_nhblack + .data$rh_nhasian +
                                 .data$rh_nhpi + .data$rh_nhaian) ) /
                            .data$rh_total),
        p_poverty   = .data$poverty_below/.data$poverty_denom
        ) %>%
      dplyr::select(.data$year, .data$state_fips, .data$state_abbr, .data$zcta,
                    .data$p_zcta_covered_tot, .data$total,
                    .data$p_age_00_04, .data$p_age_05_09, .data$p_age_10_14,
                    .data$p_age_15_19, .data$p_age_20_24, .data$p_age_25_29,
                    .data$p_age_30_34, .data$p_age_35_39, .data$p_age_40_44,
                    .data$p_age_45_49, .data$p_age_50_54, .data$p_age_55_59,
                    .data$p_age_60_64, .data$p_age_65_69, .data$p_age_70_74,
                    .data$p_age_75_79, .data$p_age_80_84, .data$p_age_85,
                    .data$p_race_white, .data$p_race_black, .data$p_race_asian,
                    .data$p_race_pi, .data$p_race_api, .data$p_race_aian,
                    .data$p_race_other,
                    .data$p_rh_hisp, .data$p_rh_nhwhite, .data$p_rh_nhblack,
                    .data$p_rh_nhasian, .data$p_rh_nhpi, .data$p_rh_nhapi,
                    .data$p_rh_nhaian, .data$p_rh_nhother,
                    .data$p_poverty)
  }else{
    keep_cols <- c("year", "state_fips", "state_abbr", "zcta",
                   "p_zcta_covered_tot", "total")
    if(age %in% TRUE) {
      acs_data <- acs_data %>%
        dplyr::mutate(
          p_age_00_04 = .data$age_00_04/.data$age_total,
          p_age_05_09 = .data$age_05_09/.data$age_total,
          p_age_10_14 = .data$age_10_14/.data$age_total,
          p_age_15_19 = ((.data$age_15_17  + .data$age_18_19)/
                           .data$age_total),
          p_age_20_24 = ((.data$age_20 + .data$age_21 +
                             .data$age_22_24)/.data$age_total),
          p_age_25_29 = .data$age_25_29/.data$age_total,
          p_age_30_34 = .data$age_30_34/.data$age_total,
          p_age_35_39 = .data$age_35_39/.data$age_total,
          p_age_40_44 = .data$age_40_44/.data$age_total,
          p_age_45_49 = .data$age_45_49/.data$age_total,
          p_age_50_54 = .data$age_50_54/.data$age_total,
          p_age_55_59 = .data$age_55_59/.data$age_total,
          p_age_60_64 = ((.data$age_60_61 + .data$age_62_64)/
                           .data$age_total),
          p_age_65_69 = ((.data$age_65_66 + .data$age_67_69)/.data$age_total),
          p_age_70_74 = .data$age_70_74/.data$age_total,
          p_age_75_79 = .data$age_75_79/.data$age_total,
          p_age_80_84 = .data$age_80_84/.data$age_total,
          p_age_85    = .data$age_85/.data$age_total
        )
      keep_cols <- c(keep_cols, "p_age_00_04", "p_age_05_09", "p_age_10_14",
                     "p_age_15_19", "p_age_20_24", "p_age_25_29", "p_age_30_34",
                     "p_age_35_39", "p_age_40_44", "p_age_45_49", "p_age_50_54",
                     "p_age_55_59", "p_age_60_64", "p_age_65_69", "p_age_70_74",
                     "p_age_75_79", "p_age_80_84", "p_age_85")
      }
    if(race %in% TRUE) {
      acs_data <- acs_data %>%
        dplyr::mutate(
          p_race_white  = .data$race_white/.data$race_total,
          p_race_black  = .data$race_black/.data$race_total,
          p_race_asian  = .data$race_asian/.data$race_total,
          p_race_pi     = .data$race_pi/.data$race_total,
          p_race_api    = (.data$race_asian + .data$race_pi)/.data$race_total,
          p_race_aian   = .data$race_aian/.data$race_total,
          p_race_other  = ((.data$race_total -
                              (.data$race_white + .data$race_black +
                                  .data$race_asian + .data$race_pi +
                                  .data$race_aian) ) /
                             .data$race_total)
        )
      keep_cols <- c(keep_cols, "p_race_white", "p_race_black", "p_race_asian",
                     "p_race_pi", "p_race_api", "p_race_aian", "p_race_other")
    }
    if(race_hisp %in% TRUE){
      acs_data <- acs_data %>%
        dplyr::mutate(
          p_rh_hisp     = .data$rh_hisp/.data$rh_total,
          p_rh_nhwhite  = .data$rh_nhwhite/.data$rh_total,
          p_rh_nhblack  = .data$rh_nhblack/.data$rh_total,
          p_rh_nhasian  = .data$rh_nhasian/.data$rh_total,
          p_rh_nhpi     = .data$rh_nhpi/.data$rh_total,
          p_rh_nhapi    = (.data$rh_nhasian + .data$rh_nhpi)/.data$rh_total,
          p_rh_nhaian   = .data$rh_nhaian/.data$rh_total,
          p_rh_nhother  = ( (.data$rh_total -
                               (.data$rh_hisp + .data$rh_nhwhite +
                                   .data$rh_nhblack + .data$rh_nhasian +
                                   .data$rh_nhpi + .data$rh_nhaian) ) /
                              .data$rh_total)
        )
      keep_cols <- c(keep_cols, "p_rh_hisp", "p_rh_nhwhite", "p_rh_nhblack",
                     "p_rh_nhasian", "p_rh_nhpi", "p_rh_nhapi", "p_rh_nhaian",
                     "p_rh_nhother")
    }
    if(poverty %in% TRUE){
      acs_data <- acs_data %>%
        dplyr::mutate(
          p_poverty   = .data$poverty_below/.data$poverty_denom)
      keep_cols <- c(keep_cols, "p_poverty")
    }
    acs_data <- acs_data[, keep_cols]
  }
  acs_data <- as.data.frame(acs_data)
  #acs_data[is.na(acs_data)] <- NA
  return(acs_data)
}
