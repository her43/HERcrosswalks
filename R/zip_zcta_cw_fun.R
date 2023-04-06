### ZIP CODE TO ZCTA CROSSWALK FUNCTION
# CODE AUTHOR: HEATHER ROLLINS
# DATA SOURCE: FROM UDSMAPPER.ORG
# SITE: https://udsmapper.org/zip-code-to-zcta-crosswalk/
library(readxl)
library(rio)
library(tidyverse)
# OPTIONS FOR YEARS TO INCLUDE IN THE DATA , AND WHETHER YOU WANT PO CITY NAME
# INPUT:
#        years
#           = a vector of numeric years that you want to consider crosswalk
#             data for (limited to any combination of years between
#             2009 and 2021).
#             Default: c(2009:2021)
#        po_city_names
#           = T/F boolean, indicate whether you want your final
#             data returned to include Post office city names and
#             states for the zip code in each row (T), or not (F).
#             Default: T
# OUTPUT: a data.frame containing the crosswalk between ZIP Code and ZCTA,
#         with the best match determined by the priority order of:
#         (1) populated ZIP code, rather than PO Box or other type,
#         (2) the ZIP-ZCTA match with the highest number of years that
#             these ZIP and ZCTA matched in the raw crosswalks
#         (3) the most recent year's match out of all remaining options.
#         Columns in the data: zip, zcta, po_city, state

zip_zcta_cw_fun <- function(years = c(2009:2021), po_city_names = T){
  if(!(unique(years) %in% c(2009:2021))){
    stop("Years provided must be between 2009-2021 (inclusive)")}
  library(readxl)
  library(rio)
  library(tidyverse)
  years_zcta_files <- years
  url_base <- "https://udsmapper.org/wp-content/uploads/2022/10/"
  ## USE RIO IMPORT FUNCTION
  zip_zcta_raw_list <- lapply(years_zcta_files, function(year_i){
    if(year_i %in% 2009:2010){
      name_end <- "UDS.xls"
    }else{
      name_end <- "UDS.xlsx"
    }
    if(year_i %in% 2015){
      file_name_base_url <- "ZipCodetoZCTACrosswalk"
    }else{
      file_name_base_url <- "ZIPCodetoZCTACrosswalk" # CAPITAL LETTERS IN ZIP
    }
    this_data <- rio::import(file = paste0(url_base, file_name_base_url,
                                           year_i, name_end),
                             sheet = 1)
    this_data$in_this_year <- T
    colnames(this_data)[colnames(this_data) %in% "in_this_year"] <-
      paste0("in_", year_i)
    colnames(this_data) <- tolower(colnames(this_data))
    return(this_data)
  })
  ## HARMONIZE THE COLUMNS
  zip_zcta_harmoncols_long <- lapply(zip_zcta_raw_list, function(this_data){
    year_in_col <- colnames(this_data)
    year_in_col <- year_in_col[substr(year_in_col, 1, 5) %in% "in_20"]
    year_i <- as.numeric(substr(year_in_col, 4, 7))
    print(year_i)
    colnames(this_data) <- tolower(colnames(this_data))
    if(year_i %in% c(2009:2013, 2015)){
      this_data$zip_type <- this_data$ziptype
      this_data$zcta <- this_data$zcta_use
      this_data$po_city <- this_data$cityname
      this_data$state <- this_data$stateabbr
      result_data <- this_data[,c("zip", "zcta", "zip_type", "po_city",
                                  "state", year_in_col)]
    }else{
      if(year_i %in% 2016:2021){ this_data$zip <- this_data$zip_code }
      this_data$po_city <- this_data$po_name
      result_data <- this_data[,c("zip", "zcta", "zip_type", "po_city",
                                  "state", year_in_col)]
    }
    if(year_i %in% 2009:2012){
      result_data$zip_type <-
        ifelse(result_data$zip_type %in% "M", "Military",
               ifelse(result_data$zip_type %in% c("P","U"), "P.O. Box",
                      ifelse(result_data$zip_type %in% "S", "Standard",
                             ifelse(result_data$zip_type %in% "L-PY",
                                    "Legacy", NA))))
    }else{
      result_data$zip_type <-
        ifelse(grepl(x = result_data$zip_type, pattern = "missing zip",
                     ignore.case = T), "ZCTA missing ZIP",
               ifelse(grepl(x = result_data$zip_type,
                            pattern = "Post Office",
                            ignore.case = T), "P.O. Box",
                      ifelse(grepl(x = result_data$zip_type,
                                   pattern = "ZIP Code Area",
                                   ignore.case = T), "Standard",
                             ifelse(grepl(x = result_data$zip_type,
                                          pattern = "add ZIP",
                                          ignore.case = T), "add ZIP",
                                    ifelse(grepl(x = result_data$zip_type,
                                                 pattern = "ZCTA Add",
                                                 ignore.case = T), "ZCTA Add",
                                           NA)))))
    }
    result_data$join_type <-
      ifelse(is.na(result_data$zcta), NA,
             ifelse(is.na(result_data$zip), NA,
                    ifelse(result_data$zcta == result_data$zip, "match",
                           ifelse(result_data$zcta != result_data$zip, "merge",
                                  NA))))
    result_data$zcta <-
      ifelse(is.na(result_data$zcta),
             NA,
             ifelse(grepl(x = result_data$zcta,
                          pattern = "[[:alpha:]]|[[:punct:]]|[[:space:]]"),
                    NA,
                    sprintf("%05i", as.numeric(result_data$zcta))))
    result_data$zip <-
      ifelse(is.na(result_data$zip),
             NA,
             ifelse(grepl(x = result_data$zip,
                          pattern = "[[:alpha:]]|[[:punct:]]|[[:space:]]"),
                    NA,
                    sprintf("%05i", as.numeric(result_data$zip))))
    result_data$year <- year_i
    result_data <- result_data[c("zip", "zcta", "zip_type", "year")]
    po_name_data <- this_data[,c("zip", "zcta", "po_city",
                                 "state")]
    po_name_data$year <- year_i
    return(list(result_data = result_data,
                po_name_data = po_name_data))
  })
  po_name_data <-
    lapply(zip_zcta_harmoncols_long, function(this_data){
      return(this_data$po_name_data)
    })
  po_name_data <- do.call(rbind, po_name_data)
  zip_zcta_result_data <-
    lapply(zip_zcta_harmoncols_long, function(this_data){
      return(this_data$result_data)
    })
  ## BIND ALL YEARS TOGETHER, DETERMINE THE PRIORITY LEVEL OF THE ROWS
  zip_zcta_priority_info <- do.call(rbind, zip_zcta_result_data) %>%
    dplyr::filter(!is.na(zcta)) %>%
    dplyr::filter(!is.na(zip)) %>%
    dplyr::mutate(type_priority =
                    case_when(zip_type %in% "Standard" ~ 1,
                              zip_type %in% "P.O. Box" ~ 2,
                              zip_type %in% "add ZIP" ~ 3,
                              zip_type %in% "ZCTA Add" ~ 4,
                              zip_type %in% "ZCTA missing ZIP" ~ 5,
                              zip_type %in% "Legacy" ~ 6,
                              zip_type %in% "Military" ~ 7 ,
                              T ~ 9)) %>%
    dplyr::group_by(zip, zcta) %>%
    dplyr::summarize(latest_year = max(year, na.rm = T),
                     type_priority = min(type_priority, na.rm = F),
                     n_years = n())
  n_zips_check <- length(unique(zip_zcta_priority_info$zip))
  # FOR EACH ZIP CODE, GET THE MIN TYPE PRIORITY, MAX LATEST YEAR,
  # AND MAX N YEARS
  print(n_zips_check)
  zip_best_info_1 <- zip_zcta_priority_info %>%
    dplyr::group_by(zip) %>%
    dplyr::summarize(min_type_priority = min(type_priority, na.rm = T),
                     n_zcta_1 = length(unique(zcta)))
  # MERGE THE "BEST" (MINIMUM TYPE) INFO BACK TO THE FULL PRIORITY DATA,
  # SELECT THE BEST TYPE.
  zip_zcta_cw_1_merge <- merge(zip_zcta_priority_info,
                               zip_best_info_1,
                               by = "zip", all = T)
  zip_zcta_cw_1 <- zip_zcta_cw_1_merge[zip_zcta_cw_1_merge$type_priority ==
                                         zip_zcta_cw_1_merge$min_type_priority,]
  print(length(unique(zip_zcta_cw_1$zip))) # [1] 41476
  if(n_zips_check !=length(unique(zip_zcta_cw_1$zip))){
    stop("Something went wrong with priority step 1")}
  # SAME WITH N YEARS
  zip_best_info_2 <- zip_zcta_cw_1 %>%
    dplyr::group_by(zip) %>%
    dplyr::summarize(max_n_years = max(n_years, na.rm = T),
                     n_zcta_2 = length(unique(zcta)))
  zip_zcta_cw_2_merge <- merge(zip_zcta_cw_1,
                               zip_best_info_2,
                               by = "zip", all = T)
  zip_zcta_cw_2 <- zip_zcta_cw_2_merge[zip_zcta_cw_2_merge$n_years ==
                                         zip_zcta_cw_2_merge$max_n_years,]
  print(length(unique(zip_zcta_cw_2$zip))) # [1] 41476
  if(n_zips_check !=length(unique(zip_zcta_cw_2$zip))){
    stop("Something went wrong with priority step 2")}
  # SAME WITH MOST RECENT YEAR
  zip_best_info_3 <- zip_zcta_cw_2 %>%
    dplyr::group_by(zip) %>%
    dplyr::summarize(max_latest_year = max(latest_year, na.rm = T),
                     n_zcta_3 = length(unique(zcta)))
  zip_zcta_cw_3_merge <- merge(zip_zcta_cw_2,
                               zip_best_info_3,
                               by = "zip", all = T)
  zip_zcta_cw_3 <- zip_zcta_cw_3_merge[zip_zcta_cw_3_merge$latest_year ==
                                         zip_zcta_cw_3_merge$max_latest_year,]
  print(length(unique(zip_zcta_cw_3$zip))) # [1] 41476
  if(n_zips_check !=length(unique(zip_zcta_cw_3$zip))){
    stop("Something went wrong with priority step 3")}

  ###### PO NAMES ########
  if(po_city_names %in% T){
    # KEEP THE ZIP_ZCTA MATCHES THAT EXIST IN THE CROSSWALK WE CREATED ABOVE
    po_name_data_filtered <- merge(po_name_data,
                                   zip_zcta_cw_3[,c("zip","zcta",
                                                    "type_priority")],
                                   all.y = T)
    po_name_data_filtered <-
      po_name_data_filtered[!is.na(po_name_data_filtered$type_priority),]
    ## BIND ALL YEARS TOGETHER, DETERMINE THE PRIORITY LEVEL OF THE ROWS
    names_best_info_1 <- po_name_data_filtered %>%
      dplyr::filter(!is.na(zcta)) %>%
      dplyr::filter(!is.na(zip)) %>%
      dplyr::group_by(zip, zcta, po_city, state) %>%
      dplyr::summarize(latest_year = max(year, na.rm = T),
                       n_years = n())
    n_zips_check <- length(unique(names_best_info_1$zip))
    print(n_zips_check)
    names_best_info_2 <- names_best_info_1 %>%
      dplyr::group_by(zip, zcta) %>%
      dplyr::summarize(max_n_years = max(n_years, na.rm = T),
                       n_names_2 = n()) %>%
      ungroup()
    names_best_info_2_merge <- merge(names_best_info_1,
                                     names_best_info_2,
                                     by = c("zip", "zcta"), all = T)
    po_name_data_2 <-
      names_best_info_2_merge[names_best_info_2_merge$n_years ==
                                names_best_info_2_merge$max_n_years,]
    print(length(unique(po_name_data_2$zip))) # [1] 41476
    if(n_zips_check !=length(unique(po_name_data_2$zip))){
      stop("Something went wrong with priority step 2")}
    # SAME WITH MOST RECENT YEAR
    names_best_info_3 <- po_name_data_2 %>%
      dplyr::group_by(zip, zcta) %>%
      dplyr::summarize(max_latest_year = max(latest_year, na.rm = T),
                       n_names_3 = n()) %>%
      ungroup()
    names_best_info_3_merge <- merge(po_name_data_2,
                                     names_best_info_3,
                                     by = c("zip", "zcta"), all = T)
    po_name_data_3 <-
      names_best_info_3_merge[names_best_info_3_merge$latest_year ==
                                names_best_info_3_merge$max_latest_year,]
    print(length(unique(po_name_data_3$zip))) # [1] 41476
    if(n_zips_check !=length(unique(po_name_data_3$zip))){
      stop("Something went wrong with priority step 3")}
    final_cw <- po_name_data_3[,c("zip", "zcta", "po_city", "state")]
  }else{
    final_cw <- zip_zcta_cw_3[,c("zip", "zcta")]
  }
  return(final_cw)
}
