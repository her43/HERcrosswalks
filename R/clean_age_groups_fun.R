#' Create age groups
#'
#' Create new age group data from numeric age data
#'
#' Provide one of either `custom_groups` or `default_group`, but not both.
#'
#' If using a default/pre-set age grouping scheme, provide one of the following
#' values of `default_group` listed below (shown with associated age group
#' lower bounds)
#'
#' * "5" (char), 5 (num): 0, 5, 10, 15, ..., 80, 85 (5-year age groups)
#' * "10" (char), 10 (num): 0, 10, 20, ..., 70, 80 (10-year age groups)
#' * "65", "65+" (char), 65 (num): 0, 65
#' * "18", "18+" (char), 18 (num): 0, 18
#' * "10 offset": 0, 18, 25, 35, 45, ..., 75, 85
#'
#' @param age a numeric vector of ages
#' @param custom_groups a vector of the lower bounds of desired age groups,
#' e.g., c(0, 18, 65) for desired age groups 0-17, 18-64, 65+. Lower bound 0
#' will be added by default if missing from the list of provided values.
#' @param default_group (Optional) one of several preset age group schemes.
#' See below for optional `default_group` values.
#' @param sep_infant Boolean (T/F). If TRUE, then infants age 0 years will be
#' considered their own age group (0) separate from children aged 1+ years.
#' This results in an age group with lower bound 1 being added to the final
#' list of age groups, regardless of values provided for `custom_groups` or
#' `default_group`. If FALSE (default), then no extra changes will be made to
#' the age groups defined by either `custom_groups` or `default_group`.
#' @param max_age_lower (Optional) a numeric value for a maximum age group
#' lower bound. All age values greater than opr equal to this value will be
#' considered one age group, overwriting the higher age group bounds defined by
#' either `custom_groups` or `default_group`.
#' @param format The desired format of the returned age groups. If "numeric"
#' (default), then the numeric value of the lower bound for the age group is
#' returned (e.g., 75, 80, and 85 for age groups 75-79, 80-84, 85+). If "range"
#' or "character", then the age range label for the age groups will be returned
#' (e.g., "75-79", "80-84", and "85+").
#'
#' @return a vector the same length as `age` with corresponding age groups
#' instead of single-year ages.
#'
#' @export
#'
#' @examples
#' sample_ages <- seq(0,102, by = 3)
#' clean_age_groups_fun(age = sample_ages, custom_groups = c(18, 65))
#' clean_age_groups_fun(age = sample_ages, custom_groups = seq(15, 85, by = 10))
#' clean_age_groups_fun(age = sample_ages, default_group = "5", sep_infant = TRUE)
#' clean_age_groups_fun(age = sample_ages, default_group = "10", format = "range")
#' clean_age_groups_fun(age = sample_ages, default_group = "5", max_age_lower = 65)
#'
clean_age_groups_fun <- function(age,
                                 custom_groups = NULL,
                                 default_group = NULL,
                                 sep_infant = FALSE,
                                 max_age_lower = NULL,
                                 format = "numeric"){
  if(is.null(custom_groups) && is.null(default_group)){
    stop(paste0("Please define an age grouping system using options ",
                "custom_groups or default_group"))
  }else if((!is.null(custom_groups)) & (!is.null(default_group))){
    stop("Provide one of either custom_groups or default_group, not both.")
  }
  if(is.null(age)){
    stop("NULL data provided for age.")
  }else if(length(age) %in% 0){
    stop("No age data provided.")
  }else{
    age <- as.vector(age)
    age <- as.numeric(age)
    age <- floor(age)
    if(length(which(age < 0)) > 0){
      stop("Negative age value(s). Check data.")
    }
    if(length(which(age > 130)) > 0){
      stop("Value(s) of age above 130. Check data.")
    }
  }
  if(length(sep_infant) != 1){
    stop("Provide exactly 1 value for sep_infant")
  }
  if(is.numeric(custom_groups)){
    default_group <- NULL
    custom_groups <- round(custom_groups)
    if(length(custom_groups) %in% 1){
      if(0 %in% custom_groups){
        stop("You have specified only unique custom age group 0+.")
      }else{
        custom_groups <- c(0, custom_groups)
      }
    }
    if(sep_infant){
      print("You have selected to separate infants aged 0 from other ages 1+")
      ages_list <- c(1, ages_list)
    }
    custom_groups <- c(0, custom_groups)
    ages_list <- sort(unique(custom_groups))
    print(paste0("Your custom age group lower bounds are: ",
                 paste(ages_list, collapse = ", ")))
    age_group <- age
    n_groups <- length(ages_list)
    for(i in 1:(n_groups-1)){
      age_group[(age >= ages_list[i]) & (age < ages_list[i+1])] <- ages_list[i]
    }
    age_group[(age >= ages_list[n_groups])] <- ages_list[n_groups]
  }else if(default_group %in% c("5") | default_group %in% c(5) ){
    ages_list <- seq(0,85, by = 5)
    age_group <- dplyr::case_when(age %in% 0:4 ~ 0,
                           age %in% 5:9 ~ 5,
                           age %in% 10:14 ~ 10,
                           age %in% 15:19 ~ 15,
                           age %in% 20:24 ~ 20,
                           age %in% 25:29 ~ 25,
                           age %in% 30:34 ~ 30,
                           age %in% 35:39 ~ 35,
                           age %in% 40:44 ~ 40,
                           age %in% 45:49 ~ 45,
                           age %in% 50:54 ~ 50,
                           age %in% 55:59 ~ 55,
                           age %in% 60:64 ~ 60,
                           age %in% 65:69 ~ 65,
                           age %in% 70:74 ~ 70,
                           age %in% 75:79 ~ 75,
                           age %in% 80:84 ~ 80,
                           age %in% 85:150 ~ 85)
    # INFANTS
    if(sep_infant){
      print("Selected to separate infants aged 0 from other ages 1-4.")
      age_group[age %in% 1:4] <- 1
      ages_list <- c(0,1,seq(5,85, by = 5))
    }else{
      print("Selected combined infant age group 0 with other ages.")
    }

  }else if(default_group %in% c("10") |default_group %in% c(10)){
    ages_list <- seq(0,80, by = 10)
    age_group <- dplyr::case_when(age %in% 0:9 ~ 0,
                           age %in% 10:19 ~ 10,
                           age %in% 20:29 ~ 20,
                           age %in% 30:39 ~ 30,
                           age %in% 40:49 ~ 40,
                           age %in% 50:59 ~ 50,
                           age %in% 60:69 ~ 60,
                           age %in% 70:79 ~ 70,
                           age %in% 80:150 ~ 80)
    # INFANTS
    if(sep_infant){
      print("Selected to separate infants aged 0 from other ages 1-9.")
      age_group[age %in% 1:9] <- 1
      ages_list <- c(0,1,seq(10,80, by = 10))
    }else{
      print("Selected combined infant age group 0 with other ages.")
    }

  }else if(default_group %in% c("10 offset")){
    ages_list <- c(0,18,seq(25,85, by = 10))
    age_group <- dplyr::case_when(age %in% 0:17 ~ 0,
                                  age %in% 18:24 ~ 18,
                                  age %in% 25:34 ~ 25,
                                  age %in% 35:44 ~ 35,
                                  age %in% 45:54 ~ 45,
                                  age %in% 55:64 ~ 55,
                                  age %in% 65:74 ~ 65,
                                  age %in% 75:84 ~ 75,
                                  age %in% 85:150 ~ 85)
    # INFANTS
    if(sep_infant){
      print("Selected to separate infants aged 0 from other ages 1-17.")
      age_group[age %in% 1:17] <- 1
      ages_list <- c(0,1,18,seq(25,85, by = 10))
    }else{
      print("Selected combined infant age group 0 with other ages.")
    }

  }else if(default_group %in% c("65", "65+") | default_group %in% c(65) ){
    ages_list <- c(0,65)
    age_group <- dplyr::case_when(age %in% 0:64 ~ 0,
                                  age %in% 65:150 ~ 65)
    # INFANTS
    if(sep_infant){
      print("Selected to separate infants aged 0 from other ages 1-64.")
      age_group[age %in% 1:64] <- 1
      ages_list <- c(0,1,65)
    }else{
      print("Selected combined infant age group 0 with other ages.")
    }

  }else if(default_group %in% c("18", "18+") | default_group %in% c(18)){
    ages_list <- c(0,18)
    age_group <- dplyr::case_when(age %in% 0:17 ~ 0,
                           age %in% 18:150 ~ 18)
    # INFANTS
    if(sep_infant){
      print("Selected to separate infants aged 0 from other ages 1-17.")
      age_group[age %in% 1:17] <- 1
      ages_list <- c(0,1,18)
    }else{
      print("Selected combined infant age group 0 with other ages.")
    }
  }else{
    stop("You didn't select any existing pre-defined default grouping.")
  }
  ## CUSTOM HIGHEST AGE GROUP LOWER BOUND
  if(!is.null(max_age_lower)){
    if(!is.vector(max_age_lower)){
      stop(paste0("Incorrect format for max_age_lower. ",
                  "Provide a numeric vector of length 1"))
    }else if(!(length(max_age_lower) %in% 1)){
      stop(paste0("Incorrect format for max_age_lower. ",
                  "Provide a numeric vector of length 1"))
    }else if(!is.numeric(max_age_lower)){
      stop(paste0("Incorrect format for max_age_lower. ",
                  "Provide a numeric vector of length 1"))
    }
    print(paste0("Selected custom max lower bound for age groups, ",
                 max_age_lower))
    age_group[age %in% max_age_lower:150] <- max_age_lower
    ages_list <- unique( c(ages_list[ages_list < max_age_lower],max_age_lower))
  }
  #### FORMAT: CHARACTER RANGE
  if(format %in% c("numeric", "num", "number")){
    print("Selected numeric format for age groups, using lower bound age.")
  }else if(format %in% c("range", "character", "string", "char")){
    print("Selected range format for age groups.")


    ages_list <- sort(unique(ages_list))
    print(paste0("Your custom age group lower bounds are: ",
                 paste(ages_list, collapse = ", ")))
    age_groups_char <- age_group
    n_groups <- length(ages_list)
    for(i in 1:(n_groups)){
      if(i %in% n_groups){
        label_i <- paste0(ages_list[i], "+")
      }else{
        label_i <- paste0(ages_list[i], "-", (ages_list[i+1])-1)
      }
      age_groups_char[(age_group %in% ages_list[i])] <- label_i
    }
    print("Character range age groups complete.")
    age_group <- age_groups_char
  }
  ## COMPLETE. RETURN NEW COLUMN.
  return(age_group)
}


