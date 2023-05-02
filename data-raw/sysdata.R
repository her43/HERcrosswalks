#### PLACES FIPS CODES FROM CENSUS.GOV #########################################
place_fips_url <- paste0("https://www2.census.gov/geo/docs/reference/",
                         "codes/files/national_places.txt")

raw_place_fips <- rio::import(file = place_fips_url,
                              format = "txt",
                              sep = "|",
                              header = T)
place_fips_data <- raw_place_fips
# LOWER CASE COLUMN NAMES
colnames(place_fips_data) <- tolower(colnames(place_fips_data))
place_fips_data <- place_fips_data %>%
  dplyr::mutate(city_fips = paste0(sprintf("%02d", statefp),
                                   sprintf("%05d", placefp))) %>%
  dplyr::select(state_abbr = state,
                city_name = placename,
                city_fips,
                county_info = county)


###### RELATIONSHIP FILE FROM CENSUS BUREAU: PLACE 2010 TO 2020 ####
# CALIFORNIA:
place_rel_ca_url <- paste0("https://www2.census.gov/geo/docs/maps-data/data/",
                         "rel2020/place/tab20_place20_place10_st06.txt")
raw_place_rel_ca <- rio::import(file = place_rel_ca_url,
                              format = "txt",
                              sep = "|",
                              header = T)
place_rel_ca <- raw_place_rel_ca
# LOWER CASE COLUMN NAMES
colnames(place_rel_ca) <- tolower(colnames(place_rel_ca))
place_rel_ca <- place_rel_ca %>%
  dplyr::mutate(place_fips_20 = sprintf("%07d", geoid_place_20),
                place_fips_10 = sprintf("%07d", geoid_place_10),
                p_land_overlap_20 = round(arealand_part/arealand_place_20,5),
                p_land_overlap_10 = round(arealand_part/arealand_place_10,5)) %>%
  dplyr::select(place_fips_20,
                place_name_20 = namelsad_place_20,
                landarea_20 = arealand_place_20,
                p_land_overlap_20,
                place_fips_10,
                place_name_10 = namelsad_place_10,
                landarea_10 = arealand_place_10,
                p_land_overlap_10,
                landarea_overlap = arealand_part)

# GEORGIA:
place_rel_ga_url <- paste0("https://www2.census.gov/geo/docs/maps-data/data/",
                           "rel2020/place/tab20_place20_place10_st13.txt")
raw_place_rel_ga <- rio::import(file = place_rel_ga_url,
                                format = "txt",
                                sep = "|",
                                header = T)
place_rel_ga <- raw_place_rel_ga
# LOWER CASE COLUMN NAMES
colnames(place_rel_ga) <- tolower(colnames(place_rel_ga))
place_rel_ga <- place_rel_ga %>%
  dplyr::mutate(place_fips_20 = sprintf("%07d", geoid_place_20),
                place_fips_10 = sprintf("%07d", geoid_place_10),
                p_land_overlap_20 = round(arealand_part/arealand_place_20,5),
                p_land_overlap_10 = round(arealand_part/arealand_place_10,5)) %>%
  dplyr::select(place_fips_20,
                place_name_20 = namelsad_place_20,
                landarea_20 = arealand_place_20,
                p_land_overlap_20,
                place_fips_10,
                place_name_10 = namelsad_place_10,
                landarea_10 = arealand_place_10,
                p_land_overlap_10,
                landarea_overlap = arealand_part)


#### CENSUS DESIGNATED PLACE TO COUNTY CROSSWALKS ##############################
# SOURCE: MISSOURI.EDU GEOCORR TOOL
# https://mcdc.missouri.edu/applications/geocorr.html

#### 2020s #####################################
# 2022 VERSION:
# "Geocorr 2022 uses geography from the 2020 census and beyond."
# https://mcdc.missouri.edu/applications/geocorr2022.html

path_data_geocorr <-
  paste0("C:/Users/her43/OneDrive - Drexel University/",
         "Functional Programming Project HCUP/",
         "data/geocorr_cdp_county/")

# LOAD IN INDIVIDUAL 2022 DATA SETS, CREATE ONE "ALL"/FULL USA DATA SET
geocorr2022_files <- list.files(path = path_data_geocorr,
                                pattern = "geocorr2022")
city_county_2020_cw <- lapply(geocorr2022_files, function(i){
  headers <- read.csv(file = paste0(path_data_geocorr, i),
                      header = F, nrows = 1)
  temp  <- read.csv(file = paste0(path_data_geocorr, i),
                    header = F, skip = 2)
  colnames(temp) <- as.vector(headers)
  return(temp)
})
city_county_2020_cw <- do.call(rbind, city_county_2020_cw)
# CLEAN UP FIPS CODES AND COLUMN NAMES A BIT MORE
colnames(city_county_2020_cw) <- tolower(colnames(city_county_2020_cw))
city_county_2020_cw <- city_county_2020_cw %>%
  dplyr::mutate(city_fips = paste0(sprintf("%02d", state),
                                   sprintf("%05d", place)),
                county_fips = sprintf("%05d", county)) %>%
  dplyr::select(stab,
                city_fips,
                county_fips,
                placename,
                county_name = countyname,
                pop_overlap_2020 = pop20,
                p_city = afact,
                p_county = afact2)
# CLEAN CITY/STATE NAMING (TO MATCH CITY POP DATA FROM THE CENSUS)
city_county_2020_cw$placename <- gsub(x = city_county_2020_cw$placename,
                                      pattern = ", Moore County",
                                      replacement = "-Moore County")
city_county_2020_cw$placename <- gsub(x = city_county_2020_cw$placename,
                                      pattern = ", Village of",
                                      replacement = " Village of")
city_county_2020_cw[c('city_name', 'state_abbr')] <-
  stringr::str_split_fixed(city_county_2020_cw$placename, ', ', 2)
city_county_2020_cw$state_abbr <- gsub(x = city_county_2020_cw$state_abbr,
                                       pattern = " ",
                                       replacement = "")
city_county_2020_cw$state_abbr <-
  ifelse(is.na(city_county_2020_cw$state_abbr),
         city_county_2020_cw$stab,
         ifelse(city_county_2020_cw$state_abbr %in% c(""),
                city_county_2020_cw$stab,
                city_county_2020_cw$state_abbr))
# Coeur dAlene city
city_county_2020_cw$city_name <- gsub(x = city_county_2020_cw$city_name,
                                      pattern = "Coeur dAlene",
                                      replacement = "Coeur d'Alene")
# Lees Summit city
city_county_2020_cw$city_name <- gsub(x = city_county_2020_cw$city_name,
                                      pattern = "Lees Summit",
                                      replacement = "Lee's Summit")
# OFallon city
city_county_2020_cw$city_name <- gsub(x = city_county_2020_cw$city_name,
                                      pattern = "OFallon",
                                      replacement = "O'Fallon")
# FINAL COLUMNS
city_county_2020_cw <-
  city_county_2020_cw[,c("city_name", "state_abbr", "city_fips",
                         "county_name", "county_fips",
                         "p_county", "p_city", "pop_overlap_2020")]
#### 2010s #################################################
geocorr2014_files <- list.files(path = path_data_geocorr,
                                pattern = "geocorr2014")
geocorr2014_files <- geocorr2014_files[grepl(x = geocorr2014_files,
                                             pattern = "nonames")]
city_county_2010_cw <- lapply(geocorr2014_files, function(i){
  headers <- read.csv(file = paste0(path_data_geocorr, i),
                      header = F, nrows = 1)
  temp  <- read.csv(file = paste0(path_data_geocorr, i),
                    header = F, skip = 2)
  colnames(temp) <- as.vector(headers)
  return(temp)
})
city_county_2010_cw <- do.call(rbind, city_county_2010_cw)
# CLEAN UP FIPS CODES AND COLUMN NAMES A BIT MORE
colnames(city_county_2010_cw) <- tolower(colnames(city_county_2010_cw))
city_county_2010_cw <- city_county_2010_cw %>%
  dplyr::mutate(city_fips = paste0(sprintf("%02d", state),
                                   sprintf("%05d", placefp)),
                county_fips = sprintf("%05d", county)) %>%
  dplyr::select(city_fips, county_fips,
                p_city = afact,
                p_county = afact2,
                pop_overlap_2010 = pop10)

###### MERGE CLEANED 2020 DATA CITY NAMES TO 2010 DATA ####
city_county_2010_cw <-
  merge(city_county_2010_cw,
        unique(city_county_2020_cw[,c("city_name", "state_abbr", "city_fips")]),
        by = "city_fips",
        all.x = T)
# SOME ARE STILL MISSING, SO ALSO MERGE place_fips_data TO 2010 CW
city_county_2010_cw <-
  merge(city_county_2010_cw,
        unique(place_fips_data[,c("city_name", "state_abbr", "city_fips")]),
        by = "city_fips",
        all.x = T,
        suffixes = c(".2020", ".census"))
# CHECK TO SEE WHETHER A CITY HAS A 2020 NAME, CENSUS DATA NAME, OR BOTH
city_county_2010_cw$has_2020_name <- !is.na(city_county_2010_cw$city_name.2020)
city_county_2010_cw$has_census_name <- !is.na(city_county_2010_cw$city_name.census)
print(table(has_2020_name = city_county_2010_cw$has_2020_name,
            has_census_name = city_county_2010_cw$has_census_name,
            useNA = "ifany"))
# CHECK ALSO IF THE NAMES FROM THE 2 DIFFERENT SOURCES (2020 AND CENSUS) MATCH
city_county_2010_cw$match_names <-
  ifelse(city_county_2010_cw$has_2020_name %in% T,
         ifelse(city_county_2010_cw$has_census_name %in% T,
                ifelse(city_county_2010_cw$has_2020_name ==
                         city_county_2010_cw$has_census_name,
                       "Yes.", "No."),
                "Only 2020 name."),
         ifelse(city_county_2010_cw$has_census_name %in% T,
                "Only census name.",
                "No name."))
print(table(city_county_2010_cw$match_names, useNA = "ifany"))
#   Only 2020 name. Only census name.              Yes.
#              3066               274             30067

# FORTUNATELY, THERE ARE NO MISMATCHES OR MISSING NAMES AFTER CONSIDERING
# BOTH SOURCES.
# ASSIGN A CITY NAME BASED ON WHICHEVER IS AVAILABLE.
city_county_2010_cw$city_name <-
  ifelse(city_county_2010_cw$match_names %in%
           c("Yes.","Only 2020 name."),
         city_county_2010_cw$city_name.2020,
         city_county_2010_cw$city_name.census)
# ALSO ASSIGN THE STATE NAME
city_county_2010_cw$state_abbr <-
  ifelse(is.na(city_county_2010_cw$state_abbr.2020),
         city_county_2010_cw$state_abbr.census,
         city_county_2010_cw$state_abbr.2020)
print(table(is.na(city_county_2010_cw$city_name))) # FALSE ONLY
print(table(is.na(city_county_2010_cw$state_abbr))) # FALSE ONLY

###### MERGE CLEANED 2020 DATA COUNTY NAMES TO 2010 DATA ######
city_county_2010_cw <-
  merge(city_county_2010_cw,
        unique(city_county_2020_cw[,c("county_name", "county_fips")]),
        by = "county_fips",
        all.x = T)
# CHECK DATA ROWS THAT HAVE NA COUNTY_NAME
missing_county_2010 <-
  city_county_2010_cw[is.na(city_county_2010_cw$county_name),
                      c("county_fips", "city_fips","city_name",
                        "state_abbr", "county_name")]
print(unique(missing_county_2010$county_fips))
# [1] "02261" "02270" "46113" "51515"
place_fips_for_na_county <- place_fips_data[place_fips_data$city_fips %in%
                                              missing_county_2010$city_fips,]
check_missing_counties_names <- merge(missing_county_2010,
                                      place_fips_for_na_county,
                                      by = c("city_fips"))
unique_miss_county_names <-
  unique(check_missing_counties_names[,c("county_fips", "county_info")])
print(unique_miss_county_names)
#    county_fips                county_info
# 1        02270   Wade Hampton Census Area
# 2        02261 Valdez-Cordova Census Area
# 38       46113             Shannon County
# 45       51515               Bedford city
# THERE ARE ONLY 4 COUNTIES WITH NO 2020 NAME, BUT ALL THESE COUNTIES
# COME UP IN THE PLACE_FIPS_DATA FROM THE CENSUS.
# ADD IN THESE 4 NAMES VIA MERGE
city_county_2010_cw <- merge(city_county_2010_cw,
                             unique_miss_county_names,
                             by = "county_fips",
                             all.x = T)
city_county_2010_cw$county_name <-
  ifelse(!is.na(city_county_2010_cw$county_name ),
         city_county_2010_cw$county_name,
         city_county_2010_cw$county_info)
print(table(is.na(city_county_2010_cw$county_name))) # FALSE ONLY

city_county_2010_cw <-
  city_county_2010_cw[,c("city_name", "state_abbr", "city_fips", "county_name",
                        "county_fips", "p_county", "p_city",
                        "pop_overlap_2010")]


########################################################
#### DOWNLOAD CENSUS DATA SETS: CITIES LARGER THAN 50K ####

#### 2020s ####
# WEBSITE:
# https://www.census.gov/data/tables/time-series/demo/popest/2020s-total-cities-and-towns.html
# DATA SET: "Annual Estimates of the Resident Population for Incorporated
# Places of 50,000 or More, Ranked by July 1, 2021 Population:
# April 1, 2020 to July 1, 2021 (SUB-IP-EST2021-ANNRNK) [< 1.0 MB]"
# DOWNLOAD URL
city_2020s_url <-
  paste0("https://www2.census.gov/programs-surveys/popest/",
         "tables/2020-2021/cities/totals/SUB-IP-EST2021-ANNRNK.xlsx")

# IMPORT
library(readxl)
library(rio)
city_2020s_temp_headers <- rio::import(file = city_2020s_url,
                                      format = "xlsx",
                                      col_names = F,
                                      skip = 1, n_max = 3)
city_2020s_pop_raw <- rio::import(file = city_2020s_url,
                                 format = "xlsx",
                                 col_names = F,
                                 skip = 4)
city_2020s_pop_headers <-
  do.call(c, c(city_2020s_temp_headers[2,1],
               city_2020s_temp_headers[2,2],
               city_2020s_temp_headers[2,3],
               city_2020s_temp_headers[3,4:ncol(city_2020s_temp_headers)]))
names(city_2020s_pop_headers) <- c()
city_2020s_pop_headers[3] <- "estimates_base_2020"
city_2020s_pop_headers <- tolower(city_2020s_pop_headers)
city_2020s_pop_headers <- gsub(x = city_2020s_pop_headers,
                              pattern = " ",
                              "_")
city_2020s_pop <- city_2020s_pop_raw
colnames(city_2020s_pop) <- city_2020s_pop_headers
city_2020s_pop <-
  city_2020s_pop[!is.na(city_2020s_pop$geographic_area),]

#### 2010s ####
# WEBSITE:
# https://www.census.gov/data/tables/time-series/demo/popest/2010s-total-cities-and-towns.html
# DATA SET: "Annual Estimates of the Resident Population for Incorporated
# Places of 50,000 or More, Ranked by July 1, 2019 Population:
# April 1, 2010 to July 1, 2019 [< 1.0 MB]"
# DOWNLOAD URL
city_2010s_url <-
  paste0("https://www2.census.gov/programs-surveys/popest/",
         "tables/2010-2019/cities/totals/SUB-IP-EST2019-ANNRNK.xlsx")

# IMPORT
city_2010s_temp_headers <- rio::import(file = city_2010s_url,
                                      format = "xlsx",
                                      col_names = F,
                                      skip = 1, n_max = 3)
city_2010s_pop_raw <- rio::import(file = city_2010s_url,
                                 format = "xlsx",
                                 col_names = F,
                                 skip = 4)
city_2010s_pop_headers <-
  do.call(c, c(city_2010s_temp_headers[2,1],
               city_2010s_temp_headers[2,2],
               city_2010s_temp_headers[3,3],
               city_2010s_temp_headers[3,4],
               city_2010s_temp_headers[3,5:ncol(city_2010s_temp_headers)]))
names(city_2010s_pop_headers) <- c()
city_2010s_pop_headers[3:4] <- paste0(city_2010s_pop_headers[3:4], "_2010")
city_2010s_pop_headers <- tolower(city_2010s_pop_headers)
city_2010s_pop_headers <- gsub(x = city_2010s_pop_headers,
                              pattern = " ",
                              "_")
city_2010s_pop <- city_2010s_pop_raw
colnames(city_2010s_pop) <- city_2010s_pop_headers
city_2010s_pop <-
  city_2010s_pop[!is.na(city_2010s_pop$geographic_area),]
#### COMBINE 2020s AND 2010S CITY POP DATA ####
city_pop_by_year <- merge(city_2010s_pop[,2:ncol(city_2010s_pop)],
                         city_2020s_pop[,2:ncol(city_2020s_pop)],
                         by = "geographic_area",
                         all = T)

######  ADD A STATE COLUMN IN THE CITY POP BY YEAR DATA ################

# MAKE A NEW COLUMN FOR STATE NAME
# AND ELIMINATE THE OLD STATE NAME IN NEW PLACE NAME
city_pop_by_year[c('city_name', 'state_name')] <-
  stringr::str_split_fixed(city_pop_by_year$geographic_area, ', ', 2)
city_pop_by_year <- city_pop_by_year %>%
  dplyr::mutate(state_abbr =
                  dplyr::case_when(
                    state_name %in% "Alabama" ~ "AL",
                    state_name %in% "Alaska" ~ "AK",
                    state_name %in% "Arizona" ~ "AZ",
                    state_name %in% "Arkansas" ~ "AR",
                    state_name %in% "California" ~ "CA",
                    state_name %in% "Colorado" ~ "CO",
                    state_name %in% "Connecticut" ~ "CT",
                    state_name %in% "Delaware" ~ "DE",
                    state_name %in% "District of Columbia" ~ "DC",
                    state_name %in% "Florida" ~ "FL",
                    state_name %in% "Georgia" ~ "GA",
                    state_name %in% "Hawaii" ~ "HI",
                    state_name %in% "Idaho" ~ "ID",
                    state_name %in% "Illinois" ~ "IL",
                    state_name %in% "Indiana" ~ "IN",
                    state_name %in% "Iowa" ~ "IA",
                    state_name %in% "Kansas" ~ "KS",
                    state_name %in% "Kentucky" ~ "KY",
                    state_name %in% "Louisiana" ~ "LA",
                    state_name %in% "Maine" ~ "ME",
                    state_name %in% "Maryland" ~ "MD",
                    state_name %in% "Massachusetts" ~ "MA",
                    state_name %in% "Michigan" ~ "MI",
                    state_name %in% "Minnesota" ~ "MN",
                    state_name %in% "Mississippi" ~ "MS",
                    state_name %in% "Missouri" ~ "MO",
                    state_name %in% "Montana" ~ "MT",
                    state_name %in% "Nebraska" ~ "NE",
                    state_name %in% "Nevada" ~ "NV",
                    state_name %in% "New Hampshire" ~ "NH",
                    state_name %in% "New Jersey" ~ "NJ",
                    state_name %in% "New Mexico" ~ "NM",
                    state_name %in% "New York" ~ "NY",
                    state_name %in% "North Carolina" ~ "NC",
                    state_name %in% "North Dakota" ~ "ND",
                    state_name %in% "Ohio" ~ "OH",
                    state_name %in% "Oklahoma" ~ "OK",
                    state_name %in% "Oregon" ~ "OR",
                    state_name %in% "Pennsylvania" ~ "PA",
                    state_name %in% "Rhode Island" ~ "RI",
                    state_name %in% "South Carolina" ~ "SC",
                    state_name %in% "South Dakota" ~ "SD",
                    state_name %in% "Tennessee" ~ "TN",
                    state_name %in% "Texas" ~ "TX",
                    state_name %in% "Utah" ~ "UT",
                    state_name %in% "Vermont" ~ "VT",
                    state_name %in% "Virginia" ~ "VA",
                    state_name %in% "Washington" ~ "WA",
                    state_name %in% "West Virginia" ~ "WV",
                    state_name %in% "Wisconsin" ~ "WI",
                    state_name %in% "Wyoming" ~ "WY",
                    state_name %in% "Puerto Rico" ~ "PR",
                    T ~ ""))

####### CHECK CITY NAMES ACROSS DATA SETS #########################
# CHECK THAT ALL THE CITIES THAT EXIST IN THE POP DATA FOR THE 2010s
# ALSO EXIST IN THE 2010 CITY-COUNTY CROSSWALK

unique_2010pop_cities <-
  unique(city_pop_by_year[,c("city_name", "state_abbr"),
                          (!is.na(city_pop_by_year$`2010`)) |
                            (!is.na(city_pop_by_year$`2011`)) |
                            (!is.na(city_pop_by_year$`2012`)) |
                            (!is.na(city_pop_by_year$`2013`)) |
                            (!is.na(city_pop_by_year$`2014`)) |
                            (!is.na(city_pop_by_year$`2015`)) |
                            (!is.na(city_pop_by_year$`2016`)) |
                            (!is.na(city_pop_by_year$`2017`)) |
                            (!is.na(city_pop_by_year$`2018`)) |
                            (!is.na(city_pop_by_year$`2019`)) ] )
unique_2010pop_cities <- paste0(unique_2010pop_cities$city_name,
                              ", ",
                              unique_2010pop_cities$state_abbr)
unique_2010cw_cities <-
  unique(city_county_2010_cw[,c("city_name", "state_abbr")])

unique_2010cw_cities <- paste0(unique_2010cw_cities$city_name,
                                ", ",
                               unique_2010cw_cities$state_abbr)
print(table(unique_2010pop_cities %in% unique_2010cw_cities, useNA = "ifany"))
# FALSE  TRUE
#     5   807

missing_cities_2010 <- unique_2010pop_cities[!(unique_2010pop_cities %in%
                                                 unique_2010cw_cities)]
# "Brookhaven city, GA"
# "Jurupa Valley city, CA"
# "Macon-Bibb County, GA"
# "South Fulton city, GA"
# "Stonecrest city, GA"
# WHICH YEARS DO THESE CITIES HAVE POPULATION DATA?
check_missing_cities_years <-
  city_pop_by_year[(city_pop_by_year$city_name %in%
                     c("Brookhaven city",
                       "Macon-Bibb County",
                       "South Fulton city",
                       "Stonecrest city"   ) &
                     city_pop_by_year$state_abbr %in% "GA") |
                     (city_pop_by_year$city_name %in%
                        "Jurupa Valley city" &
                        city_pop_by_year$state_abbr %in% "CA"),]
# ALL YEARS APPARENTLY.

# ARE THESE 5 CITIES IN THE 2020 CITY-COUNTY CROSSWALK?
temp_miss_city <-
  city_county_2020_cw[(city_county_2020_cw$city_name %in%
                         c("Brookhaven city",
                           "Macon-Bibb County",
                           "South Fulton city",
                           "Stonecrest city"   ) &
                         city_county_2020_cw$state_abbr %in% "GA") |
                        (city_county_2020_cw$city_name %in%
                           "Jurupa Valley city" &
                           city_county_2020_cw$state_abbr %in% "CA"),]
print(temp_miss_city)
# YES, ALL:
#               city_name state_abbr city_fips  county_name county_fips p_county p_city pop_overlap_2020
# 2986 Jurupa Valley city         CA   0637692 Riverside CA       06065   0.0434      1           105053
# 5969    Brookhaven city         GA   1310944    DeKalb GA       13089   0.0722      1            55161
# 6281  Macon-Bibb County         GA   1349008      Bibb GA       13021   1.0000      1           157346
# 6480  South Fulton city         GA   1372122    Fulton GA       13121   0.1007      1           107436
# 6491    Stonecrest city         GA   1373784    DeKalb GA       13089   0.0774      1            59194
# CHECK 2010 CROSSWALK FOR SIMILAR CITY NAMES IN THE SAME COUNTIES

############### JURUPA VALLEY CITY, CA: ##################
# NOT IN 2010 CROSSWALK.
# INFO ONLINE:
# - https://www.jurupavalley.org/309/About-Us
#     "The City of Jurupa Valley was incorporated on July 1, 2011"

# FROM THE CENSUS RELATIONSHIP FILE FOR PLACE 2010-2020:
jurupa_info <- place_rel_ca[grepl(x = place_rel_ca$place_name_20,
                                  pattern = "jurupa",
                                  ignore.case = T),]
print(jurupa_info[, c("place_fips_20", "place_name_20", "p_land_overlap_20",
                      "place_fips_10", "place_name_10","p_land_overlap_10")])
#      place_fips_20      place_name_20 p_land_overlap_20 place_fips_10         place_name_10 p_land_overlap_10
# 1361       0637692 Jurupa Valley city           0.24422            NA                                      NA
# 1362       0637692 Jurupa Valley city           0.00670       0617204 Crestmore Heights CDP           1.00000
# 1363       0637692 Jurupa Valley city           0.18859       0629644         Glen Avon CDP           1.00000
# 1364       0637692 Jurupa Valley city           0.18576       0647976         Mira Loma CDP           0.99602
# 1365       0637692 Jurupa Valley city           0.00178       0651560            Norco city           0.00549
# 1366       0637692 Jurupa Valley city           0.11693       0656350            Pedley CDP           0.98597
# 1367       0637692 Jurupa Valley city           0.00005       0662000        Riverside city           0.00003
# 1368       0637692 Jurupa Valley city           0.22165       0663260          Rubidoux CDP           0.98498
# 1369       0637692 Jurupa Valley city           0.03432       0676022        Sunnyslope CDP           1.00000

# JURUPA VALLEY CITY WAS ESSENTIALLY CREATED FROM:
# - 0617204 Crestmore Heights CDP
# - 0629644         Glen Avon CDP
# - 0647976         Mira Loma CDP
# - 0656350            Pedley CDP
# - 0663260          Rubidoux CDP
# - 0676022        Sunnyslope CDP
# - WITH A LITTLE MORE FROM UNKNOWN SOURCES.

# ARE THESE 2010 CDPs IN THE 2010 CROSSWALK?
temp_jurupa_equiv_cities <-
  city_county_2010_cw[city_county_2010_cw$city_fips %in%
                         c("0617204","0629644","0647976",
                           "0656350","0663260","0676022") ,]
print(temp_jurupa_equiv_cities)
#                 city_name state_abbr city_fips  county_name county_fips p_county p_city pop_overlap_2010
#3042            Pedley CDP         CA   0656350 Riverside CA       06065   0.0058      1            12672
#3055 Crestmore Heights CDP         CA   0617204 Riverside CA       06065   0.0002      1              384
#3082        Sunnyslope CDP         CA   0676022 Riverside CA       06065   0.0024      1             5153
#3091         Glen Avon CDP         CA   0629644 Riverside CA       06065   0.0092      1            20199
#3097         Mira Loma CDP         CA   0647976 Riverside CA       06065   0.0100      1            21930
#3108          Rubidoux CDP         CA   0663260 Riverside CA       06065   0.0157      1            34280

# EXAMINE THE "NOT A PLACE" ROWS FOR RIVERSIDE CA IN 2010 AND 2020 AND ALSO
# IN RELATIONSHIP FILE.
# AND ARE THERE ANY OTHER REMAINING "NOT A PLACE"/UNINCORPORATED AREAS OF
# RIVERSIDE COUNTY?
temp_riverside_2020 <-
  city_county_2020_cw[city_county_2020_cw$county_name %in% "Riverside CA",]
print(temp_riverside_2020)
temp_riverside_2010 <-
  city_county_2010_cw[city_county_2010_cw$county_name %in% "Riverside CA",]
print(temp_riverside_2010[order(temp_riverside_2010$city_name),])
riverside_na_info <- place_rel_ca[place_rel_ca$place_name_10 %in% "" &
                                    place_rel_ca$place_name_20 %in%
                                    temp_riverside_2020$city_name,]
# THERE IS A LOT OF OVERLAP BETWEEN "NOT IN A PLACE" AREAS IN 2010
# AND NAMED CDPS OR INCORPORATED CITIES IN 2020.
# --> WE CANNOT JUST ADD ALL OF THE "NOT IN A PLACE" POPULATION TO
# JURUPA VALLEY CITY.

# WE CAN CREATE A NEW LINE OF DATA IN THE 2010 CROSSWALK FOR JURUPA VALLEY CITY
# BY AGGREGATING THE 6 USEFUL CDPs. (SINCE p_city = 1 FOR ALL CDPs)
new2010row_jurupa <- temp_jurupa_equiv_cities %>%
  dplyr::mutate(city_name = "Jurupa Valley city",
                state_abbr = "CA",
                city_fips = "0637692") %>%
  dplyr::group_by(city_name, state_abbr, city_fips,
                  county_name, county_fips) %>%
  dplyr::summarize(p_county = sum(p_county),
                   pop_overlap_2010 = sum(pop_overlap_2010)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(p_city = 1) %>%
  select(city_name, state_abbr, city_fips, county_name, county_fips, p_county,
         p_city, pop_overlap_2010) %>%
  as.data.frame()
#            city_name state_abbr city_fips  county_name county_fips p_county p_city pop_overlap_2010
# 1 Jurupa Valley city         CA   0637692 Riverside CA       06065   0.0433      1            94618

############### BROOKHAVEN CITY, GA: ##################
# CITY:   Brookhaven city GA   1310944
# COUNTY: DeKalb GA    13089

# NOT IN 2010 CROSSWALK.
# INFO ONLINE:
# - https://www.brookhavenga.gov/community/page/city-brookhaven-ga
#     "Brookhaven is a city in DeKalb County, Ga., in the metro Atlanta area.
#      Brookhaven officially became a city in 2012. The city stretches over
#      12 square miles with a population of about 55,000. "

# FROM THE CENSUS RELATIONSHIP FILE FOR PLACE 2010-2020:
brookhaven_info <- place_rel_ga[grepl(x = place_rel_ga$place_name_20,
                                  pattern = "brookhaven",
                                  ignore.case = T),]
print(brookhaven_info[, c("place_fips_20", "place_name_20", "p_land_overlap_20",
                      "place_fips_10", "place_name_10","p_land_overlap_10")])
#     place_fips_20   place_name_20 p_land_overlap_20 place_fips_10         place_name_10 p_land_overlap_10
# 150       1310944 Brookhaven city           0.31036            NA                                      NA
# 151       1310944 Brookhaven city           0.00002       1304000          Atlanta city           0.00000
# 152       1310944 Brookhaven city           0.00207       1315172         Chamblee city           0.00767
# 153       1310944 Brookhaven city           0.65200       1356000     North Atlanta CDP           0.99822
# 154       1310944 Brookhaven city           0.03555       1356168 North Druid Hills CDP           0.08229

# BROOKHAVEN CITY WAS ESSENTIALLY CREATED FROM:
# - 1356000     North Atlanta CDP (ABOUT 2/3 OF BROOKHAVEN LAND AREA)
# - WITH  1/3 MORE FROM UNKNOWN SOURCES IN DEKALB COUNTY GA.

# EXAMINE THE "NOT A PLACE" ROWS FOR DEKALB COUNTY CA IN 2010 AND 2020 AND ALSO
# IN RELATIONSHIP FILE.
# AND ARE THERE ANY OTHER REMAINING "NOT A PLACE"/UNINCORPORATED AREAS OF
# DEKALB COUNTY?
temp_dekalb_2020 <-
  city_county_2020_cw[city_county_2020_cw$county_name %in% "DeKalb GA",]
print(temp_dekalb_2020)
temp_dekalb_2010 <-
  city_county_2010_cw[city_county_2010_cw$county_name %in% "DeKalb GA",]
print(temp_dekalb_2010[order(temp_dekalb_2010$city_name),])
dekalb_na_info <- place_rel_ga[place_rel_ga$place_name_10 %in% "" &
                                 place_rel_ga$place_name_20 %in%
                                 temp_dekalb_2020$city_name,]
print(dekalb_na_info)
# THERE ARE TOO MANY AREAS IN 2020 THAT OVERLAP WITH THE "NOT A PLACE"
# AREAS OF DEKALB COUNTY IN 2010. NO CLEAN WAY TO MATCH THESE.

# IS THE North Atlanta CDP IN THE 2010 CROSSWALK?
temp_brookhaven_equiv_cities <-
  city_county_2010_cw[city_county_2010_cw$city_fips %in% "1356000",]
print(temp_brookhaven_equiv_cities)
#              city_name state_abbr city_fips county_name
# 5748 North Atlanta CDP         GA   1356000   DeKalb GA
#      county_fips p_county p_city pop_overlap_2010
# 5748       13089   0.0585      1            40456
# WE CAN CREATE A NEW LINE OF DATA IN THE 2010 CROSSWALK TO APPROXIMATE
# BROOKHAVEN CITY BY RENAMEING THIS 1 CDP. (NOTE: p_city = 1)
new2010row_brookhaven <- temp_brookhaven_equiv_cities %>%
  dplyr::mutate(city_name = "Brookhaven city",
                city_fips = "1310944") %>%
  as.data.frame()
print(new2010row_brookhaven)
#            city_name state_abbr city_fips county_name county_fips
# 5748 Brookhaven city         GA   1310944   DeKalb GA       13089
#      p_county p_city pop_overlap_2010
# 5748   0.0585      1            40456

############### MACON-BIBB COUNTY, GA: ##################
# CITY:   Macon-Bibb County GA   1349008
# COUNTY: Bibb GA  13021
# % OVERLAP: 100%

# 2010 CROSSWALK:
# - Macon city GA, FIPS: 1349000, POP: ABOUT 91,000
#   >99% IN BIBB COUNTY, p_county = 58%
# - OTHER CITIES/CDPS IN BIBB COUNTY
#   - "not in a place" - 41% OF THE COUNTY
#   - Payne city GA - p_city = 1.00 (100%), p_county = 0.0014 (0.14%), pop = 218
# - THERE IS ALSO A COUNTY CALLED MACON COUNTY, UNRELATED

# INFO ONLINE:
# - https://www.maconbibb.us/tag/consolidation/
#     "Macon and Bibb County consolidated and became Macon-Bibb County"

# FROM THE CENSUS RELATIONSHIP FILE FOR PLACE 2010-2020:
maconbibb_info <- place_rel_ga[grepl(x = place_rel_ga$place_name_20,
                                      pattern = "macon",
                                      ignore.case = T),]
print(maconbibb_info[,c("place_fips_20", "place_name_20", "p_land_overlap_20",
                        "place_fips_10", "place_name_10","p_land_overlap_10")])
#     place_fips_20     place_name_20 p_land_overlap_20 place_fips_10 place_name_10 p_land_overlap_10
# 659       1349008 Macon-Bibb County           0.77773            NA                              NA
# 660       1349008 Macon-Bibb County           0.22210       1349000    Macon city           0.99337
# 661       1349008 Macon-Bibb County           0.00017       1359584    Payne city           1.00000

# ANYTHING ELSE LEFT IN BIBB COUNTY THAT'S NOT IN MACON-BIBB IN 2020?
print(city_county_2020_cw[city_county_2020_cw$county_name %in% "Bibb GA",])
#              city_name state_abbr city_fips county_name county_fips p_county p_city pop_overlap_2020
# 6281 Macon-Bibb County         GA   1349008     Bibb GA       13021        1      1           157346

# --> IT'S LITERALLY THE WHOLE COUNTY.

# PULL EVERYTHING FROM BIBB COUNTY GA FROM 2010 CROSSWALK
temp_maconbibb_equiv_cities <-
  city_county_2010_cw[city_county_2010_cw$county_name %in% "Bibb GA",]
print(temp_maconbibb_equiv_cities)
#             city_name state_abbr city_fips county_name
# 5571       Payne city         GA   1359584     Bibb GA
# 5572       Macon city         GA   1349000     Bibb GA
# 5573 [not in a place]         GA   1399999     Bibb GA
#      county_fips p_county p_city pop_overlap_2010
# 5571       13021   0.0014 1.0000              218
# 5572       13021   0.5843 0.9949            90885
# 5573       13021   0.4143 0.0123            64444
# WE CAN CREATE A NEW LINE OF DATA IN THE 2010 CROSSWALK TO APPROXIMATE
# MACON-BIBB COUNTY (CONSOLIDATED CITY-COUNTY) BY AGGREGATING THE DATA
# FOR THESE ROWS.
new2010row_maconbibb <- temp_maconbibb_equiv_cities %>%
  dplyr::mutate(city_name = "Macon-Bibb County",
                city_fips = "1349008") %>%
  dplyr::group_by(city_name, state_abbr, city_fips,
                  county_name, county_fips) %>%
  dplyr::summarize(p_county = sum(p_county),
                   pop_overlap_2010 = sum(pop_overlap_2010)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(p_city = 1) %>%
  select(city_name, state_abbr, city_fips, county_name, county_fips, p_county,
         p_city, pop_overlap_2010) %>%
  as.data.frame()
print(new2010row_maconbibb)
#           city_name state_abbr city_fips county_name
# 1 Macon-Bibb County         GA   1349008     Bibb GA
#   county_fips p_county p_city pop_overlap_2010
# 1       13021        1      1           155547

############### SOUTH FULTON, GA: ##################
# CITY:   South Fulton city GA   1372122
# COUNTY: Fulton GA  13121

# NOT IN 2010 CROSSWALK. FULTON COUNTY HAS A LOT OF CITIES, INCLUDING ATLANTA.
# INFO ONLINE:
# - https://www.knowatlanta.com/counties/fulton/south-fulton
#     "South Fulton elected its first mayor in March 2017 after the Georgia
#      General Assembly voted in 2016 to approve the creation of this new city."

# FROM THE CENSUS RELATIONSHIP FILE FOR PLACE 2010-2020:
southfulton_info <- place_rel_ga[grepl(x = place_rel_ga$place_name_20,
                                      pattern = "south fulton",
                                      ignore.case = T),]
print(southfulton_info[, c("place_fips_20", "place_name_20",
                           "p_land_overlap_20",
                           "place_fips_10", "place_name_10",
                           "p_land_overlap_10")])
#     place_fips_20     place_name_20 p_land_overlap_20 place_fips_10
# 975       1372122 South Fulton city           0.99773            NA
# 976       1372122 South Fulton city           0.00018       1304000
# 977       1372122 South Fulton city           0.00011       1317776
# 978       1372122 South Fulton city           0.00006       1325720
# 979       1372122 South Fulton city           0.00025       1328380
# 980       1372122 South Fulton city           0.00000       1358884
# 981       1372122 South Fulton city           0.00167       1378324
#         place_name_10 p_land_overlap_10
# 975                                  NA
# 976      Atlanta city           0.00012
# 977 College Park city           0.00089
# 978   East Point city           0.00032
# 979     Fairburn city           0.00127
# 980     Palmetto city           0.00000
# 981   Union City city           0.00751

# >99% OF SOUTH FULTON CITY CAME FROM THE UNINCORPORATED AREAS OF FULTON COUNTY
# COMPARE FULTON COUNTY PLACES IN 2010 VS 2020 - ANY OTHER DIFFERENCES BESIDES
# SOUTH FULTON?
# AND ARE THERE ANY OTHER REMAINING "NOT A PLACE"/UNINCORPORATED AREAS OF
# FULTON COUNTY?
print(city_county_2020_cw[city_county_2020_cw$county_name %in% "Fulton GA",])
temp_fulton_2010 <-
  city_county_2010_cw[city_county_2010_cw$county_name %in% "Fulton GA",]
print(temp_fulton_2010[order(temp_fulton_2010$city_name),])
# NO, ALL OTHER CITY NAMES ARE THE SAME AND p_county VALUES ARE SIMILAR FROM
# 2010 TO 2020.
# ONLY 889 POPULATION IN REMAINING UNINCORPORATED/"NOT A PLACE" LOCATIONS
# IN FULTON COUNTY.

# BEST APPROXIMATION FOR SOUTH FULTON, GA:
# PULL THE "NOT A PLACE" AREA OF FULTON COUNTY AND RE-NAME.
temp_southfulton_equiv_cities <-
  city_county_2010_cw[city_county_2010_cw$county_name %in% "Fulton GA" &
                        city_county_2010_cw$city_fips %in% "1399999",]
print(temp_southfulton_equiv_cities)
#             city_name state_abbr city_fips county_name county_fips p_county
# 5864 [not in a place]         GA   1399999   Fulton GA       13121    0.095
#      p_city pop_overlap_2010
# 5864 0.0167            87478
# South Fulton city GA   1372122
new2010row_southfulton <-
  temp_southfulton_equiv_cities %>%
  dplyr::mutate(city_name = "South Fulton city",
                city_fips = "1372122") %>%
  as.data.frame()
print(new2010row_southfulton)
#              city_name state_abbr city_fips county_name county_fips
# 5864 South Fulton city         GA   1372122   Fulton GA       13121
#      p_county p_city pop_overlap_2010
# 5864    0.095 0.0167            87478


############### STONECREST CITY, GA: ##################
# CITY:   Stonecrest city         GA   1373784
# COUNTY: DeKalb GA       13089

# NOT IN 2010 CROSSWALK. MANY CDPS AND CITIES IN THIS COUNTY.
# INFO ONLINE: NOTHING USEFUL, OTHER THAN WIKIPEDIA

# FROM THE CENSUS RELATIONSHIP FILE FOR PLACE 2010-2020:
stonecrest_info <- place_rel_ga[grepl(x = place_rel_ga$place_name_20,
                                      pattern = "stonecrest",
                                      ignore.case = T),]
print(stonecrest_info[,c("place_fips_20", "place_name_20", "p_land_overlap_20",
                         "place_fips_10", "place_name_10","p_land_overlap_10")])
#     place_fips_20   place_name_20 p_land_overlap_20 place_fips_10 place_name_10 p_land_overlap_10
# 996       1373784 Stonecrest city           0.95751            NA                              NA
# 997       1373784 Stonecrest city           0.00004       1346860 Lithonia city           0.00162
# 998       1373784 Stonecrest city           0.04245       1363952     Redan CDP           0.16410

# >95% OF STONECREST CITY CAME FROM THE "NOT A PLACE" AREAS OF DEKALB COUNTY
# COMPARE DEKALB COUNTY PLACES IN 2010 VS 2020 - ANY OTHER DIFFERENCES?
# HOW DOES THE REMAINING "NOT A PLACE" AREAS OF THE COUNTY COMPARE 2010 VS 2020?

# THERE IS STILL A LOT OF "NOT IN A PLACE" AREA IN 2020. SO, WE CAN TELL THAT
# ONLY A PORTION OF THE 2010 "NOT IN A PLACE" REMAINING PORTION OF DEKALB
# COUNTY GOT INCORPORATED.

# THERE'S NO CLEAN WAY TO AGGREGATE 2010 DATA FOR STONECREST CITY.
# SOLUTION: USE 2020 CROSSWALK DATA.
new2010row_stonecrest <-
  city_county_2020_cw %>%
  dplyr::filter(city_fips %in% "1373784") %>%
  dplyr::rename(pop_overlap_2010 = pop_overlap_2020) %>%
  as.data.frame()
print(new2010row_stonecrest)
#         city_name state_abbr city_fips county_name county_fips p_county p_city pop_overlap_2010
# 1 Stonecrest city         GA   1373784   DeKalb GA       13089   0.0774      1            59194

##########################################################################
#### ADD NEW 2010 ROWS FOR "NEW" CITIES TO 2010 CITY-COUNTY CROSSWALK
city_county_2010_cw <- rbind(city_county_2010_cw,
                             new2010row_brookhaven,
                             new2010row_jurupa,
                             new2010row_maconbibb,
                             new2010row_southfulton,
                             new2010row_stonecrest)
########################################
### SAVE DATA
usethis::use_data(city_county_2020_cw,
                  city_county_2010_cw,
                  city_pop_by_year,
                  overwrite = TRUE,
                  internal = T)
