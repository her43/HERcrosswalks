#' ACS B-table variable list creation
#'
#' Quickly create a list of ACS variable names in a format that is
#' compatible with the tidycensus package.
#'
#' @param var_num vector of numbers between 1 and 999, each representing a
#'  3-digit variable number within the ACS table.
#'
#' @return A vector of ACS variable names
#' @examples
#' b17001_(1:2) # "B17001_001" "B17001_002"
#' b02001_(1:4) # "B02001_001" "B02001_002" "B02001_003" "B02001_004"
#' b01001_(c(1, 3:25, 27:49))

#' @rdname b_table_funs
#' @export
b01001_ <- function(var_num){
  if(sum(!(var_num %in% 1:999)) >0){
    stop("values in var_num must be between 1 and 999.")}
  paste0("B01001_", sprintf("%03d", var_num)  )
}
#' @rdname b_table_funs
#' @export
b02001_ <- function(var_num){
  if(sum(!(var_num %in% 1:999)) >0){
    stop("values in var_num must be between 1 and 999.")}
  paste0("B02001_", sprintf("%03d", var_num)  )
}
#' @rdname b_table_funs
#' @export
b03002_ <- function(var_num){
  if(sum(!(var_num %in% 1:999)) >0){
    stop("values in var_num must be between 1 and 999.")}
  paste0("B03002_", sprintf("%03d", var_num)  )
}
#' @rdname b_table_funs
#' @export
b17001_ <- function(var_num){
  if(sum(!(var_num %in% 1:999)) >0){
    stop("values in var_num must be between 1 and 999.")}
  paste0("B17001_", sprintf("%03d", var_num)  )
}
