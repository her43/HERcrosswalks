#' ACS B-table variable list creation
#'
#' Quickly create a list of ACS variable names in a format that is
#' compatible with the tidycensus package.
#'
#' @param var_num vector of (integer) numbers greater than or equal to 1,
#' each representing a 3-digit variable number within the ACS table.
#'
#' @return A vector of ACS variable names
#' @examples
#' b17001_(1:2) # "B17001_001" "B17001_002"
#' b02001_(1:4) # "B02001_001" "B02001_002" "B02001_003" "B02001_004"
#' b01001_(c(1, 3:25, 27:49))

#' @rdname b_table_funs
#' @export
b01001_ <- function(var_num){
  var_num <- unique(sort(var_num))
  if(sum(!(var_num %in% 1:49)) >0){
    stop("For ACS table B01001, values in var_num must be between 1 and 49.")}
  paste0("B01001_", sprintf("%03d", var_num)  )
}
#' @rdname b_table_funs
#' @export
b02001_ <- function(var_num){
  var_num <- unique(sort(var_num))
  if(sum(!(var_num %in% 1:10)) >0){
    stop("For ACS table B02001, values in var_num must be between 1 and 10.")}
  paste0("B02001_", sprintf("%03d", var_num)  )
}

#' @rdname b_table_funs
#' @export
b03001_ <- function(var_num){
  var_num <- unique(sort(var_num))
  if(sum(!(var_num %in% 1:31)) >0){
    stop("For ACS table B03001, values in var_num must be between 1 and 31.")}
  paste0("B03001_", sprintf("%03d", var_num)  )
}

#' @rdname b_table_funs
#' @export
b03002_ <- function(var_num){
  var_num <- unique(sort(var_num))
  if(sum(!(var_num %in% 1:21)) >0){
    stop("For ACS table B03002, values in var_num must be between 1 and 21.")}
  paste0("B03002_", sprintf("%03d", var_num)  )
}

#' @rdname b_table_funs
#' @export
b05012_ <- function(var_num){
  var_num <- unique(sort(var_num))
  if(sum(!(var_num %in% 1:3)) >0){
    stop("For ACS table B05012, values in var_num must be between 1 and 3.")}
  paste0("B05012_", sprintf("%03d", var_num)  )
}

#' @rdname b_table_funs
#' @export
b05013_ <- function(var_num){
  var_num <- unique(sort(var_num))
  if(sum(!(var_num %in% 1:39)) >0){
    stop("For ACS table B05013, values in var_num must be between 1 and 39.")}
  paste0("B05013_", sprintf("%03d", var_num)  )
}

#' @rdname b_table_funs
#' @export
b05014_ <- function(var_num){
  var_num <- unique(sort(var_num))
  if(sum(!(var_num %in% 1:19)) >0){
    stop("For ACS table B05014, values in var_num must be between 1 and 19.")}
  paste0("B5014_", sprintf("%03d", var_num)  )
}

#' @rdname b_table_funs
#' @export
b17001_ <- function(var_num){
  var_num <- unique(sort(var_num))
  if(sum(!(var_num %in% 1:59)) >0){
    stop("For ACS table B17001, values in var_num must be between 1 and 59.")}
  paste0("B17001_", sprintf("%03d", var_num)  )
}

#' @rdname b_table_funs
#' @export
b19013_ <- function(var_num){
  var_num <- unique(sort(var_num))
  if(sum(!(var_num %in% 1)) >0){
    stop("For ACS table B19013, values in var_num must be 1.")}
  return("B19013_001")
}
