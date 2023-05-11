#' Say hello to a cat
#' @param cat The minimum proportion of the city that must
#' overlap with the county (weighted by population) for the county to be
#' considered an appropriate proxy for the city. Proportions are valued
#' 0.0001 (0.01%) to 1 (100%).
#'
#' @return A data frame containing a crosswalk between cities and
#' appropriate county proxies. The data frame will contain the following
#' columns:
#'
#' @export
#'
#' @examples
#' hello_cat()


hello_cat <- function(cat='bubbles'){
  return(paste("HERcrosswalks says hello to", cat))
}
