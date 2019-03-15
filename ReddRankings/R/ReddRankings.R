#' @docType package
#' @title ReddRankings
#' 
#' @description Package for calculating the Redd Rankings
#' 
#' @importFrom rvest html_nodes html_attr
#' @importFrom xml2 read_html
#' @importFrom XML htmlParse readHTMLTable
#' @importFrom dplyr %>% select filter rename bind_cols mutate sample_n 
#' group_by ungroup arrange inner_join desc left_join tbl collect
#' @importFrom tidyr spread
#' @importFrom plyr ldply
#' @importFrom MASS ginv
#' 
#' @name ReddRankings
NULL