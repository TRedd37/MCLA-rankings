#' @docType package
#' @title ReddRankings
#' 
#' @description Package for calculating the Redd Rankings
#' 
#' @importFrom rvest html_nodes html_attr html_table
#' @importFrom xml2 read_html
#' @importFrom XML htmlParse readHTMLTable
#' @importFrom dplyr %>% select filter rename bind_cols mutate sample_n 
#' group_by ungroup arrange inner_join desc left_join tbl collect bind_rows
#' slice
#' @importFrom tidyr spread gather
#' @importFrom plyr ldply
#' @importFrom MASS ginv
#' @importFrom lubridate parse_date_time now
#' @importFrom stringr str_detect str_split
#' 
#' @name ReddRankings
NULL