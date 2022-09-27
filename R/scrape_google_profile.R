#' @name scrape_google_profile
#' @title Web Scraping for 'Google Scholar' Profiles
#'
#' @description Get all of the published information from the profile page by using the 'Google Scholar' profiles link and putting it into a table.
#'
#' @param url A url link of the google scholar profile.
#'
#' @return A dataframe containing publishing information such as title, author, journal, date, page, and so on.
#'
#' @examples
#' url <- "https://scholar.google.com/citations?user=hGTnO4oAAAAJ&hl=en&authuser=3"
#' paper_list <- scrape_google_profile(url)
#' @export


scrape_google_profile<- function(url){

  page <- xml2::read_html(paste0(url,"&pagesize=100"))
  paper_title <- trimws(html_text(html_nodes(page, ".gsc_a_at")))
  second_info_raw <- trimws(html_text(html_nodes(page, ".gs_gray")))
  if(length(second_info_raw) != length(paper_title)*2){
  second_info_raw <- second_info_raw[-which(second_info_raw %in% c("0 articles","not available"))]}
  authors  <- second_info_raw[seq(1,length(second_info_raw),2)]
  journals_info <- second_info_raw[seq(2,length(second_info_raw),2)]
  journals <- trimws(substr(journals_info,1,  stringr::str_locate(journals_info,"\\d")[,1]-1))
  years <- substr_right(journals_info,4)
  total_info <- tibble::tibble(data.frame(title = paper_title,
                                          author = authors,
                                          journal = journals,
                                          date = years,
                                          page  = journals_info))
  total_info
}
