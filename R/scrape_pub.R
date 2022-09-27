#' @name scrape_pub
#' @title Web Scraping for 'PubMed'
#'
#' @description Use web scraping for 'PubMed' searches based on first and last names to get all of the published information from the search page. Then placing it on a table
#'
#' @param first_name The author's first name
#' @param last_name  The author's last name
#' @import rvest
#' @return A dataframe containing publishing information such as title, author, journal, date, page, and so on.
#' @examples
#' paper_list <- scrape_pub("Jedi","wars")
#' @export


scrape_pub <- function(first_name, last_name){

  url1 <- creat_url_pub(first_name, last_name, 1)
  page <- xml2::read_html(url1)
  total_results <- as.numeric(trimws(strsplit(html_text(html_nodes(page, ".results-amount")),"\n")[[1]][[3]]))

  total_info <- NULL
  page_ids <- seq(1:(total_results/100))
  for(page_id in page_ids){
    total_info <- dplyr::bind_rows(NULL, scrape_pub_in(creat_url_pub(first_name, last_name, page_id)))
  }
  total_info
}
