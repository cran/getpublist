#' @importFrom magrittr %T>%

creat_url_pub <- function(first_name, last_name, page){
  paste0("https://pubmed.ncbi.nlm.nih.gov/?term=",first_name,"+",last_name,"&sort=date&size=100&page=",page,"&ac=no")#1
}

scrape_pub_in <- function(url){

  page<-read_html(url)

  title <- trimws(html_text(html_nodes(page, ".docsum-title")))
  author <- trimws(html_text(html_nodes(page, ".full-authors")))
  author_list <- strsplit(author, ",")
  cit_info <- trimws(html_text(html_nodes(page, ".full-journal-citation")))
  pmid <-  trimws(html_text(html_nodes(page, ".docsum-pmid")))

  total_info <- tibble::tibble(data.frame(title = title,
                                          author = author,
                                          cit_info = cit_info,
                                          pmid = pmid))
  clean_info <- suppressWarnings(tidyr::separate(total_info,cit_info,c("journal","page","doi","other"),sep = "\\. ",convert = TRUE) )
  clean_info <- clean_info %>%
    dplyr::mutate(date = substr(page, 0, 4)) %>%
    dplyr::mutate(author_list = strsplit(author, ","))
  clean_info
}

substr_right <- function(target_string, n){
  substr(target_string, nchar(target_string)-n+1, nchar(target_string))
}




