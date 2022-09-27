test_that("scrape_goolge_profile works", {
  url <- "https://scholar.google.com/citations?user=hGTnO4oAAAAJ&hl=en&authuser=3"
  paper_list <- scrape_google_profile(url)
  expect_equal(nrow(paper_list),4)
})
