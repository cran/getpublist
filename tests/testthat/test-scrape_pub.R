test_that("scrape_pub works", {
  paper_list <- scrape_pub("Jedi","wars")
  expect_equal(nrow(paper_list),5)
})
