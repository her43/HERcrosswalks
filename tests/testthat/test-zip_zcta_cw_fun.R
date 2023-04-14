test_that("correct data", {
  temp_2009 <- zip_zcta_cw_fun(years = 2009, po_city_names = T)
  expect_equal(colnames(temp_2009),
               c("zip", "zcta", "po_city", "state"))
  expect_equal(nrow(temp_2009), length(unique(temp_2009$zip)))
})
