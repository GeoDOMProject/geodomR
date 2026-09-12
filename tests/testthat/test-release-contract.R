test_that("canonical codes preserve parents and leading zeros", {
  data <- data.frame(PROV = c("01", "02"), MUN = c("01", "01"), DM = "01", SECC = "01", BP = "001")
  codes <- geodom:::.gd_add_codes(data)
  expect_equal(codes$MUN_CODE, c("0101", "0201"))
  expect_equal(codes$BP_CODE, c("01010101001", "02010101001"))
  expect_equal(gd_detect_level(data.frame(MUN_CODE = c("0101", "0201")))$level, "municipalities")
})

test_that("map joins reject duplicate rows and preserve fill collisions", {
  data <- data.frame(PROV_CODE = c("01", "02"), TOPONIMIA = c(10, 20))
  result <- gd_map_data(data, fill = "TOPONIMIA", .level = "provinces", .name = "PROV_CODE", .key = "PROV_CODE")
  expect_equal(attr(result, "fill_var"), "TOPONIMIA_data")
  expect_equal(sum(!is.na(result$TOPONIMIA_data)), 2L)
  expect_error(gd_map_data(rbind(data, data[1, ]), fill = "TOPONIMIA", .level = "provinces", .name = "PROV_CODE", .key = "PROV_CODE"), "duplicadas")
})

test_that("municipalities and provinces use compatible geographic coordinates", {
  municipalities <- gd_municipalities()
  expect_equal(sf::st_crs(municipalities)$epsg, 4326)
  expect_true(all(sf::st_bbox(municipalities)[c("xmin", "xmax")] > -73))
  expect_true(all(sf::st_bbox(municipalities)[c("xmin", "xmax")] < -68))
  expect_equal(length(unique(municipalities$MUN_CODE)), 158L)
})
