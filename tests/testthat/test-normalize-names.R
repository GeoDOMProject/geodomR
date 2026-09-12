test_that("gd_normalize_name lowercases and removes accents without dropping words", {
  expect_equal(
    gd_normalize_name(c("San Cristóbal", "San Francisco de Macorís", " El Seibo ")),
    c("san cristobal", "san francisco de macoris", "el seibo")
  )
  expect_equal(gd_normalize_name(NA_character_), NA_character_)
  expect_equal(gd_normalize_name("_NA_"), NA_character_)
})

test_that("gd_add_normalized_names adds normalized geographic columns", {
  datos <- data.frame(
    provincia = c(
      "San Cristóbal",
      "San Cristóbal",
      "Duarte",
      "Distrito Nacional",
      "Santo Domingo"
    ),
    municipio = c(
      "Villa Altagracia",
      "San Cristóbal",
      "San Francisco de Macorís",
      "Distrito Nacional",
      "Santo Domingo Este"
    ),
    stringsAsFactors = FALSE
  )

  result <- gd_add_normalized_names(datos)

  expect_equal(
    result$provincia_norm,
    c("san cristobal", "san cristobal", "duarte", "distrito nacional", "santo domingo")
  )
  expect_equal(
    result$municipio_norm,
    c(
      "villa altagracia",
      "san cristobal",
      "san francisco de macoris",
      "distrito nacional",
      "santo domingo este"
    )
  )
})

test_that("gd_add_normalized_names can clean before normalizing", {
  datos <- data.frame(
    provincia = c("san cristobal", "duarte"),
    municipio = c("villa altagracia", "san francisco de macoris"),
    stringsAsFactors = FALSE
  )

  result <- gd_add_normalized_names(datos, .clean = TRUE)

  expect_equal(result$provincia_norm, c("san cristobal", "duarte"))
  expect_equal(result$municipio_norm, c("villa altagracia", "san francisco de macoris"))
})

test_that("gd_add_normalized_names validates explicit columns and levels", {
  datos <- data.frame(nombre = "San Cristóbal", stringsAsFactors = FALSE)

  expect_error(
    gd_add_normalized_names(datos, .cols = "provincia"),
    "Columnas no encontradas"
  )
  expect_error(
    gd_add_normalized_names(datos, .cols = "nombre", .clean = TRUE),
    "No se pudo inferir"
  )
  expect_error(
    gd_add_normalized_names(datos, .cols = "nombre", .clean = TRUE, .levels = "invalid"),
    "Nivel administrativo no válido"
  )
})
