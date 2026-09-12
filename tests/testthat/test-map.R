test_that("gd_map supports JS-compatible text and label options", {
  prov <- gd_provinces(sf = FALSE)
  datos <- data.frame(
    provincia = prov$TOPONIMIA[1:3],
    valor = c(1, 2, 3)
  )

  p <- gd_map(
    datos,
    fill = "valor",
    labels = "both",
    title = "Titulo",
    subtitle = "Subtitulo",
    caption = "Fuente: GeoDOM"
  )

  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "Titulo")
  expect_equal(p$labels$subtitle, "Subtitulo")
  expect_equal(p$labels$caption, "Fuente: GeoDOM")
})
