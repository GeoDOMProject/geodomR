test_that("interactive HTML keeps zero, missing values and unmeasured context", {
    poly <- sf::st_polygon(list(matrix(c(-70,18,-69,18,-69,19,-70,18), ncol=2, byrow=TRUE)))
    frame <- sf::st_sf(PROV_CODE=c("01","02"), TOPONIMIA=c("Azua","Bahoruco"),
                       value=c(0,NA_real_), geometry=sf::st_sfc(poly,poly,crs=4326))
    attr(frame,"geo_level") <- "provinces"
    attr(frame,"fill_var") <- "value"
    context <- frame[,c("PROV_CODE","TOPONIMIA")]
    context$MUN_CODE <- c("0101","0201")
    danger <- "</script><script>alert(1)</script>"
    frame$TOPONIMIA[1] <- danger
    file <- tempfile(fileext=".html")
    on.exit(unlink(file))
    result <- geodom:::.gd_interactive_html(frame,
        context_layers=list(municipalities=sf::st_transform(context,32619)), title=danger, file=file)
    expect_s3_class(result,"geodom_html")
    expect_true(file.exists(file))
    expect_false(grepl(danger,result,fixed=TRUE))
    expect_false(grepl('<script[^>]+src=',result))
    encoded <- sub('.*<script id="geodom-payload" type="application/octet-stream" data-encoding="gzip">(.*?)</script>.*','\\1',result)
    payload <- jsonlite::fromJSON(rawToChar(memDecompress(jsonlite::base64_dec(encoded), "gzip")),simplifyVector=FALSE)
    expect_equal(payload$layers[[1]]$geojson$features[[1]]$properties$value,0)
    expect_null(payload$layers[[1]]$geojson$features[[2]]$properties$value)
    expect_equal(payload$layers[[1]]$geojson$features[[1]]$properties$TOPONIMIA,danger)
    expect_false(payload$layers[[2]]$measured)
    expect_null(payload$layers[[2]]$fillVar)
    expect_null(payload$layers[[2]]$geojson$features[[1]]$properties$value)
    expect_output(print(result),"Mapa interactivo")
})

test_that("interactive options fail clearly", {
    expect_error(gd_map_interactive(data.frame(),background="unknown"),"arg")
    expect_error(gd_map_interactive(data.frame(),labels="unknown"),"labels")
})
