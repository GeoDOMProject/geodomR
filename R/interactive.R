#' Crear un mapa interactivo HTML portable
#'
#' Incluye el visor, los estilos y las geometrias en un documento HTML. Permite
#' buscar, seleccionar y consultar territorios, acercar y desplazar el mapa y
#' explorar provincias y municipios. Las capas de contexto no heredan mediciones.
#' El fondo predeterminado funciona sin conexion despues de generar el archivo.
#'
#' @inheritParams gd_map_data
#' @param file Ruta HTML opcional donde guardar el mapa.
#' @param title,subtitle,caption Titulo, subtitulo y fuente del mapa.
#' @param labels FALSE, TRUE, "name", "value" o "both".
#' @param background "none" (sin conexion) u "osm" (calles, requiere internet).
#' @param context Incluir limites provinciales y municipales para explorar.
#' @return Documento HTML de clase `geodom_html`, invisiblemente si se usa `file`.
#' @export
#' @examples
#' \dontrun{
#' datos <- data.frame(PROV_CODE = c("01", "25"), valor = c(12, 34))
#' gd_map_interactive(datos, fill = "valor", .level = "provinces",
#'                    .name = "PROV_CODE", .key = "PROV_CODE", file = "mapa.html")
#' }
gd_map_interactive <- function(data, fill = NULL, .level = NULL, .name = NULL,
                               .key = NULL, file = NULL, title = "Mapa GeoDOM",
                               subtitle = NULL, caption = NULL, labels = FALSE,
                               background = "none", context = TRUE) {
    background <- match.arg(background, c("none", "osm"))
    if (is.null(labels)) labels <- FALSE
    if (length(labels) != 1L || !labels %in% c(FALSE, TRUE, "name", "value", "both")) {
        stop("labels debe ser FALSE, TRUE, 'name', 'value' o 'both'.")
    }
    joined <- gd_map_data(data, fill, .level, .name, .key)
    context_layers <- list()
    if (isTRUE(context)) {
        getters <- list(provinces = gd_provinces, municipalities = gd_municipalities)
        for (id in names(getters)) {
            if (id != attr(joined, "geo_level")) context_layers[[id]] <- getters[[id]]()
        }
    }
    .gd_interactive_html(joined, context_layers, file, title, subtitle, caption, labels, background)
}

.gd_interactive_html <- function(joined, context_layers = list(), file = NULL,
                                 title = "Mapa GeoDOM", subtitle = NULL, caption = NULL,
                                 labels = FALSE, background = "none") {
    primary <- attr(joined, "geo_level")
    to_geojson <- function(x) {
        if (!inherits(x, "sf") || nrow(x) == 0L || is.na(sf::st_crs(x))) {
            stop("La capa debe contener geometrias con un sistema de referencia conocido.")
        }
        if (any(sf::st_is_empty(x)) || !all(sf::st_geometry_type(x) %in% c("POLYGON", "MULTIPOLYGON"))) {
            stop("El visor requiere poligonos o multipoligonos no vacios.")
        }
        temporary <- tempfile(fileext = ".geojson")
        on.exit(unlink(temporary), add = TRUE)
        sf::st_write(sf::st_transform(x, 4326), temporary, driver = "GeoJSON", quiet = TRUE,
                     layer_options = c("RFC7946=YES", "COORDINATE_PRECISION=6"))
        jsonlite::fromJSON(temporary, simplifyVector = FALSE)
    }
    layers <- list(list(id = primary, fillVar = attr(joined, "fill_var"),
                        measured = TRUE, geojson = to_geojson(joined)))
    for (id in names(context_layers)) {
        layers[[length(layers) + 1L]] <- list(id = id, fillVar = NULL, measured = FALSE,
                                             geojson = to_geojson(context_layers[[id]]))
    }
    payload <- list(version = "1.1.0", primary = primary, layers = layers,
                    options = list(title = title, subtitle = subtitle %||% "",
                                   caption = caption %||% "", labels = labels,
                                   background = background))
    encoded <- as.character(jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null",
                                             na = "null", digits = NA))
    for (pair in list(c("<", "\\u003c"), c(">", "\\u003e"), c("&", "\\u0026"))) {
        encoded <- gsub(pair[1], pair[2], encoded, fixed = TRUE)
    }
    compressed_file <- tempfile(fileext = ".gz")
    on.exit(unlink(compressed_file), add = TRUE)
    connection <- gzfile(compressed_file, "wb")
    tryCatch(writeBin(charToRaw(enc2utf8(encoded)), connection), finally = close(connection))
    encoded <- jsonlite::base64_enc(readBin(compressed_file, "raw", n = file.info(compressed_file)$size))
    assets <- system.file("interactive", package = "geodom")
    if (!nzchar(assets)) stop("No se encontraron los archivos del visor. Reinstala geodom.")
    runtime <- paste(readLines(file.path(assets, "interactive-runtime.js"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    css <- paste(readLines(file.path(assets, "interactive.css"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    safe_title <- as.character(title)
    for (pair in list(c("&", "&amp;"), c("<", "&lt;"), c(">", "&gt;"), c('"', "&quot;"))) {
        safe_title <- gsub(pair[1], pair[2], safe_title, fixed = TRUE)
    }
    html <- paste0('<!doctype html><html lang="es"><head><meta charset="utf-8">',
        '<meta name="viewport" content="width=device-width,initial-scale=1">',
        '<meta name="generator" content="GeoDOM 1.1.0">',
        '<meta http-equiv="Content-Security-Policy" content="default-src \'none\'; script-src \'unsafe-inline\'; style-src \'unsafe-inline\'; img-src data: https://tile.openstreetmap.org; connect-src \'none\'; base-uri \'none\'; form-action \'none\'">',
        '<title>', safe_title, '</title><style>', css, '</style></head><body>',
        '<main id="geodom-interactive"></main><script id="geodom-payload" type="application/octet-stream" data-encoding="gzip">',
        encoded, '</script><script>', runtime,
        '\nGeoDOMInteractive.boot();</script></body></html>')
    result <- structure(html, class = c("geodom_html", "character"))
    if (!is.null(file)) {
        writeLines(enc2utf8(html), file, useBytes = TRUE)
        return(invisible(result))
    }
    result
}

#' @export
print.geodom_html <- function(x, ...) {
    cat("Mapa interactivo GeoDOM (HTML portable).\n",
        "Guardalo con writeLines(x, 'mapa.html') o con file en gd_map_interactive().\n", sep = "")
    invisible(x)
}
