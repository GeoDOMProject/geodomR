# Internal canonical territorial identifiers. Local municipality/section codes repeat.
.gd_code_parts <- list(REG_CODE = "CODREG", PROV_CODE = "PROV",
  MUN_CODE = c("PROV", "MUN"), DM_CODE = c("PROV", "MUN", "DM"),
  SEC_CODE = c("PROV", "MUN", "DM", "SEC"), BP_CODE = c("PROV", "MUN", "DM", "SEC", "BP"))

.gd_code <- function(x, width) {
  value <- sub("\\.0$", "", as.character(x))
  digits <- !is.na(value) & grepl("^[0-9]+$", value)
  value[digits] <- stringr::str_pad(value[digits], width, pad = "0")
  value
}

.gd_add_codes <- function(data) {
  if (!"SEC" %in% names(data) && "SECC" %in% names(data)) data$SEC <- data$SECC
  if (!"CODREG" %in% names(data) && "REG" %in% names(data)) data$CODREG <- data$REG
  for (key in names(.gd_code_parts)) {
    parts <- .gd_code_parts[[key]]
    width <- if (key == "BP_CODE") 11L else length(parts) * 2L
    if (key %in% names(data)) {
      data[[key]] <- .gd_code(data[[key]], width)
    } else if (all(parts %in% names(data))) {
      data[[key]] <- do.call(paste0, lapply(parts, function(part) .gd_code(data[[part]], if (part == "BP") 3L else 2L)))
      data[[key]][!stats::complete.cases(sf::st_drop_geometry(data)[parts])] <- NA_character_
    }
  }
  data
}

.gd_restore_crs <- function(data, id) {
  # RD_MUN158 matches the EPSG:32619 geometry in sfDR/R/sysdata.rda.
  # The other listed default layers are published in longitude/latitude.
  source_crs <- c(RD_PROV = 4326L, RD_RUP = 4326L, RD_DM = 4326L,
    RD_SECCIONES = 4326L, RD_BPARAJES = 4326L, RD_MUN158 = 32619L)
  if (is.na(sf::st_crs(data)) && id %in% names(source_crs)) {
    data <- sf::st_set_crs(data, source_crs[[id]])
  }
  if (!is.na(sf::st_crs(data))) data <- sf::st_transform(data, 4326)
  data
}
