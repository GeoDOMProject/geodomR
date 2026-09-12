# R/normalize-names.R

#' Normalizar nombres geográficos
#'
#' Convierte nombres a una forma estable para joins o comparaciones:
#' minúsculas, sin acentos, sin puntuación y con espacios normalizados.
#' A diferencia de `.text_cleaning()`, esta función no elimina artículos ni
#' preposiciones como "de", "el", "la" o "los".
#'
#' @param name Vector de caracteres con nombres geográficos.
#'
#' @return Un vector de caracteres normalizado.
#' @export
#'
#' @examples
#' gd_normalize_name("San Francisco de Macorís")
gd_normalize_name <- function(name) {
  if (length(name) == 0) {
    return(character(0))
  }

  name <- as.character(name)
  missing <- is.na(name) | name == "_NA_"

  name <- stringr::str_to_lower(name)
  name <- stringi::stri_trans_general(name, "Latin-ASCII")
  name <- stringr::str_replace_all(name, stringr::regex("[^0-9a-z]+"), " ")
  name <- stringr::str_squish(name)
  name[missing] <- NA_character_

  name
}

#' Agregar columnas con nombres geográficos normalizados
#'
#' Agrega columnas normalizadas a partir de columnas geográficas existentes.
#' Por defecto normaliza los valores existentes. Si `.clean = TRUE`, primero
#' corrige cada valor usando el cleaner oficial del nivel administrativo y luego
#' normaliza el resultado.
#'
#' @param data Un data frame.
#' @param .cols Vector de nombres de columnas a normalizar. Si es `NULL`, se
#'   usan columnas geográficas detectadas por nombre, como `provincia`,
#'   `municipio`, `region`, `dm`, `seccion`, `barrio_paraje` o `zona`.
#' @param .suffix Sufijo para las columnas nuevas. Por defecto `"_norm"`.
#' @param .clean Lógico. Si `TRUE`, corrige primero los nombres usando los
#'   cleaners oficiales de geodom. Por defecto `FALSE`.
#' @param .levels Vector opcional con los niveles administrativos de `.cols`.
#'   Puede ser nombrado, por ejemplo `c(provincia = "provinces")`. Niveles
#'   válidos: `"regions"`, `"provinces"`, `"municipalities"`, `"dm"`,
#'   `"sections"`, `"bparajes"` y `"zones"`.
#' @param .tol Tolerancia para los cleaners cuando `.clean = TRUE`.
#' @param .on_error Manejo de errores en los cleaners cuando `.clean = TRUE`:
#'   `"fail"`, `"na"` u `"omit"`.
#'
#' @return `data` con columnas adicionales normalizadas.
#' @export
#'
#' @examples
#' datos <- data.frame(
#'   provincia = c("San Cristóbal", "Duarte"),
#'   municipio = c("Villa Altagracia", "San Francisco de Macorís")
#' )
#'
#' gd_add_normalized_names(datos)
gd_add_normalized_names <- function(data, .cols = NULL, .suffix = "_norm",
                                    .clean = FALSE, .levels = NULL,
                                    .tol = 0.25, .on_error = "na") {
  if (!is.data.frame(data)) {
    cli::cli_abort("`data` debe ser un data frame.")
  }
  if (!is.character(.suffix) || length(.suffix) != 1 || is.na(.suffix)) {
    cli::cli_abort("`.suffix` debe ser una cadena de caracteres de longitud 1.")
  }
  if (!is.logical(.clean) || length(.clean) != 1 || is.na(.clean)) {
    cli::cli_abort("`.clean` debe ser `TRUE` o `FALSE`.")
  }
  if (isTRUE(.clean)) {
    .validate_clean_params(.tol, .on_error)
  }

  cols <- .resolve_normalized_name_cols(data, .cols)
  result <- data

  for (i in seq_along(cols)) {
    col <- cols[[i]]
    values <- data[[col]]

    if (isTRUE(.clean)) {
      level <- .resolve_normalized_name_level(data, cols, col, i, .levels)
      values <- .clean_values_for_normalized_names(
        values = values,
        level = level,
        data = data,
        current_col = col,
        .tol = .tol,
        .on_error = .on_error
      )
    }

    result[[paste0(col, .suffix)]] <- gd_normalize_name(values)
  }

  result
}

.resolve_normalized_name_cols <- function(data, .cols) {
  if (is.null(.cols)) {
    is_text <- vapply(data, function(x) is.character(x) || is.factor(x), logical(1))
    cols <- names(data)[is_text & vapply(names(data), function(x) {
      !is.null(.infer_normalized_name_level(x))
    }, logical(1))]
  } else {
    if (!is.character(.cols)) {
      cli::cli_abort("`.cols` debe ser un vector de nombres de columnas.")
    }
    cols <- .cols
  }

  missing_cols <- setdiff(cols, names(data))
  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "x" = paste0("Columnas no encontradas: ", paste(missing_cols, collapse = ", ")),
      "i" = paste0("Columnas disponibles: ", paste(names(data), collapse = ", "))
    ))
  }
  if (length(cols) == 0) {
    cli::cli_abort(c(
      "x" = "No se detectaron columnas geogr\u00e1ficas para normalizar.",
      "i" = "Use `.cols` para especificarlas expl\u00edcitamente."
    ))
  }

  cols
}

.resolve_normalized_name_level <- function(data, cols, col, i, .levels) {
  valid_levels <- c("regions", "provinces", "municipalities", "dm",
                    "sections", "bparajes", "zones")

  level <- NULL
  if (!is.null(.levels)) {
    levels <- unlist(.levels, use.names = TRUE)
    level_names <- names(levels)

    if (!is.null(level_names) && col %in% level_names) {
      level <- unname(levels[[col]])
    } else if (length(levels) == 1) {
      level <- unname(levels[[1]])
    } else if (length(levels) == length(cols)) {
      level <- unname(levels[[i]])
    } else {
      cli::cli_abort("`.levels` debe tener longitud 1, la misma longitud que `.cols`, o nombres que coincidan con `.cols`.")
    }
  } else {
    level <- .infer_normalized_name_level(col)
  }

  if (is.null(level)) {
    cli::cli_abort(c(
      "x" = paste0("No se pudo inferir el nivel administrativo de la columna '", col, "'."),
      "i" = "Use `.levels` para especificarlo expl\u00edcitamente."
    ))
  }
  if (!level %in% valid_levels) {
    cli::cli_abort(c(
      "x" = paste0("Nivel administrativo no v\u00e1lido: ", level),
      "i" = paste0("Niveles v\u00e1lidos: ", paste(valid_levels, collapse = ", "))
    ))
  }

  level
}

.infer_normalized_name_level <- function(col) {
  col <- gd_normalize_name(col)

  if (grepl("distrito|municipal district|^dm$|_dm$", col)) {
    return("dm")
  }
  if (grepl("municipio|municipality|^mun$|_mun$", col)) {
    return("municipalities")
  }
  if (grepl("provincia|province|^prov$|_prov$", col)) {
    return("provinces")
  }
  if (grepl("region|^reg$|_reg$", col)) {
    return("regions")
  }
  if (grepl("seccion|section|^sec$|_sec$", col)) {
    return("sections")
  }
  if (grepl("barrio|paraje|bparaje|bp", col)) {
    return("bparajes")
  }
  if (grepl("zona|zone", col)) {
    return("zones")
  }

  NULL
}

.clean_values_for_normalized_names <- function(values, level, data, current_col,
                                               .tol, .on_error) {
  values <- as.character(values)

  switch(level,
    regions = gd_clean_region_name(values, .tol = .tol, .on_error = .on_error),
    provinces = gd_clean_prov_name(values, .tol = .tol, .on_error = .on_error),
    municipalities = .clean_municipality_values_for_normalized_names(
      values, data, current_col, .tol, .on_error
    ),
    dm = gd_clean_dm_name(values, .tol = .tol, .on_error = .on_error),
    sections = gd_clean_section_name(values, .tol = .tol, .on_error = .on_error),
    bparajes = gd_clean_bparaje_name(values, .tol = .tol, .on_error = .on_error),
    zones = gd_clean_zone_name(values, .tol = .tol, .on_error = .on_error),
    cli::cli_abort(paste0("Nivel administrativo no reconocido: ", level))
  )
}

.clean_municipality_values_for_normalized_names <- function(values, data, current_col,
                                                            .tol, .on_error) {
  province_col <- .find_normalized_name_parent_col(data, current_col, "provinces")
  if (is.null(province_col)) {
    return(gd_clean_municipality_name(values, .tol = .tol, .on_error = .on_error))
  }

  vapply(seq_along(values), function(i) {
    gd_clean_municipality_name(
      values[[i]],
      .province = data[[province_col]][[i]],
      .tol = .tol,
      .on_error = .on_error
    )
  }, character(1))
}

.find_normalized_name_parent_col <- function(data, current_col, parent_level) {
  cols <- setdiff(names(data), current_col)
  inferred <- vapply(cols, function(col) {
    level <- .infer_normalized_name_level(col)
    if (is.null(level)) {
      return(NA_character_)
    }
    level
  }, character(1), USE.NAMES = FALSE)
  match <- cols[!is.na(inferred) & inferred == parent_level]

  if (length(match) == 0) {
    return(NULL)
  }

  match[[1]]
}
