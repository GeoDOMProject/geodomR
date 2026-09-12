# Evitar NOTEs de variables globales en dplyr
utils::globalVariables(c(
  "MUN_ID", "MUN_NAME", "MUN_NAME_CLEAN", "MUN_NAME_OFFICIAL", "distance"
))

# R/municipalities.R

#' Obtener Límites de los Municipios de la República Dominicana
#'
#' Descarga (si es necesario) y carga los límites de los municipios de la
#' República Dominicana, según la division correspondiente al id suministrado.
#'
#' Las divisiones disponibles son:
#'
#' - `"RD_MUN158"` (por defecto): 158 Municipios de la República Dominicana.
#' - `"RD_MUN155"`: TODO
#'
#' @param id Nombre del archivo de datos en el servidor remoto.
#' @param sf Lógico. Si es `FALSE`, devuelve un data.frame regular sin la
#'   columna de geometría. Por defecto es `TRUE`.
#' @param .parents Lógico. Si es `TRUE`, agrega columnas de niveles superiores
#'   disponibles (`Provincia` y `Region`). Por defecto es `FALSE`.
#' @param .levels Vector opcional con los niveles superiores específicos a
#'   agregar. Para municipios acepta `"provinces"` y `"regions"`. Si se
#'   proporciona, implica `.parents = TRUE`.
#'
#' @return Un objeto de la clase `sf` o un `data.frame`.
#' @export
#' @importFrom sf st_drop_geometry
#' @examples
#' \dontrun{
#' # Cargar el objeto sf completo
#' municipios_sf <- gd_municipalities()
#'
#' # Cargar solo la tabla de atributos (sin geometría)
#' municipios_df <- gd_municipalities(sf = FALSE)
#'
#' # Cargar municipios con provincia y región
#' municipios_padres <- gd_municipalities(.parents = TRUE)
#' }
gd_municipalities <- function(id = "RD_MUN158", sf = TRUE, .parents = FALSE, .levels = NULL) {
  data_sf <- fetch_and_cache(id = id)

  if (isTRUE(.parents) || !is.null(.levels)) {
    data_sf <- .add_municipality_parent_cols(data_sf, .levels = .levels)
  }

  if (!sf) {
    data_sf <- sf::st_drop_geometry(data_sf)
  }

  return(data_sf)
}

.add_municipality_parent_cols <- function(data_sf, .levels = NULL) {
  valid_levels <- c("regions", "provinces")
  parent_levels <- if (is.null(.levels)) valid_levels else .levels

  invalid <- setdiff(parent_levels, valid_levels)
  if (length(invalid) > 0) {
    cli::cli_abort(c(
      "x" = paste0("Niveles superiores no v\u00e1lidos para municipios: ", paste(invalid, collapse = ", ")),
      "i" = paste0("Use uno de: ", paste(valid_levels, collapse = ", "))
    ))
  }

  if ("regions" %in% parent_levels) {
    reg_sf <- gd_regions(sf = FALSE)
    reg_map <- stats::setNames(
      gd_clean_region_name(reg_sf$TOPONIMIA, .on_error = "na"),
      reg_sf$CODREG
    )
    data_sf$Region <- unname(reg_map[data_sf$REG])
  }

  if ("provinces" %in% parent_levels) {
    prov_sf <- gd_provinces(sf = FALSE)
    prov_map <- stats::setNames(
      gd_clean_prov_name(prov_sf$TOPONIMIA, .on_error = "na"),
      prov_sf$PROV
    )
    data_sf$Provincia <- unname(prov_map[data_sf$PROV])
  }

  data_sf
}

# Helper function for municipality aliases
.get_municipios_alias <- function() {
  raw_data <- gd_get_dataset(id = "municipios_alias", verbose = FALSE)
  
  # Extract the data portion from the JSON structure
  if (is.list(raw_data) && "data" %in% names(raw_data)) {
    return(raw_data$data)
  } else {
    # If it's already in the expected format, return as is
    return(raw_data)
  }
}

# Helper function for robust municipality name cleaning
.do_municipality_names_cleaning <- function(names, alias_data, .tol = 0.25, .on_error = "fail",
                                             parent_filter_ids = NULL, parent_prefix_len = NULL) {
  .do_generic_names_cleaning(
    names = names, alias_data = alias_data,
    id_col = "MUN_ID", name_col = "MUN_NAME",
    level_label = "Municipality",
    prefix_regex = "^(municipio|mun)\\.?\\s+",
    code_regex = "^\\d{4}$",
    parent_filter_ids = parent_filter_ids,
    parent_prefix_len = parent_prefix_len,
    parent_hint = "Use .province to disambiguate",
    .tol = .tol, .on_error = .on_error
  )
}

#' Limpia y estandariza los nombres de municipios de la República Dominicana
#'
#' Esta función limpia y estandariza los nombres de los municipios en la República Dominicana,
#' con tolerancia para la similitud de cadenas y opciones para el manejo de errores. Utiliza
#' el sistema oficial de códigos municipales de la República Dominicana.
#'
#' Soporta tres modos de entrada:
#' - **Nombre**: se busca en el dataset de aliases (exacto, prefijo, fuzzy).
#' - **Código**: un código de 4 dígitos (MUN_ID) se valida directamente.
#' - **Desambiguación**: usar `.province` para filtrar por provincia padre.
#'
#' @param mun Vector de caracteres con los nombres (o códigos de 4 dígitos) de
#'   municipios a limpiar.
#' @param .province Nombre de la provincia padre para desambiguar.
#' @param .tol Nivel de tolerancia numérica para la similitud de cadenas. Por defecto es 0.25.
#' @param .on_error Cadena de caracteres que especifica el método de manejo de errores.
#'   Puede ser "fail", "omit" o "na". Por defecto es "fail".
#'
#' @return Un vector de caracteres con los nombres de municipios limpiados.
#' @export
#'
#' @examples
#' \dontrun{
#' # Uso básico con nombres de municipios
#' gd_clean_municipality_name(c("santiago", "moca", "bonao"))
#'
#' # Con código directo (4 dígitos)
#' gd_clean_municipality_name("0201")
#'
#' # Con variantes de prefijos
#' gd_clean_municipality_name(c("Municipio de Santiago", "Mun. Moca"))
#'
#' # Con provincia padre
#' gd_clean_municipality_name("Azua", .province = "Azua")
#'
#' # Con tolerancia y manejo de errores
#' gd_clean_municipality_name("santiagooo", .tol = 0.8, .on_error = "na")
#' }
gd_clean_municipality_name <- function(mun, .province = NULL, .tol = 0.25, .on_error = "fail") {
  .validate_clean_params(.tol, .on_error)

  alias_data <- .get_municipios_alias()

  # Resolver filtro de padre
  parent_filter_ids <- NULL
  parent_prefix_len <- NULL

  if (!is.null(.province)) {
    prov_alias <- .get_provincias_alias()
    prov_ids <- .resolve_parent_ids(.province, prov_alias, "PROV_ID", "PROV_NAME")
    if (!is.null(prov_ids)) {
      parent_filter_ids <- prov_ids
      parent_prefix_len <- 2L
    }
  }

  .do_municipality_names_cleaning(mun, alias_data, .tol, .on_error,
                                   parent_filter_ids = parent_filter_ids,
                                   parent_prefix_len = parent_prefix_len)
}
