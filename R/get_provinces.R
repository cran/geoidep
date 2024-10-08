#' Download INEI province boundaries
#'
#' @description
#' This function allows you to download the latest version of the \bold{geometry} and \bold{ubigeos}
#' corresponding to the \bold{official political division} of the province boundaries of Peru.
#' For more information, you can visit the following page \url{https://ide.inei.gob.pe/}
#'
#' @param dsn Character. Output filename. If missing, a temporary file is created.
#' @param show_progress Logical. Suppress bar progress.
#' @param quiet Logical. Suppress info message.
#'
#' @returns An sf or tibble object.
#'
#' @examples
#' \donttest{
#' library(geoidep)
#' prov <- get_provinces(show_progress = FALSE)
#' head(prov)
#' }
#' @export

get_provinces <- \(dsn = NULL, show_progress = TRUE, quiet = TRUE){
  primary_link <- get_inei_link("provincia")

  if (is.null(dsn)) {
    dsn <- tempfile(fileext = ".rar")
  }

  if (isTRUE(show_progress)) {
    rar.download <- httr::GET(
      primary_link,
      config = httr::config(ssl_verifypeer = FALSE),
      httr::write_disk(dsn, overwrite = TRUE),
      httr::progress()
    )
  } else {
    rar.download <- httr::GET(
      primary_link,
      config = httr::config(ssl_verifypeer = FALSE),
      httr::write_disk(dsn, overwrite = TRUE)
    )
  }

  # Check if the download was successful
  if (httr::http_error(rar.download)) {
    stop("Error downloading the file. Check the URL or connection")
  }

  extract_dir <- tempfile()
  dir.create(extract_dir)

  archive::archive_extract(
    archive = dsn,
    dir = extract_dir
  )

  gpkg_file <- list.files(extract_dir, pattern = "\\.gpkg$", full.names = TRUE)

  # Validate if .gpkg file exists
  if (length(gpkg_file) == 0) {
    stop("No .gpkg file was found after extraction")
  }

  sf_data <- sf::st_read(gpkg_file, quiet = quiet)

  return(sf_data)
}
