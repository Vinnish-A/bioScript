
# geo ---------------------------------------------------------------------


#' List files from a URL
#'
#' Extracts and lists files from a specified URL that match a certain pattern.
#'
#' @param url Character. The URL to fetch files from.
#'
#' @return A character vector containing file names matching the pattern.
#'
#' @importFrom xml2 read_html xml_text xml_find_all
#'
#' @examples
#' listURL(gse2url('GSE129516', series = 'matrix'))
#'
#' @keywords internal
listURL = function(url) {

  contents = xml2::read_html(url)
  files = grep("^G", xml2::xml_text(xml2::xml_find_all(contents, "//a/@href")), value = TRUE)

  return(files)

}

#' Generate GEO Series URL
#'
#' Constructs the URL for accessing GEO series files from NCBI's FTP server.
#'
#' @param gse_id Character. The GEO series ID.
#' @param series Character. The type of series to access ('matrix' or 'suppl').
#'
#' @return A character string containing the constructed URL.
#'
#' @examples
#' gse2url('GSE129516', series = 'matrix')
#'
#' @keywords internal
gse2url = function(gse_id, series = 'matrix') {

  gse_uid = gsub("\\d{1,3}$", "nnn", gse_id, perl = TRUE)

  if (series == 'matrix') {
    url_base = 'https://ftp.ncbi.nlm.nih.gov/geo/series/%s/%s/matrix/'
    url_spec = sprintf(url_base, gse_uid, gse_id)
  } else {
    url_base = 'https://ftp.ncbi.nlm.nih.gov/geo/series/%s/%s/suppl/'
    url_spec = sprintf(url_base, gse_uid, gse_id)
  }

  return(url_spec)

}

#' List GEO Files
#'
#' Lists GEO series files from NCBI's FTP server.
#'
#' @param gse_id Character. The GEO series ID.
#' @param series Character. The type of series to access ('matrix', 'suppl', or 'supplementary').
#' @param full Logic. Whether to return full URLs.
#'
#' @return A character vector of file names or full URLs.
#'
#' @examples
#' listGSE('GSE129516', series = 'matrix', full = TRUE)
#'
#' @export
listGSE = function(gse_id, series = 'matrix', full = T) {

  series = tolower(series)
  match.arg(series, c('matrix', 'suppl', 'supplementary'))
  if (series == 'supplementary') series = 'suppl'

  url_gse = gse2url(gse_id, series)
  files = listURL(url_gse)

  if (full) files = paste0(url_gse, files)

  return(files)

}

#' Download GEO Files
#'
#' Downloads GEO series files from NCBI's FTP server.
#'
#' @param gse_id Character. The GEO series ID.
#' @param series Character. The type of series to download ('matrix' or 'suppl').
#'
#' @return No return value. Files are downloaded to the current working directory.
#'
#' @examples
#' downloadGSE('GSE129516', series = 'matrix')
#'
#' @export
downloadGSE = function(gse_id, series = 'matrix') {

  files = listGSE(gse_id, series)

  for (file in files) {

    status = tryCatch({
      each = download.file(
        file,
        destfile = basename(file),
        mode = 'wb',
        method = 'auto'
      )
      each == 0},
      error = function(e) return(F),
      warning = function(w) return(F)
    )

    if (!status) stop(sprintf('Failed to download %s!', file))

  }

  invisible()

}


