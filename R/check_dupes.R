#' Check to see whether duplicate records exist. A wrapper around \code{\link[janitor]{get_dupes}}.
#'
#' @param d The data file to be checked.
#' @param tool The pharmacometric tool the data is to be used with. Potential values are "nonmem" (the default).
#' @param id The unique identifier for individuals (defaults to "ID").
#' @param idv The independent variable (defaults to "TIME").
#' @param dv The dependent variable (defaults to "DV").
#' @param mdv Missing variable identifier (defaults to "MDV").
#' @param evid Event type identifier (defaults to "EVID").
#'
#' @return A tibble containing duplicates.
#'
#' @author Justin Wilkins, \email{justin.wilkins@@occams.com}
#'
#' @examples
#' \dontrun{
#'  check_dupes(nmdata)
#' }
#'
#' @export
#' @importFrom janitor get_dupes

check_dupes <- function(d,
                        tool="nonmem",
                        id = "ID",
                        idv = "TIME",
                        dv = "DV",
                        mdv = "MDV",
                        evid = "EVID") {

  x <- d[,c(id,idv,dv, mdv, evid)]
  out <- janitor::get_dupes(x)

  if(nrow(out)>0) {
    warning(paste(nrow(out), " identical combinations of ", id, ", ", idv, ", ", dv, ", ", mdv, ", ", evid, " found in this dataset.", sep=""))
  } else {
    warning(paste("No identifical combinations of ", id, ", ", idv, ", ", dv, ", ", mdv, ", ", evid, " found in this dataset.", sep=""))
  }
  out

}
