#' Check and summarize the numbers and types of events in the dataset.
#'
#' @param d The data file to be checked.
#' @param tool The pharmacometric tool the data is to be used with. Potential values are "nonmem" (the default).
#' @param id The unique identifier for individuals (defaults to "ID").
#' @param evid Event type identifier (defaults to "EVID").
#' @param c Record censor identifier (defaults to "C").
#' @param mdv Missing variable identifier (defaults to "MDV").
#'
#' @return A tibble containing a summary of events in the dataset.
#' \itemize{
#'   \item \code{EVID}: Event ID (analogous to NONMEM: 0=observation, 1=dose, 2=other, 3=reset, 4=dose and reset)
#'   \item \code{type}: Event type as text
#'   \item \code{unique_id}: Number of unique subjects
#'   \item \code{n}: Number of events
#'   \item \code{perc}: Percentage made up by this type
#'   \item \code{per_patient}: Number of rows of this type per patient
#'   \item \code{dropped}: Number of rows of this type flagged for censoring
#'   \item \code{dropped_perc}: Percentage of dropped rows in this type
#'   \item \code{dropped_X} (as many columns as unique values of censoring codes): Numbers of dropped records by drop code
#' }
#' @author Justin Wilkins, \email{justin.wilkins@@occams.com}
#'
#' @examples
#' \dontrun{
#'  check_events(nmdata)
#' }
#'
#' @export
#' @importFrom dplyr summarise n group_by_at left_join relocate %>%
#' @importFrom tidyr pivot_wider

check_events <- function(d,
                        tool="nonmem",
                        id = "ID",
                        evid = "EVID",
                        c=NULL,
                        mdv=NULL) {

  niddd    <- NULL
  dropflag <- NULL
  cccc     <- NULL
  dropped  <- NULL
  unique_id <- NULL

  d$niddd <- d[[id]]
  nid <- length(unique(d[[id]]))
  type = NA

  d$Rownum <- 1:nrow(d)

  out <- d %>%
    group_by_at(evid) %>%
    dplyr::summarise(
      unique_id = length(unique(niddd)),
      n = n(),
      perc = n()/nrow(d),
      per_patient = n()/nid)


  out$type <- NA
  out$type[out[[evid]]==0] <- "observation"
  out$type[out[[evid]]==1] <- "dose"
  out$type[out[[evid]]==2] <- "other"
  out$type[out[[evid]]==3] <- "reset"
  out$type[out[[evid]]==4] <- "reset and dose"

  pured <- d

  if(!is.null(d[[c]])) {
    d$cccc <- d[[c]]
    d[[c]] <- as.character(d[[c]])
    d$dropflag <- 0
    d$dropflag[grepl('^[A-Za-z]+$', d[[c]])] <- 1

    d[[c]] <- as.factor(as.character(d[[c]]))
    drops  <- d %>%
      group_by_at(c(evid, "dropflag")) %>%
      summarise(dropped=n())

    droptypes  <- d %>%
      filter(dropflag==1) %>%
      group_by_at(c(evid, "cccc")) %>%
      summarise(dropped=n()) %>%
      pivot_wider(names_from=cccc, values_from = dropped, names_prefix="dropped_")

    out <- out %>%
      left_join(drops[drops$dropflag==1,c(evid, "dropped")], by=evid)
    out$dropped_perc <- out$dropped / out$n

    out <- out %>%
      left_join(droptypes, by=evid)
  }


  events <- out %>%
    relocate(type, .before=unique_id)

  events

}
