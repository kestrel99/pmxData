#' Check and summarize the numbers and types of events in the dataset.
#'
#' @param d The data file to be checked.
#' @param tool The pharmacometric tool the data is to be used with. Potential values are "nonmem" (the default).
#' @param id The unique identifier for individuals (defaults to "ID").
#' @param evid Event type identifier (defaults to "EVID").
#' @param c Record censor identifier (defaults to "C").
#'
#' @return A tibble containing a summary of events in the dataset.
#'
#' @author Justin Wilkins, \email{justin.wilkins@@occams.com}
#'
#' @examples
#' \dontrun{
#'  check_events(nmdata)
#' }
#'
#' @export
#' @importFrom dplyr summarise n group_by_at left_join relocate %>%

check_events <- function(d,
                        tool="nonmem",
                        id = "ID",
                        evid = "EVID",
                        c="C") {

  nid <- length(unique(d[[id]]))
  type = NA

  d$Rownum <- 1:nrow(d)

  out <- d %>%
    group_by_at(evid) %>%
    dplyr::summarise(
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

  d[[c]] <- as.character(d[[c]])
  d$dropped <- 0
  d$dropped[grepl('^[A-Za-z]+$', d[[c]])] <- 1

  d[[c]] <- as.factor(as.character(d[[c]]))
  drops  <- d %>%
    group_by_at(c(evid, "dropped")) %>%
    summarise(drops=n())

  out <- out %>%
    left_join(drops[drops$dropped==1,c(evid, "drops")], by=evid)
  out$drop_perc <- out$drops / out$n

  events <- out %>%
    relocate(type, .before=n)

  events

}
