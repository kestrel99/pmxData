#' Check the dosing history in the dataset.
#'
#' @param d The data file to be checked.
#' @param tool The pharmacometric tool the data is to be used with. Potential values are "nonmem" (the default).
#' @param id The unique identifier for individuals (defaults to "ID").
#' @param idv The independent variable (defaults to "TIME").
#' @param dv The dependent variable (defaults to "DV").
#' @param mdv Missing variable identifier (defaults to "MDV").
#' @param evid Event type identifier (defaults to "EVID").
#' @param amt The dose amount variable (defaults to "AMT").
#' @param rate The dose rate variable (defaults to "RATE").
#' @param c Record censor identifier (defaults to "C").
#' @param sd_sens The multiple of the standard deviation to use for identifying anomalous RATE values from the duration of infusion, obtained as AMT/RATE (defaults to 1).
#'
#' @return A list of tibbles containing problematic rows in the dataset.
#'
#' @author Justin Wilkins, \email{justin.wilkins@@occams.com}
#'
#' @examples
#' \dontrun{
#'  check_doses(nmdata)
#' }
#'
#' @export
#' @importFrom dplyr filter bind_rows %>%
#' @importFrom purrr map_df
#' @importFrom stats sd

check_doses <- function(d,
                        tool="nonmem",
                        id = "ID",
                        idv="TIME",
                        dv = "DV",
                        mdv = "MDV",
                        evid = "EVID",
                        amt="AMT",
                        rate="RATE",
                        c="C",
                        sd_sens = 1) {

  dropflag <- NULL

  pured <- d

  nid <- length(unique(d[[id]]))
  type <- NULL
  superfluous_dose <- NULL
  . = NULL

  d$Rownum <- 1:nrow(d)

  suppressWarnings(d[[idv]] <- as.numeric(as.character(d[[idv]])))
  suppressWarnings(d[[dv]] <- as.numeric(as.character(d[[dv]])))
  suppressWarnings(d[[amt]] <- as.numeric(as.character(d[[amt]])))
  suppressWarnings(d[[rate]] <- as.numeric(as.character(d[[rate]])))

  ## nonzero conc, MDV=1
  dd <- d[d$dropped==0,]
  suppressWarnings(dd[[dv]] <- as.numeric(as.character(dd[[dv]])))
  dd[[dv]][is.na(dd[[dv]])] <- 0

  suppressWarnings(dd[[amt]] <- as.numeric(as.character(dd[[amt]])))
  dd[[amt]][is.na(dd[[amt]])] <- 0

  if(rate %in% colnames(dd)) {
    suppressWarnings(dd[[rate]] <- as.numeric(as.character(dd[[rate]])))
    dd[[rate]][is.na(dd[[rate]])] <- 0
  }

  tmp <- dd[dd[[dv]]!=0 & dd[[mdv]]==1 & dd[[evid]]==0,]
  dv_nonzero_mdv1 <- pured[pured$Rownum %in% tmp$Rownum,]

  ## zero obs when MDV=0
  tmp <- dd[dd[[dv]]==0 & dd[[mdv]]==0 & dd[[evid]]==0,]
  dv_zero_mdv0 <- pured[pured$Rownum %in% tmp$Rownum,]

  ## zero dose when EVID=1
  tmp <- dd[dd[[amt]]==0 & dd[[evid]]==1,]
  amt_zero_evid1 <- pured[pured$Rownum %in% tmp$Rownum,]

  ## nonzero dose when EVID=0
  tmp <- dd[dd[[amt]]!=0 & dd[[evid]]==0,]
  amt_nonzero_evid0 <- pured[pured$Rownum %in% tmp$Rownum,]

  ## zero rate when amt nonzero
  tmp <- dd[dd[[rate]]==0 & dd[[evid]]==1 & dd[[amt]]!=0,]
  rate_zero_amt_nonzero<- pured[pured$Rownum %in% tmp$Rownum,]


  ## incorrect RATE

  if(rate %in% colnames(dd)) {
    r <- dd[dd[[evid]]==1,]
    r$DUR <- r[[amt]]/r[[rate]]

    rr <- r[r$DUR < (mean(r$DUR, na.rm=T) - sd_sens*sd(r$DUR, na.rm=T)) | r$DUR > (mean(r$DUR, na.rm=T) + sd_sens*sd(r$DUR, na.rm=T)),]
    rate_anomaly <- pured[pured$Rownum %in% rr$Rownum,]
  }

  strip_doses <- function(x) {
    obs <- x[x[[evid]]==0,]
    last_obs_time <- obs[[idv]][nrow(obs)]
    x$superfluous_dose <- 0
    x$superfluous_dose[x[[idv]]>last_obs_time & x[[evid]]==1] <- 1
    x
  }

  dd$dropflag <- 0
  if(!is.null(dd[[c]])) {
    dd$cccc <- dd[[c]]
    dd[[c]] <- as.character(dd[[c]])
    dd$dropflag[grepl('^[A-Za-z]+$', dd[[c]])] <- 1
  }

  superfluous_doses <- dd %>%
    split(.[[id]]) %>%
    map_df(~ strip_doses(.)) %>%
    bind_rows %>%
    filter(superfluous_dose==1) %>%
    filter(dropflag==0)

  out <- list(
    dv_nonzero_mdv1 = dv_nonzero_mdv1,
    dv_zero_mdv0 = dv_zero_mdv0,
    amt_zero_evid1 = amt_zero_evid1,
    amt_nonzero_evid0 = amt_nonzero_evid0,
    superfluous_doses=superfluous_doses
  )

  if(rate %in% colnames(dd)) {
    out <- list(
      dv_nonzero_mdv1 = dv_nonzero_mdv1,
      dv_zero_mdv0 = dv_zero_mdv0,
      amt_zero_evid1 = amt_zero_evid1,
      amt_nonzero_evid0 = amt_nonzero_evid0,
      superfluous_doses=superfluous_doses,
      rate_zero_amt_nonzero = rate_zero_amt_nonzero,
      rate_anomaly = rate_anomaly
    )
  }

  if(nrow(dv_nonzero_mdv1)>0) warning(paste(nrow(dv_nonzero_mdv1), " non-dropped rows have ", dv, "!=0 and ", mdv, "==1 and ", evid, "==0.", sep=""))
  if(nrow(dv_zero_mdv0)>0) warning(paste(nrow(dv_zero_mdv0), " non-dropped rows have ", dv, "==0 and ", mdv, "==0 and ", evid, "==0.", sep=""))
  if(nrow(amt_zero_evid1)>0) warning(paste(nrow(amt_zero_evid1), " non-dropped rows have ", amt, "==0 and ", evid, "==1.", sep=""))
  if(nrow(amt_nonzero_evid0)>0) warning(paste(nrow(amt_nonzero_evid0), " non-dropped rows have ", amt, ">0 and ", evid, "!=1.", sep=""))
  if(nrow(superfluous_doses)>0) warning(paste(nrow(superfluous_doses), " non-dropped dose records are present after final observations.", sep=""))
  if(rate %in% colnames(dd)) {
    if(nrow(rate_zero_amt_nonzero)>0) warning(paste(nrow(rate_zero_amt_nonzero), " non-dropped rows have ", rate, "==0 and ", amt, ">0 and ", evid, "==1.", sep=""))
    if(nrow(rate_anomaly)>0) warning(paste(nrow(rate_anomaly), " non-dropped dose records have anomalous ", rate, " values.", sep=""))
  }

  out
}
