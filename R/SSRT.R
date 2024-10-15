#' @title meanSSRT
#'
#' @description Estimates the SSRT using the mean of all SSDs in the
#' experiment. This calculation hinges upon the assumption that p(response |
#' signal = 0.5)
#'
#' @import magrittr
#' @import dplyr
#'
#' @param dat a dataframe containing vp number, rt, accuracy, the conditions
#' and stop-signal delays
#' @param vp_num Name of the column containing the vp identifier.
#' @param type the column that reflects the current trial type (stop vs. go)
#' @param corr boolean that reflects wether the current trial is Correct
#' @param ssd Name of the column containing the current stop signal delay
#' @param rt Reaction times
#' @return a dataframe containing an estimate of the SSRT, the mean reaction
#' times on go trials and the mean stop signal delay for each VP
#' @examples dat <- meanSSRT(dat)
#' @export
meanSSRT <- function(
    dat,
    RTexc = FALSE,
    RTmin = 100,
    RTmax = 600) {
  if (RTexc) {
    dat <- dat %>% filter(between(rt, RTmin, RTmax))
  }
  return(
    merge(
      dat[dat$type == "go", ] %>%
        group_by(vp_num) %>%
        summarise(meanRT = round(mean(rt))),
      dat[dat$type == "stop", ] %>%
        group_by(vp_num) %>%
        summarise(meanSSD = round(mean(ssd))),
      by = "vp_num"
    ) %>% mutate(SSRT = meanRT - meanSSD)
  )
}

#' @title intSSRT
#'
#' @description utilises the RT distribution of correct Go Trials in a
#' Stop-Signal task to estimate the Stop-Signal Reaction Time
#'
#' @import magrittr
#' @import dplyr
#'
#' @param dat a dataframe containing vp number, rt, accuracy, the conditions
#' and stop-signal delays
#' @param vp_num Name of the column containing the vp identifier.
#' @param type the column that reflects the current trial type (stop vs. go)
#' @param corr boolean that reflects wether the current trial is Correct
#' @param ssd Name of the column containing the current stop signal delay
#' @param rt Reaction times
#' @param replace_slow boolean that indicates whether slow values should be
#' raplaced with the slowest correct RT, to correct distribution (see
#' Verbruggen et al., 2019)
#' @return a dataframe containing the vp identifier, an estimate of the SSRT,
#' nth reaction time, used to compute the SSRT, the mean Stop Signal delay,
#' the p(response | signal) and the estimated SSRT.
#' @examples dat <- intSSRT(dat)
#' @export
intSSRT <- function(
    dat,
    replace_slow = TRUE) {
  if (replace_slow) {
    for (vp_num in unique(dat$vp_num)) {
      dat$rt[dat$vp_num == vp_num & dat$slow == TRUE] <- max(dat$rt[dat$vp_num & dat$slow == FALSE])
    }
  }

  dat_SSRT <- data.frame(
    vp_num = integer(),
    nthRT = integer(),
    meanSSD = integer(),
    pResp = numeric()
  )

  for (vp_num in unique(dat$vp_num)) {
    pResp <- round(mean(dat$corr[dat$vp_num == vp_num & dat$type == "stop", ]), 1)
    GoRTs <- dat$RT[order(dat$rt[dat$vp_num == vp_num & dat$type == "go"])]
    nthRT <- GoRTs[as.integer(pResp * length(GoRTs))]
    dat_SSRT <-
      rbind(
        dat_SSRT,
        data.frame(
          vp_num = vp_num,
          nthRT = round(nthRT),
          meanSSD = round(meanSSD),
          pResp = round(pResp * 100, 1)
        )
      )
  }
  dat_SSRT <- dat_SSRT %>% mutate(SSRT = nthRT - meanSSD)
  return(dat_SSRT)
}
