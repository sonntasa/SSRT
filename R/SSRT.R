#' @title meanSSRT
#'
#' @description Estimates the SSRT using the mean of all SSDs in the
#' experiment. This calculation hinges upon the assumption that p(response |
#' signal = 0.5)
#'
#' @import magrittr
#' @import dplyr
#'
#' @param dat_tmp a dataframe containing vp number, rt, accuracy, the condition
#' and stop-signal delays
#' @param vp_num Name of the column containing the vp identifier.
#' @param cond Name of the column that reflects the current condition stop =
#' c(3, 4); go = c(1, 2)
#' @param correct_code Name of the column containing the accuracy (1 = correct)
#' @param ssd Name of the column containing the current stop signal delay
#' @return a dataframe containing an estimate of the SSRT, the mean reaction
#' times on go trials and the mean stop signal delay for each VP
#' @examples dat <- meanSSRT(dat)
#' @export
meanSSRT <- function(dat_tmp) {
  tmp <- dat_tmp
  stop_trials <- tmp %>% dplyr::filter(cond %in% c(3, 4))
  go_trials <- tmp %>% dplyr::filter(cond %in% c(1, 2), correct_code == 2)
  go_trials <- go_trials %>%
    group_by(vp_num) %>%
    summarise(meanRT = round(mean(RT)))
  stop_trials <- stop_trials %>%
    group_by(vp_num) %>%
    summarise(meanSSD = round(mean(ssd)))
  tmp <- merge(go_trials, stop_trials, by = "vp_num")
  tmp <- tmp %>% mutate(SSRT = meanRT - meanSSD)
  return(tmp)
}

#' @title intSSRT
#'
#' @description utilises the RT distribution of correct Go Trials in a
#' Stop-Signal task to estimate the Stop-Signal Reaction Time
#'
#' @import magrittr
#' @import dplyr
#'
#' @param dat_tmp a dataframe containing vp number, rt, accuracy, the condition
#' and stop-signal delays
#' @param vp_num Name of the column containing the vp identifier.
#' @param cond Name of the column that reflects the current condition stop =
#' c(3, 4); go = c(1, 2)
#' @param correct_code Name of the column containing the accuracy (1 = correct)
#' @param ssd Name of the column containing the current stop signal delay
#' @return a dataframe containing an estimate of the SSRT, the mean reaction
#' times on go trials, the mean stop signal delay and the mean p(reponse |
#' signal) for each VP
#' @examples dat <- meanSSRT(dat)
#' @export
intSSRT <- function(dat_tmp) {
  tmp <- dat_tmp
  stop_trials <- tmp %>% dplyr::filter(cond %in% c(3, 4))
  go_trials <- tmp %>% dplyr::filter(cond %in% c(1, 2), correct_code == 1)
  # NOTE: Right now, this includes all erroneous responses, including
  # direction errors.
  stop_trials <- stop_trials %>%
    group_by(vp_num) %>%
    summarise(
      nResp = sum(correct_code != 2),
      nGes = n(),
      meanSSD = mean(ssd)
    ) %>%
    mutate(prob = nResp / nGes)
  tmp <- tibble(vp_num = integer(), nthRT = numeric())
  for (vp_num in unique(go_trials$vp_num)) {
    GoRTs <- go_trials$RT[order(go_trials$RT[go_trials$vp_num == vp_num])]
    prob <- stop_trials$prob[stop_trials$vp_num == vp_num]
    tmp <- rbind(
      tmp,
      tibble(
        vp_num = vp_num,
        nthRT = round(GoRTs[as.integer(prob * length(GoRTs))])
      )
    )
  }
  tmp <- merge(tmp, stop_trials %>% select(c(vp_num, meanSSD, prob)), by = "vp_num") %>% head()
  tmp <- tmp %>% mutate(SSRT = round(nthRT - meanSSD))
  tmp <- tmp %>%
    mutate(
      nthRT = round(nthRT),
      meanSSD = round(meanSSD),
      prob = round(prob * 100, 1),
    ) %>%
    select(vp_num, nthRT, meanSSD, SSRT, prob)
  return(tmp)
}
