#' Identifies speeders in a survey
#'
#' Flags respondents who have completed the survey faster than 30% of the median.
#' @param data Dataset containing the responses of \strong{all} survey participants.
#' @export

top_speeders <- function(data){
  
  colnames(data) <- gsub("\\[", "_", gsub("\\]$", "", colnames(data))) # change brackets in variable names to underscores
  
  median_df <- data %>%
    dplyr::filter(!is.na(submitdate)) %>% # filter out individuals who have not completed the survey
    dplyr::select(interviewtime)
  
  median_time <- median(median_df$interviewtime)
  
  data <- data %>%
    dplyr::mutate(speeder = dplyr::if_else(!is.na(submitdate) & interviewtime < 0.3 * median_time, 1, NULL))
}
