#' Identifies straight liners in a survey.
#'
#' Identifies repondents who have straight lined through array-type questions.
#' @param data Dataset containing the responses of \strong{all} survey participants.
#' @param qcode Root question code including underscore. Multiple codes can be passed in as a character vector. \emph{Example: c("a2_", "a8_")}
#' @export

straight_liners <- function(data, qcode) {
    
    # change brackets in variable names to underscores
    
    colnames(data) <- gsub("\\[", "_", gsub("\\]$", "", colnames(data)))
    
    straight_liner <- function(data, qcode) {
        varname <- paste(qcode, "SL", sep = "")
        
        data <- data %>%
            dplyr::mutate(!!varname := dplyr::if_else(apply(data[, grep(qcode, colnames(data))], 1, sd) == 0, 1, 0, 0))   
    }
    
    # map qcodes onto data and then reduce into one dataframe maintaining only unique columns
    
    data_list <- purrr::map(qcode, ~ straight_liner(data, .x))
    data_combined <- purrr::reduce(data_list, merge)
}