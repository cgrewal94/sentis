#' Outputs SPSS syntax
#'
#' Generates syntax for variable labels and MR Sets.
#' @param data Dataset containing the variable names and labels.
#' @param method Type of SPSS syntax to generate. \emph{Valid methods: "var_labels" and "mr_sets"}
#' @export

spss_syntax <- function(data, method) {
    
    if (nchar(data[1,2]) > 10) { # confirm columns are in correct order
        if (method == "var_labels") {
            nameVarLab <- data[[1]]
            labelVarLab <- data[[2]]
            
            # create variable that contains the variable labels syntax
            data$joined <- paste0("variable labels", " ", nameVarLab, " ", '"', nameVarLab, ".", " ", labelVarLab, '"', ".")
            return(data)  
            
        } else if (method == "mr_sets") {
            # separtate out variable and label columns to allow for easy string concatenation
            data <- tidyr::separate(data, col = 2, sep = "\\[", into = c("misc", "misc2")) %>%
                tidyr::separate(col = "misc2", sep = "\\]", into = c("value_label", "mr_label")) %>%
                tidyr::separate(col = 1, sep = "_", into = c("root", "number")) %>%
                select(-misc)
            
            # create variables for value label, MR Set label, and recode syntax
            data$mr_label <- stringr::str_c(data$root, ".", data$mr_label)
            data$number <- ifelse(data$number == "other", "96", data$number)
            data$value_label <- stringr::str_c(data$number, " ", '"', data$value_label, '"')
            data$recode <- str_c("recode", " ", data$root, "_", data$number, " ", "(1=", data$number, ")", "(0=sysmis).")
            
            return(data) 
            
        } else {
            # show error message if method passed in is invalid
            stop("Please specify a valid method")
        }   
    } else {
        stop("Column order is incorrect. Variable names should be in the first column.")
    }
    
}