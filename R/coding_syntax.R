#' Outputs Coding Syntax
#'
#' Generates SPSS syntax for manually coded responses.
#' @param data An Excel file with each tab containing the columns: id, variable name, code1, code2, code3. \emph{All columns should be present in each tab in the order shown.}
#' @export
#' 

coding_syntax <- function(data) {
    
    inner_coding_function <- function(inner_data) {
        variable <- colnames(inner_data)[2]
        
        inner_data %>%
            mutate_at(c("code1", "code2", "code3"), replace_na, replace = 0) %>%
            mutate(syntax_1 = paste0("if (id eq ", id, ") ", variable, "code1 eq ", code1, "."),
                   syntax_2 = paste0("if (id eq ", id, ") ", variable, "code2 eq ", code2, "."),
                   syntax_3 = paste0("if (id eq ", id, ") ", variable, "code3 eq ", code3, ".")) %>%
            mutate(syntax_1 = if_else(!str_detect(syntax_1, "eq 0."), syntax_1, NULL),
                   syntax_2 = if_else(!str_detect(syntax_2, "eq 0."), syntax_2, NULL),
                   syntax_3 = if_else(!str_detect(syntax_3, "eq 0."), syntax_3, NULL)) %>%
            pivot_longer(cols = starts_with("syntax"), names_to = "temp", values_to = "syntax", values_drop_na = TRUE) %>%
            select(-temp)
    }
    
    output <- map(data, ~ inner_coding_function(.x))
}