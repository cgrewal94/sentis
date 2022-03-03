#' Reformat Correlation Matrix
#'
#' Reformats the correlation matrix produced by SPSS into a more user-friendly format.
#' @param input_file Relative path of the correlation matrix.
#' @param cell_range Range of cells containing the relevant data. This is column-based and not row-based. \emph{Example: "A2:C35"}
#' @param base_size The total base size used to scale the correlations.
#' @export

reformat_correlations <- function(input_file, cell_range, base_size) {
    correlations_raw <- read_xlsx(input_file, range = cell_range)
    
    correlations_processed <- correlations_raw %>%
        rename(Question = 1, measure_name = 2, value = 3) %>%
        fill(Question) %>%
        pivot_wider(names_from = measure_name, values_from = value) %>%
        tail(-1) %>%
        mutate(`Pearson Correlation` = as.numeric(str_replace_all(`Pearson Correlation`, "\\*", "")),
               Base = as.numeric(N),
               abs_correlation = abs(`Pearson Correlation`),
               Rank = rank(-abs_correlation),
               scale_factor = Base/base_size,
               `Adjusted Pearson Correlation` = scale_factor * `Pearson Correlation`,
               abs_adj_correlation = abs(`Adjusted Pearson Correlation`),
               `Adjusted Rank` = rank(-abs_adj_correlation)) %>%
        select(1, 5, 2, 7, `Adjustment Factor` = scale_factor, 9, 11)
}