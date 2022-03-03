#' Outputs SPSS tables syntax (Work in progress)
#'
#' Generates syntax for custom tables in SPSS
#' @param variable Name of the row variable.
#' @param type Type of custom table. Types currently supported are: \itemize{
#'  \item Basic tables with values sorted in ascending order (Default)
#'  \item Tables with T2B and B2B}  
#' @param title Title of the table.
#' @param banner Character string specifying banner points seperated by commas.
#' @param significance When \code{significance = TRUE}, the default, signficance tables will be included.
#' @examples
#' spss_tables("a1", "net", "Example title", "bannerPoint1, bannerPoint2, bannerPoint3", TRUE)
#' @export

spss_tables <- function (variable, type, title, banner, significance = TRUE) {
    
    # parse character string of banner points to useable SPSS syntax
    banner1 <- stringr::str_replace_all(banner, pattern = ",", "")
    banner2 <- stringr::str_c(str_replace_all(banner, pattern = ",", " [C] +"), " [C]")
    
    if (significance) {
        
        if (!is.na(type) & type == "net") {
            df <- tibble(syntax = c("CTABLES", 
                                    paste(" /VLABELS VARIABLES=", variable, "total", banner1, "DISPLAY=LABEL"),
                                    paste(" /PCOMPUTE &cat1 = EXPR([1] + [2])"),
                                    paste(" /PPROPERTIES &cat1 LABEL = 'INSERT LABEL (NET)' FORMAT=COUNT F40.0, COLPCT.COUNT PCT40.0"),
                                    paste("  HIDESOURCECATS=NO"),
                                    paste(" /PCOMPUTE &cat2 = EXPR([4] + [5])"),
                                    paste(" /PPROPERTIES &cat2 LABEL = 'INSERT LABEL (NET)' FORMAT=COUNT F40.0, COLPCT.COUNT PCT40.0"),
                                    paste("  HIDESOURCECATS=NO"),
                                    paste(" /TABLE", variable, "[C][COUNT F40.0, COLPCT.COUNT PCT40.0, TOTALS[COUNT F40.0, UCOUNT F40.0]] BY total [C]"),
                                    paste(" +", banner2 ),
                                    paste(" /SLABELS POSITION=ROW VISIBLE=NO"),
                                    paste(" /CATEGORIES VARIABLES=", variable, "[5, 4, 3, 2, 1, &cat1, &cat2, OTHERNM] EMPTY=INCLUDE TOTAL=YES LABEL='Count                      Unwtd Count' POSITION=BEFORE"),
                                    paste(" /CATEGORIES VARIABLES=total", banner1, "ORDER=A KEY=VALUE EMPTY=INCLUDE"),
                                    paste(" /TITLES"),
                                    paste0("  TITLE='", title, "'"),
                                    paste(" /COMPARETEST TYPE=PROP ALPHA=0.05 ADJUST=NONE ORIGIN=COLUMN INCLUDEMRSETS=YES"),
                                    paste("  CATEGORIES=ALLVISIBLE MERGE=NO."),
                                    paste("")))
            
        } else {
            df <- tibble(syntax = c("CTABLES", 
                                    paste(" /VLABELS VARIABLES=", variable, "total", banner1, "DISPLAY=LABEL"),
                                    paste(" /TABLE", variable, "[C][COUNT F40.0, COLPCT.COUNT PCT40.0, TOTALS[COUNT F40.0, UCOUNT F40.0]] BY total [C]"),
                                    paste(" +", banner2),
                                    paste(" /SLABELS POSITION=ROW VISIBLE=NO"),
                                    paste(" /CATEGORIES VARIABLES=", variable, "ORDER=A KEY=VALUE EMPTY=INCLUDE TOTAL=YES LABEL='Count                      Unwtd Count' POSITION=BEFORE"),
                                    paste(" /CATEGORIES VARIABLES=total", banner1, "ORDER=A KEY=VALUE EMPTY=INCLUDE"),
                                    paste(" /TITLES"),
                                    paste0("  TITLE='", title, "'"),
                                    paste(" /COMPARETEST TYPE=PROP ALPHA=0.05 ADJUST=NONE ORIGIN=COLUMN INCLUDEMRSETS=YES"),
                                    paste("  CATEGORIES=ALLVISIBLE MERGE=NO."),
                                    paste("")))  
        }
        
    } else {
        
        if (!is.na(type) & type == "net") {
            df <- tibble(syntax = c("CTABLES", 
                                    paste(" /VLABELS VARIABLES=", variable, "total", banner1, "DISPLAY=LABEL"),
                                    paste(" /PCOMPUTE &cat1 = EXPR([1] + [2])"),
                                    paste(" /PPROPERTIES &cat1 LABEL = 'INSERT LABEL (NET)' FORMAT=COUNT F40.0, COLPCT.COUNT PCT40.0"),
                                    paste("  HIDESOURCECATS=NO"),
                                    paste(" /PCOMPUTE &cat2 = EXPR([4] + [5])"),
                                    paste(" /PPROPERTIES &cat2 LABEL = 'INSERT LABEL (NET)' FORMAT=COUNT F40.0, COLPCT.COUNT PCT40.0"),
                                    paste("  HIDESOURCECATS=NO"),
                                    paste(" /TABLE", variable, "[C][COUNT F40.0, COLPCT.COUNT PCT40.0, TOTALS[COUNT F40.0, UCOUNT F40.0]] BY total [C]"),
                                    paste(" +", banner2 ),
                                    paste(" /SLABELS POSITION=ROW VISIBLE=NO"),
                                    paste(" /CATEGORIES VARIABLES=", variable, "[5, 4, 3, 2, 1, &cat1, &cat2, OTHERNM] EMPTY=INCLUDE TOTAL=YES LABEL='Count                      Unwtd Count' POSITION=BEFORE"),
                                    paste(" /CATEGORIES VARIABLES=total", banner1, "ORDER=A KEY=VALUE EMPTY=INCLUDE"),
                                    paste(" /TITLES"),
                                    paste0("  TITLE='", title, "'."),
                                    paste("")))
            
        } else {
            df <- tibble(syntax = c("CTABLES", 
                                    paste(" /VLABELS VARIABLES=", variable, "total", banner1, "DISPLAY=LABEL"),
                                    paste(" /TABLE", variable, "[C][COUNT F40.0, COLPCT.COUNT PCT40.0, TOTALS[COUNT F40.0, UCOUNT F40.0]] BY total [C]"),
                                    paste(" +", banner2),
                                    paste(" /SLABELS POSITION=ROW VISIBLE=NO"),
                                    paste(" /CATEGORIES VARIABLES=", variable, "ORDER=A KEY=VALUE EMPTY=INCLUDE TOTAL=YES LABEL='Count                      Unwtd Count' POSITION=BEFORE"),
                                    paste(" /CATEGORIES VARIABLES=total", banner1, "ORDER=A KEY=VALUE EMPTY=INCLUDE"),
                                    paste(" /TITLES"),
                                    paste0("  TITLE='", title, "'."),
                                    paste("")))  
        }
    }
    
}