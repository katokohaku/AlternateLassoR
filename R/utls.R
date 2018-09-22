#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @importFrom foreach %do%
#' @export
foreach::`%do%`

#' Same function as 'catf()' in BBmisc except for returning [str]
#'
#' @param ...     See sprintf
#' @param file    See cat. Default is ""
#' @param append  See cat. Default is FALSE.
#' @param newline Append newline at the end? Default is TRUE.
#'
#' @export

catf <- function (..., file = "", append = FALSE, newline = TRUE)
{
  msg <- sprintf(...)
  cat(msg, ifelse(newline, "\n", ""),
      sep = "", file = file, append = append)
  invisible(msg)
}

