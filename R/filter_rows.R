#' filter_rows
#'
#' This function returns rows of a data.frame that satisfy a logical condition. It is a simplified version of dplyr::filter() that is written in base R.
#'
#' @param df The input object given by the user. It is expected to be a data.frame.
#' @param condition A logical expression evaluated in the context of \code{df}. Logical conditions can be strung together by & or |.
#'
#' @return A filtered data.frame containing only the rows where the condition is TRUE.
#' @examples
#' #Keep only cars with mpg > 20
#' filter_rows(mtcars, mpg > 20)
#'
#' #Keep only cars with mpg > 20 and cyl < 6
#' filter_rows(mtcars, mpg > 20 & cyl < 6)
#'
#' @export
filter_rows = function(df, condition) {
  #check that the input is a data.frame
  df_check(df)

  #count number of rows
  n_total = nrow(df)

  #safely evaluate the condition within the df
  eval_condition = eval(substitute(condition), envir = df)

  #make sure the condition is logical and of the right length
  if (!is.logical(eval_condition) ||
      length(eval_condition) != n_total) {
    stop("Condition must evaluate to a logical vector of length equal to nrow(df).",
         call. = FALSE)
  }

  #perform the subsetting
  filtered_df = df[eval_condition, , drop = F]

  #nice return message
  rows_kept = nrow(filtered_df)
  message("Kept ", rows_kept, " of ", n_total, " rows.")

  #return new df
  return(filtered_df)
}










