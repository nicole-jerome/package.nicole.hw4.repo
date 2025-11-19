#' select_columns
#'
#' This function returns a data.frame containing only the specified columns. Users can provide \code{columns} to include specific columns, or \code{exclude} to remove specific columns. Specified columns can be provided as names (character vector) or numeric indices. The function is a simplified version of dplyr::select() that is written in base R.
#'
#' @param df The input object given by the user. It is expected to be a data.frame.
#' @param columns A character or numeric vector specifying which columns to include (can be column names or numeric indices). Cannot be used together with \code{exclude}. Columns must exist in \code{df}.
#' @param exclude A character or numeric vector specifying which columns to exclude (can be column names or numeric indices). Cannot be used together with \code{columns}. Columns must exist in \code{df}.
#'
#' @return A filtered data.frame that contains only the specified columns, with the order of the columns being equivalent to the original data.frame (besides dropped columns).
#'
#' @examples
#' # Select columns by name
#' select_columns(mtcars, columns = c("mpg", "hp"))
#'
#' # Select columns by index
#' select_columns(mtcars, columns = c(1,5))
#'
#' # Exclude columns by name
#' select_columns(mtcars, exclude = c("mpg", "hp"))
#'
#' #Exclude columns by index
#' select_columns(mtcars, exclude = c(1,2))
#'
#' # Attempting to select a non-existent column triggers an error
#' \dontrun{
#' select_columns(mtcars, columns = c("mpg", "blue_hair"))
#' }
#'
#' @export
select_columns = function(df, columns = NULL, exclude = NULL) {
  #check that the input is a data.frame
  df_check(df)

  #prevent both arguments from being used
  if (!is.null(columns) && !is.null(exclude)) {
    stop("Use either 'columns' or 'exclude', not both.", call. = F)
  }

  #require at least one argument
  if (is.null(columns) && is.null(exclude)) {
    stop("You must provide either 'columns' or 'exclude'.", call. = F)
  }

  #HANDLING INCLUSION WITH COLUMNS ARGUEMENT
  if (!is.null(columns)) {

    #numeric inclusion
    if (is.numeric(columns)) {
      if (any(columns < 1 | columns > ncol(df))) {
        stop("Given 'column' indices are out of range. Data frame has ",
             ncol(df), " columns.", call. = F)
      }
      return(df[, columns, drop = F])
    }

    #character inclusion
    if (is.character(columns)) {
      missing_cols <- setdiff(columns, names(df))
      if (length(missing_cols) > 0) {
        stop("The following columns do not exist in the data frame: ",
             paste(missing_cols, collapse = ", "),
             call. = F)
      }
      return(df[, columns, drop = F])
    }

    stop("'columns' must be numeric or character.", call. = F)
  }

  #HANDLING EXCLUSION WITH EXCLUDE ARGUEMENT
  if (!is.null(exclude)) {

    #numeric exclusion
    if (is.numeric(exclude)) {
      if (any(exclude < 1 | exclude > ncol(df))) {
        stop("Given 'exclude' indices out of range. Data frame has ",
             ncol(df), " columns.", call. = FALSE)
      }
      keep <- setdiff(seq_len(ncol(df)), exclude)
      return(df[, keep, drop = FALSE])
    }

    #character exclusion
    if (is.character(exclude)) {
      missing_cols <- setdiff(exclude, names(df))
      if (length(missing_cols) > 0) {
        stop("The following columns do not exist in the data frame: ",
             paste(missing_cols, collapse = ", "),
             call. = FALSE)
      }
      keep <- setdiff(names(df), exclude)
      return(df[, keep, drop = FALSE])
    }

    stop("'exclude' must be numeric or character.", call. = FALSE)
  }
}
