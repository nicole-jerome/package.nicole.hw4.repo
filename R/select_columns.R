#' select_columns
#'
#' This function returns a data.frame containing only the specified columns. Columns can be specified as included (indicated by a positive value) or excluded (indicated by a negative value). Specified columns can be provided as names (character vector) or numeric indices. The function is a simplified version of dplyr::select().
#'
#' @param df The input object given by the user. It is expected to be a data.frame.
#' @param columns A character or numeric vector specifying which columns to include or exclude. If a value is negative, that column will be dropped instead of kept. Columns must exist in \code{df}.
#'
#' @return A filtered data.frame that contains only the specified columns, with the order of the columns being equivalent to the original data.frame (besides dropped columns).
#'
#' @examples
#' # Select columns by name
#' select_columns(mtcars, c("mpg", "hp"))
#'
#' # Select columns by index
#' select_columns(mtcars, c(1,5))
#'
#' # Exclude columns by name
#' select_columns(mtcars, -c("mpg", "hp"))
#'
#' #Exclude columns by index
#' select_columns(mtcars, -c(1,2))
#'
#' # Attempting to select a non-existent column triggers an error
#' \dontrun{
#' select_columns(mtcars, c("mpg", "blue_hair"))
#' }
#'
#' @export
select_columns = function(df, columns) {
  #check that the input is a data.frame
  df_check(df)

  #check that selection columns are provided
  if (missing(columns)) {
    stop("You must specify at least one column name or index.", call. = F)
  }

  #detect an exclusion case
  exclude = F

  #if column is numeric and negative, exclude it
  if (is.numeric(columns) && all(columns < 0)) {
    exclude <- TRUE
    columns <- abs(columns)
  }

  #if column is character and first element is "-", exclude it
  if (is.character(columns) && grepl("^-", columns[1])) {
    exclude <- TRUE
    columns <- sub("^-", "", columns)
  }

  #if columns are numeric indices, ensure they are within a valid range of the df provided
  #if columns are character elements, ensure that they exist
  if (is.numeric(columns)) {
    if (any(columns < 1 | columns > ncol(df))) {
      stop("Column indices are out of range. Data frame has ", ncol(df), " columns.", call. = F)
        }
  } else if (is.character(columns)) {
    missing_columns = setdiff(columns, names(df))
    if (length(missing_columns) > 0) {
      stop("The following columns do not exist in the data frame:",
           paste(missing_columns, collapse = ", "), call. = F)
    }
  }

  #perform the column selection (inclusion or exclusion)
  if (exclude) {
    selected_df <- df[, setdiff(names(df), columns), drop = FALSE]
    message("Excluded ", length(columns), " column(s). Remaining: ",
            ncol(selected_df), ".")
  } else {
    selected_df <- df[, columns, drop = FALSE]
    message("Selected ", ncol(selected_df), " of ", ncol(df), " columns.")
  }

  return(selected_df)
}
