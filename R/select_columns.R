#' select_columns
#'
#' This function returns a data.frame containing only the specified columns. Columns can be specified as included (indicated by a positive value) or excluded (indicated by a negative value). Specified columns can be provided as names (character vector) or numeric indices. The function is a simplified version of dplyr::select().
#'
#' @param df The input object given by the user. It is expected to be a data.frame.
#' @param columns A character or numeric vector specifying which columns to include or exclude. If a value is negative, that column will be dropped instead of kept. Columns must exist in \code{df}.
#'
#' @return A filtered data.frame that contains only the specified columns, in the order of the original data.frame.
#'
#' @examples
#' # Select columns by name
#' select_columns(mtcars, c("mpg", "hp"))
#'
#' # Select columns by index
#' select_columns(mtcars, c(1,5))
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

  #if columns are numeric indices, ensure they are within a valid range of the df provided
  if (is.numeric(columns)) {
    if (any(columns < 1 | columns > ncol(df))) {
      stop("Column indices are out of range. Data frame has ", ncol(df), " columns.", call. = F)
        }
  }

  #if columns are character names, ensure they exist in df
  if (is.character(columns)) {
    missing_columns = setdiff(columns, names(df))
    if length(missing_columns > 0){
      stop("The following columns do not exist in the data frame: ",
           paste(missing_columns, collapse = ", "), call. = F)
    }
  }

  #perform the column selection
  selected_df = df[, columns, drop = F]

  #nice return message
  message("Selected ", ncol(selected_df), " of ", ncol(df), " columns.")

  return(selected_df)
}
