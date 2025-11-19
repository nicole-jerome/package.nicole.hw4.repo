#' df_check
#'
#' This function makes sure that the input into the function is a data.frame. If not, it stops execution and gives an informative error message.
#'
#' @param df An object the user intends to pass through manipulation functions. It is expected to be a data.frame.
#' @return Invisibly returns TRUE if the input is a valid data.frame.
#' @keywords internal
#' @examples
#' df_check(mtcars)   # passes
#' \dontrun{
#' df_check(1:10)     # throws an error
#' }
df_check = function(df) {
  if (!is.data.frame(df)) {
    stop("Input must be a data.frame, but received object of class '",
         paste(class(df), collapse = ","),
         "'.", call. = F)
  }
  invisible(T)
}
