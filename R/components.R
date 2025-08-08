#' Create tibble of file list with information.
#'
#' @param dir_ string. Path to directory.
#' @returns tibble
#' @export
#'
#' @examples
#' k_ls(dir_ = "~/Downloads")
k_ls <- function(dir_){
  stopifnot(dir_, is.character())

  tb <- tibble::tibble(dir = list.files(dir_))

  # return
  tb
}
