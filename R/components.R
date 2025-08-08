#' Create a tibble of non-recursive file list with information
#'
#' @param dir_ string. Path to directory.
#' @param pattern_ NULL or string. Pattern to feed into list.files.
#' @returns tibble
#' @export
#'
#' @examples
#' k_ls(dir_ = "~/Downloads")
k_ls <- function(
    dir_,
    pattern_ = NULL
    ){
  stopifnot(is.character(dir_))
  stopifnot(is.null(pattern_) | is.character(pattern_))

  # obtain list of file in dir, turn into tibble.
  tb <- tibble::tibble(path = list.files(dir_,
                                        pattern = pattern_,
                                        full.names = TRUE),
                       pattern = pattern_) %>%
    dplyr::mutate(is_dir = dir.exists(path),
                  size = ifelse(is_dir,
                                as.double(NA),
                                file.size(path)))


  # return
  tb
}
