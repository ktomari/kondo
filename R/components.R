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

  fl <- list.files(dir_,
                   pattern = pattern_,
                   full.names = TRUE,
                   recursive = TRUE)

  tb <- purrr::map(fl, file.info) %>%
    dplyr::bind_rows() %>%
    tibble::rownames_to_column(var = "path") %>%
    dplyr::mutate(pattern = pattern_) %>%
    dplyr::select(path, isdir, size, mtime, ctime, atime) %>%
    dplyr::mutate(size = dplyr::case_when(
      isdir ~ as.double(NA),
      .default = size
    ))

  # return
  tb
}

#' Identify duplicates files in tibble contain columns `path` and `size`.
#'
#' @description
#' If hash is present in the `md5`column, then it is duplicate!
#'
#' @param tb
#'
#' @returns tibble
#' @export
k_dupes <- function(tb){
  stopifnot(is.data.frame(tb))
  stopifnot("path" %in% names(tb))
  stopifnot("size" %in% names(tb))

  # add column that recognizes duplicate filenames
  tb <- tb %>%
    dplyr::mutate(filename = stringr::str_extract(path, "[^/]+$")) %>%
    dplyr::mutate(name_dupe = duplicated(filename) |
                    duplicated(filename,
                               fromLast = TRUE))

  # Group by size first
  potential_dups <- tb %>%
    dplyr::select(path, size) %>%
    dplyr::filter(!is.na(size)) %>%
    dplyr::group_by(size) %>%
    dplyr::filter(dplyr::n() > 1)

  # Calculate hashes only for potential duplicates
  potential_dups <- potential_dups %>%
    dplyr::mutate(
      md5 = purrr::map_chr(
      path,
      ~digest::digest(.x,
                      algo = "md5",
                      file = TRUE))) %>%
    dplyr::group_by(size, md5) %>%
    dplyr::filter(n() > 1) %>%
    dplyr::ungroup() %>%
    dplyr::select(-size)

  # return
  tb %>%
    # add dupes b
    dplyr::left_join(y = potential_dups, by = "path")
}

# tb <- tibble::tibble(
#   path = c("~/Downloads/x.csv", "~/Downloads/y.csv"),
#   size = c(898989, 898989)
# )
#
# k_dupes(tb = tb)
