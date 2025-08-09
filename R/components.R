#' Create a tibble of non-recursive file list with information
#'
#' @param dir_ character. One or more paths to directories to inventory.
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
  stopifnot(inherits(dir_, 'character'))
  stopifnot(inherits(pattern_, 'NULL') | inherits(pattern_, 'character'))

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
#' @param tb data.frame/tibble. The data.frame returned by `k_ls`.
#' @param rm.unique logical. Whether or not to remove unique values. By default, it is `TRUE`.
#'
#' @returns tibble
#' @export
k_dupes <- function(tb, rm.unique = TRUE){
  stopifnot(inherits(tb, 'data.frame'))
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

  # add md5 column in to original dataset.
  tb <- tb %>%
    # add dupes b
    dplyr::left_join(y = potential_dups, by = "path")

  if(rm.unique){
    # create filter criteria of duplicated values.
    filtr_ <- tb$md5[duplicated(tb$md5)] |>
      unique()

    # execute filter
    tb <- tb[tb$md5 %in% filtr_,]
  }

  # return
  tb
}
