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

#' List, retrieve metadata, and store information about all user directories and files
#'
#' @param user_ character. Set origin user's name.
#' @param ignore_library logical. Ignore user /Library.
#' @param ignore_apps logical. Ignore user /Applications.
#' @param ignore_hidden logical. Ignore hidden files (which begin with period).
#' @param min_size numeric. Ignore files under a certain size (bytes).
#' @param include_icloud logical. Include iCloud files. This will probably significantly increase scanning time.
#' @param savepath character. The file path to the output directory. By default, it saves to ~/Downloads.
#' @param pattern_ character. Useful for narrowing down certain filetypes, eg. ".pdf$". See pattern in arguments for \link[base]{list.files}.
#' @param date_ character. The date that should be appended to any files. By default it does `format(Sys.time(), "%Y_%m_%d")`.
#'
#' @returns tibble
#' @export
k_user_ls <- function(
    user_ = NULL,
    ignore_library = TRUE,
    ignore_apps = TRUE,
    ignore_hidden = TRUE,
    min_size = 1000,
    include_icloud = TRUE,
    savepath = "~/Downloads",
    pattern_ = NULL,
    date_ = NULL
){
  stopifnot(inherits(user_, "character")
            | inherits(user_, "NULL"))
  stopifnot(inherits(ignore_library, "logical"))
  stopifnot(inherits(ignore_apps, "logical"))
  stopifnot(inherits(ignore_hidden, "logical"))
  stopifnot(inherits(min_size, "numeric"))
  stopifnot(inherits(include_icloud, "logical"))
  stopifnot(inherits(savepath, "character"))
  stopifnot(inherits(pattern_, "character")
            | inherits(pattern_, "NULL"))
  stopifnot(inherits(date_, "character")
            | inherits(date_, "NULL"))

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Set User Path ----
  if(inherits(user_, "NULL")){
    user_ <- "~"
    icloud_path <- "~/Library/Mobile Documents/com~apple~CloudDocs/"

  } else {
    user_ <- paste0("/Users/", user_)
    # modify output save path to match custom user.
    savepath <- file.path(user_, sub(pattern = "~",
                                     replacement = "",
                                     x = savepath))

    icloud_path <- paste0(
      user_,
      "/Library/Mobile Documents/com~apple~CloudDocs/"
    )

  }

  # Create File Names ----
  # Set filename date.
  if(inherits(date_, "NULL")){
    date_ <- format(Sys.time(), "%Y_%m_%d")
  }

  # Set filename for saving to disk.
  filelistpath <- file.path(
    savepath,
    paste0("kondo_filelist_",
           date_,
           ".csv")
  )

  # create temporary directory list of all completed searches.
  dirlistpath <- file.path(
    savepath,
    paste0("kondo_dirlist_",
           date_,
           ".csv")
  )

  queuepath <- file.path(
    savepath,
    paste0("kondo_queue_", date_, ".txt")
  )

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create Pool, Queue and Tally ----
  # (POOL keeps track of number of directories in QUEUE.
  # QUEUE is vector of directories to search.
  # TLLY keeps track of how many *files* have been accounted.)
  # Scenario 1: Starting from scratch.
  if(!file.exists(queuepath)){
    # Create pool of directory paths to draw from. Init with user, eg. ~.
    queue <- user_
    pool <- 1

    # set a tally for the ID number for file info.
    tlly <- 0
  } else {
    # Scenario 2: Load previous work.

    # retrieve previous queue
    queue <- readLines(con = queuepath)
    if(queue[1] == ""){
      stop("Queue is empty.")
    }
    pool <- length(queue)

    # load tally info
    if(file.exists(filelistpath)){
      prev <- read.csv(file = filelistpath)

      # grab previous tally as baseline
      tlly <- prev$tlly[nrow(prev)]
    } else {
      tlly <- 0
    }
  }  # END Pool and Tally

  # Simply to keep track of how many loops we've done.
  loop_counter <- 0

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # LOOP1 ----
  while(length(pool) > 0){

    # update loop counter
    loop_counter <- loop_counter + 1

    if(loop_counter == 1){
      message("Begin LOOP1")
    }

    if(loop_counter %% 25 == 0){
      message(paste0("Loop #", loop_counter))
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Scan Dirs/Files in Loop1 ----
    # get path
    dir_ <- queue[1]

    # get directories (non-recursively)
    dl <- list.dirs(path = dir_,
                    full.names = TRUE,
                    recursive = FALSE)

    # get file list
    fl <- list.files(path = dir_,
                     pattern = pattern_,
                     full.names = TRUE,
                     all.files = !ignore_hidden)

    # remove directories from file list.
    fl <- fl[!(fl %in% dl)]

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Remove Unwanted Dirs and Files ----
    # Library
    if(ignore_library){
      dl <- dl[!grepl(
        pattern = paste0(user_, "/Library"),
        x = dl
      )]
    }
    # Applications
    if(ignore_apps){
      dl <- dl[!grepl(
        pattern = paste0(user_, "/Applications"),
        x = dl
      )]
    }
    # Hidden
    # The pattern is a REGEX that detects a period at the beginning of
    # a directory name.
    # (Note, hidden files are already taken care of in `list.files` above.)
    if(ignore_hidden){
      dl <- dl[
        !grepl(
          pattern = "(?<=\\/)\\.",
          x = dl,
          perl = TRUE
        )
      ]
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Append Queue ----
    queue <- append(queue, dl)

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Save FileList ----
    # ie. file.info
    if(length(fl) > 0){
      finfo <- file.info(fl) %>%
        tibble::rownames_to_column(var = "path")

      # remove small files
      if(min_size > 0){
        finfo <- finfo %>%
          dplyr::filter(size >= min_size)
      }

      # add ID and select cols
      finfo <- finfo %>%
        dplyr::mutate(id = tlly + dplyr::row_number()) %>%
        dplyr::select(id, path, size, isdir, mtime, ctime, atime)

      # update tally
      tlly <- finfo$id[nrow(finfo)]

      # write to storage
      if(file.exists(filelistpath)){
        write.csv(
          x = finfo,
          file = filelistpath,
          append = TRUE,
          row.names = FALSE
        )
      } else {
        write.csv(
          x = finfo,
          file = filelistpath,
          row.names = FALSE
        )
      }
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Decrement Pool ----
    pool <- pool - 1
    if(length(pool) >= 1){

      queue <- queue[2:length(queue)]
      stopifnot(length(queue) == length(pool))

      writeLines(text = queue, con = queuepath)

    } else if(include_icloud){
      message("Begin iCloud Inventory.")
      include_icloud <- FALSE
      queue <- icloud_path
      writeLines(text = queue, con = queuepath)
      pool <- 1

    } else {
      message("Queue is empty.")
      writeLines("", con = queuepath)
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Save DirList ----

    dirlist <- data.frame(
      dir = dir_,
      tlly = tlly,
      nsubdirs = length(dl)
    )

    # write to storage
    if(file.exists(dirlistpath)){
      write.csv(
        x = dirlist,
        file = dirlistpath,
        append = TRUE,
        row.names = FALSE
      )
    } else {
      write.csv(
        x = dirlist,
        file = dirlistpath,
        row.names = FALSE
      )
    }


    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  }
  # END LOOP1 ----

  # Return
  NULL
}




#' Identify duplicates files in tibble contain columns path and size
#'
#' @description
#' If hash is present in the `md5`column, then it is duplicate!
#'
#' @param tb data.frame/tibble. The data.frame returned by `k_ls`.
#' @param rm.unique logical. Whether or not to remove unique values. By default, it is `TRUE`.
#' @param msg_rate numeric. How often do you want a system message to appear as you run the function? By default, on every 50th file, a message will appear.
#' @param write_ logical. Write csv output to disk. By default it downloads to ~/Downloads.
#'
#' @returns tibble
#' @export
k_dupes <- function(tb,
                    rm.unique = TRUE,
                    msg_rate = 50,
                    write_ = FALSE){
  stopifnot(inherits(tb, 'data.frame'))
  stopifnot(inherits(msg_rate, 'numeric'))
  stopifnot(inherits(write_, "logical"))
  stopifnot("path" %in% names(tb))
  stopifnot("size" %in% names(tb))

  # add column that recognizes duplicate filenames
  tb <- tb %>%
    dplyr::mutate(filename = stringr::str_extract(path, "[^/]+$")) %>%
    dplyr::mutate(name_dupe = duplicated(filename) |
                    duplicated(filename,
                               fromLast = TRUE))

  # Group by size first
  dups <- tb %>%
    dplyr::select(path, size) %>%
    dplyr::filter(!is.na(size)) %>%
    dplyr::group_by(size) %>%
    dplyr::filter(dplyr::n() > 1) %>%
    dplyr::ungroup()

  # Calculate hashes only for potential duplicates
  dups <- dups %>%
    dplyr::mutate(
      md5 = purrr::imap_chr(
      path,
      function(x, idx){
        # check for messages.
        if(!(msg_rate == 0 | is.na(msg_rate))){
          if(idx %% msg_rate == 0){
            message(paste0(
              "k_dupes progress: ",
              idx,
              " of ",
              nrow(dups),
              "."
            ))

          }
        }

        # return
        digest::digest(x,
                       algo = "md5",
                       file = TRUE)
      })) %>%
    dplyr::group_by(md5) %>%
    dplyr::filter(n() > 1) %>%
    mutate(group_id = dplyr::cur_group_id()) %>%
    dplyr::ungroup() %>%
    dplyr::select(-size)

  # add md5 column in to original dataset.
  tb <- tb %>%
    # add dupes b
    dplyr::left_join(y = dups, by = "path")

  if(rm.unique){
    # create filter criteria of duplicated values.
    filtr_ <- tb$md5[duplicated(tb$md5)] |>
      unique()

    # remove NA
    filtr_ <- filtr_[!is.na(filtr_)]

    # execute filter
    tb <- tb[tb$md5 %in% filtr_,]
  }

  if(write_){
    nm <- paste0("~/Downloads/kondo_kdupes_",
                 format(Sys.time(), "%Y_%m_%d_%H:%M"),
                 ".csv")
    write.csv(
      x = tb,
      file = nm
    )
  }

  # return
  tb
}
