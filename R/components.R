

#' Single directory inventory maker
#'
#' @description
#' Takes a path to a directory and inventories the contents. This function looks for directories and files. It returns a list with two elements, a character vector of directories, and a data.frame containing some basic information about files including the path, size, creation time, etc. Importantly, this function is non-recursive.
#'
#' @param dir_ character. Normalized path to primary directory.
#' @param ignore_ character. Paths to directories to ignore.
#' @param pattern_ character. Usually file extensions to focus upon, see \link[base]{list.files}.
#' @param min_size numeric. The minimum file size (in bytes) to inventory.
#' @param include_hid logical. Whether or not to include directories and files that are hidden, ie. that begin with a period.
#'
#' @returns list
#' @export
k_inventory <- function(
    dir_,
    ignore_ = NULL,
    pattern_ = NULL,
    min_size = 0,
    include_hid = FALSE
){
  stopifnot(inherits(dir_, "character"))
  stopifnot(inherits(ignore_, "character") |
              inherits(ignore_, "NULL"))
  stopifnot(inherits(pattern_, "character") |
              inherits(pattern_, "NULL"))
  stopifnot(inherits(min_size, "numeric"))
  stopifnot(inherits(include_hid, "logical"))

  # Return NULL if this directory should be ignored
  if(dir_ %in% ignore_){
    return(NULL)
  }

  # Return NULL if this directory does not exist.
  if(!dir.exists(dir_)){
    message(paste("Directory", dir_, "does not exist."))
    return(NULL)
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Get directories and files ----
  # get directories (non-recursively)
  dl <- list.dirs(path = dir_,
                  full.names = TRUE,
                  recursive = FALSE)

  # get file list
  fl <- list.files(path = dir_,
                   pattern = pattern_,
                   full.names = TRUE,
                   all.files = include_hid)

  # remove directories from file list.
  fl <- fl[!(fl %in% dl)]

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Remove Unwanted Dirs and Files ----
  # Ignore List of Directories
  dl <- dl[!(dl %in% ignore_)]

  # Hidden
  # The pattern is a REGEX that detects a period at the beginning of
  # a directory name.
  # (Note, hidden files are already taken care of in `list.files` above.)
  if(!include_hid){
    dl <- dl[
      !grepl(
        pattern = "(?<=\\/)\\.",
        x = dl,
        perl = TRUE
      )
    ]
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Get file.info
  if(length(fl) > 0){
    finfo <- file.info(fl) %>%
      tibble::rownames_to_column(var = "path") %>%
      dplyr::select(path, size, isdir, mtime, ctime, atime)

    # remove small files
    if(min_size > 0){
      finfo <- finfo %>%
        dplyr::filter(size >= min_size)
    }
  } else {
    finfo <- NULL
  }

  # return
  list(
    dl = dl,
    finfo = finfo
  )
}

#' Queue file manager.
#'
#' @description
#' Manages a txt file that contains a list of file paths. It can do three things in any combination: 1) Read the first line and return it (which it does by default), 2) Append paths to the bottom of the txt, and 3) remove the first line of the text file. This function does not read the whole file. It will create an empty txt document if none exists.
#'
#' @param path_ character. A single file path to the txt file. Make sure this is consistent each time the file loads.
#' @param add_ character. One or more paths in a character vector. Set to NULL when nothing to add.
#' @param rm_  logical. Determines whether to delete the top line or not.
#' @param read_ logical. Read the first line of the file?
#'
#' @returns character.
intr_queue <- function(
    path_,
    add_ = NULL,
    rm_ = FALSE,
    read_ = FALSE
){
  stopifnot(inherits(path_, "character"))
  stopifnot(inherits(add_, "character") | inherits(add_, "NULL"))
  stopifnot(inherits(rm_, "logical"))
  stopifnot(inherits(read_, "logical"))

  if (!file.exists(path_)) {
    file.create(path_)
  }

  # Append new contents to queue file on disk
  # add new lines to bottom
  if(!inherits(add_, "NULL")){
    write(
      x =  add_,
      file = path_,
      append = TRUE,
      sep = "\n"
    )
  }

  # Remove first item from queue txt file.
  if(rm_ == TRUE){
    # Shell scripting (unix only)
    # Use 'tail' to skip the first line and overwrite the file
    # Safely handle paths with spaces using shQuote
    tmp_path <- paste0(path_, ".tmp")
    cmd <- paste("tail -n +2",
                 shQuote(path_), ">",
                 shQuote(tmp_path), "&& mv",
                 shQuote(tmp_path),
                 shQuote(path_))
    system(cmd)
  }

  # read and return
  if(read_){
    readLines(con = path_, n = 1)
  } else {
    NULL
  }
}

#' Write or append to csv
#'
#' @param path_ character. Path to desired csv.
#' @param df data.frame. Data to write to disk.
#'
#' @returns NULL
intr_csv_writer <- function(path_, df){
  stopifnot(inherits(path_, "character"))
  stopifnot(inherits(df, "data.frame"))

  # For new file
  if(!file.exists(path_)) {
    readr::write_csv(df, path_)
  } else {
    # For appending
    readr::write_csv(df, path_, append = TRUE)
  }

  # return
  NULL
}

#' Create file system information from directory.
#'
#' @description
#' Write file system information for all files under one or more directories. Because this task is potentially memory intensive, progress is written to disk as a csv file. As such, this function returns only the path to the output csv.
#'
#'
#' @param dir1 character. Starting directory. NULL defaults to ~. Must be length 1.
#' @param dirs2 character. Secondary directories that should be included. Set to NULL if unwanted. A common secondary directory to add is iCloud when ~/Library is ignored. Use this string "~/Library/Mobile Documents/com~apple~CloudDocs/" to include iCloud.
#' @param ignore_dirs character. Directories to ignore. By default, ignores user Library and Application
#' @param include_hid logical. Include hidden files and directories (which on macOS begin with period).
#' @param min_size numeric. Minimum file size to include (in bytes).
#' @param suffix_ character. File name suffix of output files. By default the suffix is the date. This is a useful argument if the algorithm stops part way and you need to restart it using saved data.
#' @param savepath character. Path to store output.
#' @param pattern_ character. Pattern for list.files which is used to locate specific file types.
#'
#' @returns character
#' @export
k_ls <- function(
  dir1 = "~",
  dirs2 = NULL,
  ignore_dirs = c("~/Library", "~/Applications"),
  include_hid = FALSE,
  min_size = 1000,
  suffix_ = NULL,
  savepath = "~/Downloads",
  pattern_ = NULL
){
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Checks ----
  stopifnot(inherits(dir1, "character"))
  stopifnot(inherits(dirs2, "NULL") | inherits(dirs2, "character"))
  stopifnot(inherits(ignore_dirs, "NULL") | inherits(ignore_dirs, "character"))
  stopifnot(inherits(include_hid, "logical"))
  stopifnot(inherits(min_size, "numeric"))
  stopifnot(inherits(suffix_, "NULL") | inherits(suffix_, "character"))
  stopifnot(inherits(savepath, "character"))
  stopifnot(inherits(pattern_, "NULL") | inherits(pattern_, "character"))

  # obtain system information, such as user and OS.
  sys1 <- Sys.info()
  sys1 <- data.frame(key = names(sys1),
                     value = sys1,
                     row.names = NULL)

  # check to ensure this is macOS
  stopifnot(sys1$value[sys1$key=="sysname"] == "Darwin")

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Paths ----
  # In this section we normalize paths and create output paths.

  # organize paths as a list.
  p <- list(
    # primary root directory to begin in.
    dir1 = dir1,
    # path to output directory.
    sav = savepath,
    # unique identifier for output files...
    # eg. kondo_queue_SUFFIX.txt
    suffix = ifelse(inherits(suffix_, "NULL"),
                    format(Sys.time(), "%Y_%m_%d"),
                    suffix_)
  )

  # If we supply other directories to inventory...
  if(!inherits(dirs2, "NULL")){
    p <- append(p, list(dirs2 = dirs2))
  }

  # If we supply directories to skip...
  if(!inherits(ignore_dirs, "NULL")){
    p <- append(p, list(igno = ignore_dirs))
  }

  # Normalize ~ paths to absolute paths.
  # note, mustWork is FALSE because some users don't have ~/Applications
  p <- lapply(p, function(.x) normalizePath(path = .x, mustWork = F))

  # Add output file paths.
  p <- append(
    p,
    list(
      queue = file.path(p$sav,
                        paste0("kondo_queue_", p$suffix, ".txt")
      ),
      finfo = file.path(p$sav,
                        paste0("kondo_fileinfo_", p$suffix, ".csv"))
    )
  )

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Does the queue already exist?
  # The use case of this is if this algorithm crashes,
  # you can pick up where you left off.
  if(!file.exists(p$queue)){
    # Fresh start
    queue <- c(p$dir1, p$dirs2)
    invisible(kondo:::intr_queue(
      path_ = p$queue,
      add_ = c(p$dir1, p$dirs2),
      rm_ = FALSE,
      read_ = FALSE
    ))
  } else {
    # Restart
    queue <- readLines(
      con = p$queue
    )
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Loop ----
  message("kondo::kls loop begins...")
  while(length(queue) > 0){

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Derive inventory.
    inv <- k_inventory(
      dir_ = queue[1],
      ignore_ = p$igno,
      pattern_ = pattern_,
      min_size = min_size,
      include_hid = include_hid
    )

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Write inventory contents to disk.
    if(!inherits(inv, "NULL")){

      # Write new subdirectories to queue file.
      if(!identical(inv$dl, character(0))){
        # Add new subdirectories to queue file on disk.
        invisible(kondo:::intr_queue(
          path_ = p$queue,
          add_ = inv$dl,
          rm_ = FALSE,
          read_ = FALSE
        ))

        # As a means of indicating a sense of progress,
        # whenever more than 10 new directories are inventoried,
        # a message will appear with some information.
        if(length(inv$dl) > 10){
          message(paste0(
            "kondo::kls progress update, directory ",
            sub(
              pattern = p$dir1,
              replacement = "",
              x = queue[1]
              ),
            " has ",
            length(inv$dl),
            " sub-folders..."
          ))
        }

        # Add subdirectories into the queue in RAM/environment.
        queue <- c(queue, inv$dl)
      }

      # file info
      if(!inherits(inv$finfo, "NULL")){
        invisible(kondo:::intr_csv_writer(path_ = p$finfo,
                                  df = inv$finfo))
      }

    }  # End writing inventory to disk

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Remove completed dir from the top of queue file.
    invisible(kondo:::intr_queue(
      path_ = p$queue,
      add_ = NULL,
      rm_ = TRUE,
      read_ = FALSE
    ))

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Remove completed dir from the top of the queue in RAM/environment.
    if(length(queue) > 1){
      queue <- queue[2:length(queue)]
    } else {
      queue <- c()
    }

  }  # END loop

  message("kondo::kls completed.")
  # return
  p$finfo
}

#' Identify duplicates files in tibble contain columns path and size
#'
#' @description
#' If hash is present in the `md5`column, then it is duplicate!
#'
#' @param tb data.frame or character. Either a data.frame from the file created by `k_ls` or a path to that csv.
#' @param rm.unique logical. Whether or not to remove unique values. By default, it is `TRUE`.
#' @param msg_rate numeric. How often do you want a system message to appear as you run the function? By default, on every 50th file, a message will appear.
#' @param savpath character. Write csv output to disk. By default it downloads to ~/Downloads.
#'
#' @returns tibble
#' @export
k_dupes <- function(tb,
                    rm.unique = TRUE,
                    msg_rate = 50,
                    savpath = "~/Downloads"){
  stopifnot(inherits(tb, 'data.frame') | inherits(tb, "character"))
  stopifnot(inherits(msg_rate, 'numeric'))
  stopifnot(inherits(savpath, "character"))

  # load data if needed
  if(inherits(tb, "character")){
    # If tb is just a path.
    tbp <- normalizePath(tb)
    tb <- read.csv(file = tbp)
  }

  stopifnot("path" %in% names(tb))
  stopifnot("size" %in% names(tb))

  # normalize savepath
  savpath <- normalizePath(savpath, mustWork = F)

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
              "kondo::k_dupes progress: ",
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

  # Arrange tb
  tb <- tb %>%
    dplyr::arrange(dplyr::desc(size), md5)

  if(!inherits(savpath, "NULL")){

    out_path <- file.path(
      savpath,
      paste0("kondo_kdupes_",
             format(Sys.time(), "%Y_%m_%d"),
             ".csv")
    )

    readr::write_csv(
      x = tb,
      file = out_path
    )
  }

  # return
  tb
}
