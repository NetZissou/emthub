where <- tidyselect:::where

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL


app_dev <- function(run_check = FALSE) {

  if (run_check) {
    devtools::check()
    app()
  } else {
    devtools::load_all()
    app()
  }
}

#' @importFrom rlang ".data"
check_na <- function(tbl, missing_only = TRUE) {
  missing_result <-
    tbl %>%
    dplyr::summarise_all(
      .funs = list(
        ~mean(is.na(.))
      )
    ) %>%
    tidyr::pivot_longer(
      dplyr::everything(),
      names_to = "var",
      values_to = "missing_prop"
    )
  if (missing_only) {
    missing_result <- missing_result %>%
      dplyr::filter(.data$missing_prop > 0)
  }
  return(missing_result %>% dplyr::arrange(-.data$missing_prop))
}

check_na_io <- function(tbl, missing_only = TRUE) {
  missing_info <- check_na(tbl, missing_only = missing_only)

  message("Data missing info: ")
  if (nrow(missing_info) == 0) {
    cat("No missing values detected\n")
  } else {
    print(missing_info)
  }
}

nothing_selected <- function(x) {
  return(
    length(is.na(x)) == 0
  )
}

#' Load Map Shapefile Function
#' @param file sf file path
#' @param crs crs code default to be 4326
load_map_shapefile <- function(file, crs = 4326) {

  if (crs != 4326) {
    stop("'crs' must be 4326 at this moment.")
  }

  map_data <- sf::read_sf(file, quiet = TRUE)
  map_data <- sf::st_transform(map_data, crs = 4326)

  return(map_data)
}



#' Check the updated time of files in a specified folder
#'
#' @param dir file directory
#' @param regexp 	A regular expression to filter paths
#'
#' @return return the a tibble with the first column `path` contains all of the
#' matched files path and second column `modification_time` contains the modification time of the file
#' @export
#' @importFrom rlang ".data"
#' @importFrom utils "head"
check_files_update_time <- function(
  dir,
  regexp
) {
  fs::dir_info(
    dir,
    regexp = regexp
  ) %>%
    dplyr::select(
      .data$path, .data$modification_time
    ) %>%
    dplyr::arrange(dplyr::desc(.data$modification_time))
}




cut_time_label <- function(time, type = c("car", "walk", "transit")) {

  if (type == "car") {

    unit_func <-
      function(time) {
        if (time < 15) {
          return("<15 mins")
        } else if (time >= 15 & time < 30) {
          return("15-29 mins")
        } else {
          return(">30 mins")
        }
      }

  } else if (type == "transit") {

    unit_func <-
      function(time) {
        if (time < 30) {
          return("<30 mins")
        } else if (time >= 30 & time < 45) {
          return("30-44 mins")
        } else if (time >= 45 & time < 89) {
          return("45-89 mins")
        } else {
          ">90 mins"
        }
      }

  } else if (type == "walk") {

    unit_func <-
      function(time) {
        if (time < 4) {
          return("<5 mins")
        } else if (time >= 5 & time < 10) {
          return("5-9 mins")
        } else if (time >= 10 & time < 20) {
          return("10-19 mins")
        } else if (time >= 20 & time < 30) {
          "20-29 mins"
        } else {
          ">30 mins"
        }
      }
  }


  purrr::map_chr(time, purrr::safely(unit_func, otherwise = NA))
}



